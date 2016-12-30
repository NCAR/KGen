# gen_write_typedecl_in_parentblock.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import STATE_PBLOCK_WRITE_IN_ARGS, STATE_PBLOCK_WRITE_IN_LOCALS, STATE_PBLOCK_WRITE_OUT_LOCALS, \
    DRIVER_READ_IN_ARGS, KERNEL_PBLOCK_READ_IN_LOCALS, KERNEL_PBLOCK_READ_OUT_LOCALS, \
    DRIVER_DECL_PART, DRIVER_USE_PART, get_typedecl_writename, get_dtype_writename, state_gencore_contains, \
    get_topname, get_typedecl_readname, get_dtype_readname, shared_objects, process_spec_stmts, is_zero_array, \
    is_excluded, is_remove_state, namedgen_read_istrue, namedgen_write_istrue, check_class_derived 
from gencore_subr import create_write_subr, create_read_subr

class Gen_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_created_subrs = []
        self.kernel_created_subrs = []
        self.driver_created_subrs = []
        self.driver_created_uses = []

    def check_intent(self, entity_name, stmt):
        if any( attr.startswith('intent') for attr in stmt.attrspec ) or \
            hasattr(stmt.parent, 'args') and entity_name in stmt.parent.args:
            return True
        return False

    # registration
    def register(self, msg):
        self.frame_msg = msg

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.create_subr_write_typedecl_in_parentblock) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.create_subr_read_typedecl_in_parentblock) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), None, self.process_specstmts_in_upperblocks) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), self.has_implicit_rule_resolver, self.create_read_implicit_rule_in_parentblock) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), self.has_implicit_rule_resolver, self.create_write_implicit_rule_in_parentblock) 

        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
        #    typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.remove_read_typedecl_in_parentblock) 

    def has_implicit_rule_resolver(self, node):
        if hasattr(node.kgen_stmt, 'implicit_rule_resolvers'):
            return True
        else: return False

    def create_read_implicit_rule_in_parentblock(self, node):
        for resolver in node.kgen_stmt.implicit_rule_resolvers:
            if resolver.name in node.kgen_stmt.args:
                partid = DRIVER_READ_IN_ARGS
            else:
                partid = KERNEL_PBLOCK_READ_IN_LOCALS

            if KGGenType.has_state_out(resolver.geninfo):
                attrs = {'items': [resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_genknode(node.kgen_kernel_id, partid, statements.Read, attrs=attrs)

                attrs = {'items': ['kgenref_'+resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, statements.Read, attrs=attrs)
            else:
                attrs = {'items': [resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_genknode(node.kgen_kernel_id, partid, statements.Read, attrs=attrs)
 
    def create_write_implicit_rule_in_parentblock(self, node):
        kgenunit = 'UNIT = kgen_unit'

        for resolver in node.kgen_stmt.implicit_rule_resolvers:
            if resolver.name in node.kgen_stmt.args:
                partid = STATE_PBLOCK_WRITE_IN_ARGS
            else:
                partid = STATE_PBLOCK_WRITE_IN_LOCALS

            if KGGenType.has_state_out(resolver.geninfo):
                attrs = {'items': [resolver.name], 'specs': [kgenunit]}
                namedpart_append_gensnode(node.kgen_kernel_id, partid, statements.Write, attrs=attrs)

                attrs = {'items': [resolver.name], 'specs': [kgenunit]}
                namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, statements.Write, attrs=attrs)
            else:
                attrs = {'items': [resolver.name], 'specs': [kgenunit]}
                namedpart_append_gensnode(node.kgen_kernel_id, partid, statements.Write, attrs=attrs)
           
    def process_specstmts_in_upperblocks(self, node):
        process_spec_stmts(node.kgen_stmt)

        #if node.kgen_parent is None: import pdb; pdb.set_trace()
        if hasattr(node, 'kgen_parent') and node.kgen_parent and \
            not node.kgen_parent.kgen_stmt.__class__ in [ block_statements.Module, block_statements.Program ]:
            self.process_specstmts_in_upperblocks(node.kgen_parent)

    def typedecl_has_state_parentblock(self, node):
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state(node.kgen_stmt.geninfo) \
            and "parameter" not in node.kgen_stmt.attrspec and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            for entity_name in [ get_entity_name(decl) for decl in node.kgen_stmt.entity_decls ]:
                var = node.kgen_stmt.get_variable(entity_name)
                if not var.is_parameter():
                    return True
        return False

    def remove_read_typedecl_in_parentblock(self, node):
        node.kgen_isvalid= False

    def create_subr_read_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt

        argintype = []
        localintype = []
        localouttype = []
        for uname, req in KGGenType.get_state_in(stmt.geninfo):
            entity_name = uname.firstpartname()
            var = stmt.get_variable(entity_name)

            if var.is_parameter(): continue
            if is_remove_state(entity_name, stmt): continue

            if self.check_intent(entity_name, stmt):
                if (entity_name,DRIVER_READ_IN_ARGS) not in argintype:

                    argintype.append((entity_name, DRIVER_READ_IN_ARGS))

                    if not entity_name in getinfo('kernel_driver_callsite_args'):
                        getinfo('kernel_driver_callsite_args').append(entity_name)

                    # add typedecl in driver
                    attrs={'type_spec':stmt.__class__.__name__.upper(), 'selector':stmt.selector, 'entity_decls': [entity_name]}
                    attrspec = []
                    if var.is_array():
                        attrspec.append('DIMENSION(%s)'%','.join(':'*var.rank))
                        if not var.is_pointer(): attrspec.append('ALLOCATABLE')
                        # deallocate
                    if var.is_pointer(): attrspec.append('POINTER')
                    attrs['attrspec'] = attrspec 
                    namedpart_append_genknode(node.kgen_kernel_id, DRIVER_DECL_PART, stmt.__class__, attrs=attrs)

                    if hasattr(stmt, 'unknowns'):
                        for uname, req in stmt.unknowns.iteritems():
                            if len(req.res_stmts)>0:
                                if req.res_stmts[-1].__class__==statements.Use:
                                    checks = lambda n: n.kgen_match_class==statements.Use and n.kgen_stmt and n.kgen_stmt.name==req.res_stmts[-1].name \
                                        and ( n.kgen_stmt.isonly and uname.firstpartname() in [ item.split('=>')[0].strip() for item in n.kgen_stmt.items])
                                    if not namedpart_has_node(node.kgen_kernel_id, DRIVER_USE_PART, checks):
                                        item_name = uname.firstpartname()
                                        for new_name, old_name in req.res_stmts[-1].renames:
                                            if new_name==item_name:
                                                item_name = '%s => %s'%(new_name, old_name) 
                                                break
                                        if not (req.res_stmts[-1].name, item_name) in self.driver_created_uses:
                                            attrs = {'name':req.res_stmts[-1].name, 'isonly': True, 'items':[item_name]}
                                            namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
                                            self.driver_created_uses.append((req.res_stmts[-1].name, item_name))

                                        if stmt.is_derived() and stmt.name==uname.firstpartname():
                                            readname = get_dtype_readname(req.res_stmts[0])
                                            if not (req.res_stmts[-1].name, readname) in self.driver_created_uses:
                                                attrs = {'name':req.res_stmts[-1].name, 'isonly': True, 'items':[readname]}
                                                namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
                                                self.driver_created_uses.append((req.res_stmts[-1].name, readname))

                                else:
                                    if req.res_stmts[0].genkpair.kgen_parent!=node.kgen_parent:
                                        checks = lambda n: n.kgen_match_class==statements.Use and n.kgen_stmt and n.kgen_stmt.name==get_topname(req.res_stmts[-1]) and \
                                            ( n.kgen_stmt.isonly and uname.firstpartname() in [ item.split('=>')[0].strip() for item in n.kgen_stmt.items])
                                        if not namedpart_has_node(node.kgen_kernel_id, DRIVER_USE_PART, checks):
                                            item_name = uname.firstpartname()
                                            for new_name, old_name in req.res_stmts[-1].renames:
                                                if new_name==item_name:
                                                    item_name = '%s => %s'%(new_name, old_name) 
                                                    break
                                            if not (get_topname(req.res_stmts[-1]), item_name) in self.driver_created_uses:
                                                attrs = {'name':get_topname(req.res_stmts[-1]), 'isonly': True, 'items':[item_name]}
                                                namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
                                                self.driver_created_uses.append((get_topname(req.res_stmts[-1]), item_name))

                                            if stmt.is_derived() and stmt.name==uname.firstpartname():
                                                readname = get_dtype_readname(req.res_stmts[-1])
                                                if not (get_topname(req.res_stmts[-1]), readname) in self.driver_created_uses:
                                                    attrs = {'name':get_topname(req.res_stmts[-1]), 'isonly': True, 'items':[readname]}
                                                    namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
                                                    self.driver_created_uses.append((get_topname(req.res_stmts[-1]), readname))
            elif (entity_name,KERNEL_PBLOCK_READ_IN_LOCALS) not in localintype and (entity_name,DRIVER_READ_IN_ARGS) not in argintype:
                localintype.append((uname.firstpartname(), KERNEL_PBLOCK_READ_IN_LOCALS))
        for uname, req in KGGenType.get_state_out(stmt.geninfo):
            entity_name = uname.firstpartname()
            var = stmt.get_variable(entity_name)

            if var.is_parameter(): continue
            if is_remove_state(entity_name, stmt): continue

            if (entity_name,KERNEL_PBLOCK_READ_OUT_LOCALS) not in localouttype:
                localouttype.append((uname.firstpartname(), KERNEL_PBLOCK_READ_OUT_LOCALS))

            if (entity_name,DRIVER_READ_IN_ARGS) in argintype: continue

            if (entity_name,KERNEL_PBLOCK_READ_IN_LOCALS) not in localintype:
                localintype.append((uname.firstpartname(), KERNEL_PBLOCK_READ_IN_LOCALS))
        localvartypes = { 'localintype': localintype, 'localouttype': localouttype }

        def get_attrs(attrspec, allowed_attrs):
            attrspec = []
            for attr in stmt.attrspec:
                if any( attr.startswith(allowed_attr) for allowed_attr in allowed_attrs):
                    attrspec.append(attr)
            return attrspec

        def get_decls(names, decls, prefix=''):
            import re
            entity_decls = []
            for decl in decls:
                ename = re.split('\(|\*|=', decl)[0].strip()
                if ename in names:
                    entity_decls.append(prefix+decl)
            return entity_decls

        def get_enames(names, decls, prefix=''):
            import re
            entity_enames = []
            for decl in decls:
                ename = re.split('\(|\*|=', decl)[0].strip()
                if ename in names:
                    entity_enames.append(prefix+ename)
            return entity_enames

        if len(argintype)>0:
            attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable', 'dimension', 'target'])
            attrspec.append('INTENT(INOUT)')

            argin_names = [ argin_name for argin_name, pname in argintype]
            entity_decls = get_decls(argin_names, stmt.entity_decls)

            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, \
                'selector':stmt.selector, 'entity_decls': entity_decls}
            if stmt.is_derived():
                node.type_spec = 'TYPE'
            else:
                node.type_spec = stmt.__class__.__name__.upper()
            node.attrspec = attrspec
            node.selector = stmt.selector
            node.entity_decls = entity_decls
            node.kgen_use_tokgen = True
            #part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)

        if len(localintype)==0 and len(argintype)==0 and len(localouttype)==0:
            node.kgen_forced_line = False
        elif len(localintype)>0:
            attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable', 'dimension', 'target'])

            localin_names = [ localin_name for localin_name, pname in localintype]
            entity_decls = get_decls(localin_names, stmt.entity_decls)

            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, \
                'selector':stmt.selector, 'entity_decls': entity_decls}
            if stmt.is_derived():
                node.type_spec = 'TYPE'
            else:
                node.type_spec = stmt.__class__.__name__.upper()
            node.attrspec = attrspec
            node.selector = stmt.selector
            node.entity_decls = entity_decls
            node.kgen_use_tokgen = True
            #part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)

        local_allocate = False
        if len(localouttype)>0:
            for localout_name, partid in localouttype:
                attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable'])
                var = stmt.get_variable(localout_name)
                if var.is_array():
                    if var.is_explicit_shape_array():
                        attrspec.append('dimension(%s)'%','.join(var.shape))
                    else:
                        attrspec.append('dimension(%s)'%','.join([':']*var.rank))
                        if  'allocatable' not in attrspec and 'pointer' not in attrspec:
                            local_allocate = True
                            attrspec.append('allocatable')

                #localout_names = [ localout_name for localout_name, pname in localouttype]
                #entity_decls = get_decls(localout_names, stmt.entity_decls, prefix='kgenref_')
                #entity_enames = get_enames(localout_names, stmt.entity_decls, prefix='kgenref_')

                attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, \
                    'selector':stmt.selector, 'entity_decls': [ 'kgenref_%s'%localout_name ]}
                    #'selector':stmt.selector, 'entity_decls': entity_decls}
                part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)

        # for kernel - local variables
        is_class_derived = check_class_derived(stmt)
        for vartypename, vartype in localvartypes.iteritems():
            for entity_name, partid in vartype:
                if vartypename=='localouttype': ename_prefix = 'kgenref_'
                else: ename_prefix = ''
                var = stmt.get_variable(entity_name)
                subrname = get_typedecl_readname(stmt, entity_name)
                if var.is_array():
                    if is_zero_array(var, stmt): continue
                    if stmt.is_derived() or is_class_derived:
                        self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt, ename_prefix=ename_prefix, allocate=local_allocate)
                            self.kernel_created_subrs.append(subrname)
                    else: # intrinsic type
                        if var.is_explicit_shape_array():
                            if var.is_pointer():
                                self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                                if subrname not in self.kernel_created_subrs:
                                    create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt, ename_prefix=ename_prefix, allocate=local_allocate)
                                    self.kernel_created_subrs.append(subrname)
                            else:
                                self.create_read_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var, ename_prefix=ename_prefix)
                        else: # implicit array
                            self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                            if subrname not in self.kernel_created_subrs:
                                create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt, ename_prefix=ename_prefix, allocate=local_allocate)
                                self.kernel_created_subrs.append(subrname)
                else: # scalar
                    if stmt.is_derived() or is_class_derived or var.is_pointer():
                        if var.is_allocatable() or var.is_pointer():
                            self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                            if subrname not in self.kernel_created_subrs:
                                create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt, ename_prefix=ename_prefix, allocate=local_allocate)
                                self.kernel_created_subrs.append(subrname)
                        else:
                            subrname = None
                            for uname, req in stmt.unknowns.iteritems():
                                if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                    res = req.res_stmts[0]
                                    subrname = get_dtype_readname(res)
                                    break
                            if subrname is None:
                                print 'WARNING: Can not find Type resolver for %s'%stmt.name
                                namedpart_append_comment(node.kgen_kernel_id, partid, \
                                    'ERROR: "%s" is not resolved. Call statements to read "%s" is not created here.'%\
                                    (stmt.name, stmt.name))
                            else:
                                self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                    else: # intrinsic type
                        self.create_read_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var, ename_prefix=ename_prefix)

        # for kernel - argument variables
        for entity_name, partid in argintype:
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_readname(stmt, entity_name)
            if var.is_array():
                self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                if subrname not in self.driver_created_subrs:
                    create_read_subr(subrname, entity_name, shared_objects['driver_object'], var, stmt, allocate=True)
                    self.driver_created_subrs.append(subrname)
            else: # scalar
                if stmt.is_derived() or is_class_derived or var.is_pointer():
                    if var.is_allocatable() or var.is_pointer():
                        self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, ename_prefix=ename_prefix)
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt, ename_prefix=ename_prefix)
                            self.kernel_created_subrs.append(subrname)
                    else:
                        subrname = None
                        for uname, req in stmt.unknowns.iteritems():
                            if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                res = req.res_stmts[0]
                                subrname = get_dtype_readname(res)
                                break
                        if subrname is None:
                            print 'WARNING: Can not find Type resolver for %s'%stmt.name
                            namedpart_append_comment(node.kgen_kernel_id, partid, \
                                'ERROR: "%s" is not resolved. Call statements to read "%s" is not created here.'%\
                                (stmt.name, stmt.name))
                        else:
                            self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                else: # intrinsic type
                    self.create_read_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)

    def create_subr_write_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt

        argintype = []
        localintype = []
        localouttype = []
        for uname, req in KGGenType.get_state_in(stmt.geninfo):
            entity_name = uname.firstpartname()
            var = stmt.get_variable(entity_name)

            if var.is_parameter(): continue
            if is_remove_state(entity_name, stmt): continue

            if self.check_intent(entity_name, stmt):
                if (entity_name,STATE_PBLOCK_WRITE_IN_ARGS) not in argintype:
                    argintype.append((entity_name, STATE_PBLOCK_WRITE_IN_ARGS))
            elif (entity_name,STATE_PBLOCK_WRITE_IN_LOCALS) not in localintype and (entity_name,STATE_PBLOCK_WRITE_IN_ARGS) not in argintype:
                localintype.append((uname.firstpartname(), STATE_PBLOCK_WRITE_IN_LOCALS))

        for uname, req in KGGenType.get_state_out(stmt.geninfo):
            entity_name = uname.firstpartname()
            var = stmt.get_variable(entity_name)

            if var.is_parameter(): continue
            if is_remove_state(entity_name, stmt): continue

            if (entity_name,STATE_PBLOCK_WRITE_OUT_LOCALS) not in localouttype:
                localouttype.append((uname.firstpartname(), STATE_PBLOCK_WRITE_OUT_LOCALS))

            if (entity_name,STATE_PBLOCK_WRITE_IN_ARGS) in argintype: continue

            if (entity_name,STATE_PBLOCK_WRITE_IN_LOCALS) not in localintype:
                localintype.append((uname.firstpartname(), STATE_PBLOCK_WRITE_IN_LOCALS))
        vartypes = { 'argintype': argintype, 'localintype': localintype, 'localouttype': localouttype }

        # for state
        is_class_derived = check_class_derived(stmt)
        for vartypename, vartype in vartypes.iteritems():
            for entity_name, partid in vartype:
                var = stmt.get_variable(entity_name)
                subrname = get_typedecl_writename(stmt, entity_name)
                if var.is_array():
                    if is_zero_array(var, stmt): continue
                    if stmt.is_derived() or is_class_derived:
                        self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                        if subrname not in self.state_created_subrs:
                            create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                            self.state_created_subrs.append(subrname)
                    else: # intrinsic type
                        if var.is_explicit_shape_array():
                            if vartypename=='argintype' or var.is_pointer():
                                self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                                if subrname not in self.state_created_subrs:
                                    create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                    self.state_created_subrs.append(subrname)
                            else:
                                self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)
                        else: # implicit array
                            self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                            if subrname not in self.state_created_subrs:
                                create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                self.state_created_subrs.append(subrname)
                else: # scalar
                    if stmt.is_derived() or is_class_derived or var.is_pointer():
                        if var.is_allocatable() or var.is_pointer() or var.is_pointer():
                            self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                            if subrname not in self.state_created_subrs:
                                create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                self.state_created_subrs.append(subrname)
                        else:
                            subrname = None
                            for uname, req in stmt.unknowns.iteritems():
                                if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                    res = req.res_stmts[0]
                                    subrname = get_dtype_writename(res)
                                    break
                            if subrname is None:
                                print 'WARNING: Can not find Type resolver for %s'%stmt.name
                                namedpart_append_comment(node.kgen_kernel_id, partid, \
                                    'ERROR: "%s" is not resolved. Call statements to write "%s" is not created here.'%\
                                    (stmt.name, stmt.name))
                            else:
                                self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                    else: # intrinsic type
                        self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)

    def create_read_intrinsic(self, kernel_id, partid, entity_name, stmt, var, ename_prefix=''):

        pobj = namedgen_read_istrue(kernel_id, partid, var, entity_name, ename_prefix=ename_prefix)

#        pobj = None
#        if (var.is_array() and not var.is_explicit_shape_array()) or var.is_allocatable() or var.is_pointer():
#            attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
#            namedpart_append_genknode(kernel_id, partid, statements.Read, attrs=attrs)
#
#            attrs = {'expr': 'kgen_istrue'}
#            iftrueobj = namedpart_append_genknode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            pobj = iftrueobj
#
#        if var.is_allocatable():
#            attrs = {'expr': 'ALLOCATED( %s )'%(ename_prefix+entity_name)}
#            ifalloc = namedpart_append_genknode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'items': ['%s'%(ename_prefix+entity_name)]}
#            part_append_genknode(ifalloc, EXEC_PART, statements.Deallocate, attrs=attrs)
#
#        if var.is_pointer():
#            attrs = {'expr': 'ASSOCIATED( %s )'%(ename_prefix+entity_name)}
#            ifalloc = namedpart_append_genknode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'items': ['%s'%(ename_prefix+entity_name)]}
#            part_append_genknode(ifalloc, EXEC_PART, statements.Nullify, attrs=attrs)

        if pobj:
            attrs = {'items': [ename_prefix+entity_name], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

            if var.is_array() and stmt.is_numeric():
                if isinstance(stmt, typedecl_statements.Real):
                    attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(ename_prefix+entity_name), \
                        'kgen_array_sum', 'REAL(SUM(%s, mask=(%s .eq. %s)), 8)'%(ename_prefix+entity_name, ename_prefix+entity_name, ename_prefix+entity_name), '.TRUE.']}
                else:
                    attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(ename_prefix+entity_name), \
                        'kgen_array_sum', 'REAL(SUM(%s), 8)'%(ename_prefix+entity_name), '.TRUE.']}
                part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)

            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%(ename_prefix+entity_name), 'REAL(SUM(%s), 8)'%(ename_prefix+entity_name)]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%(ename_prefix+entity_name), ename_prefix+entity_name]}
                part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'items': [ename_prefix+entity_name], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_genknode(kernel_id, partid, statements.Read, attrs=attrs)

            if var.is_array() and stmt.is_numeric():
                if isinstance(stmt, typedecl_statements.Real):
                    attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(ename_prefix+entity_name), \
                        'kgen_array_sum', 'REAL(SUM(%s, mask=(%s .eq. %s)), 8)'%(ename_prefix+entity_name, ename_prefix+entity_name, ename_prefix+entity_name), '.TRUE.']}
                else:
                    attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(ename_prefix+entity_name), \
                        'kgen_array_sum', 'REAL(SUM(%s), 8)'%(ename_prefix+entity_name), '.TRUE.']}
                part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)

            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%(ename_prefix+entity_name), 'REAL(SUM(%s), 8)'%(ename_prefix+entity_name)]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%(ename_prefix+entity_name),ename_prefix+entity_name]}
                namedpart_append_genknode(kernel_id, partid, statements.Write, attrs=attrs)

    def create_write_intrinsic(self, kernel_id, partid, entity_name, stmt, var):
        kgenunit = 'UNIT = kgen_unit'

        pobj = namedgen_write_istrue(kernel_id, partid, var, entity_name)

#        pobj = None
#        # if isarray
#        if var.is_array() and not var.is_explicit_shape_array():
#            attrs = {'expr': 'SIZE(%s)==1'%entity_name}
#            ifsizeobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'expr': 'UBOUND(%s, 1)<LBOUND(%s, 1)'%(entity_name, entity_name)}
#            ifarrobj = part_append_gensnode(ifsizeobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
#            part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#            attrs = {'expr': 'UBOUND(%s, 1)==0 .AND. LBOUND(%s, 1)==0'%(entity_name, entity_name)}
#            part_append_gensnode(ifarrobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
#            part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#            part_append_gensnode(ifarrobj, EXEC_PART, block_statements.Else, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
#            part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#            part_append_gensnode(ifsizeobj, EXEC_PART, block_statements.Else, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
#            part_append_gensnode(ifsizeobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#        # if allocatable
#        if var.is_allocatable():
#            attrs = {'expr': '.NOT. ALLOCATED(%s)'%entity_name}
#            ifallocobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
#            part_append_gensnode(ifallocobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#        # if pointer
#        if var.is_pointer():
#            attrs = {'expr': '.NOT. ASSOCIATED(%s)'%entity_name}
#            ifptrobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
#            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)
#
#
#        if (var.is_array() and not var.is_explicit_shape_array()) or var.is_allocatable() or var.is_pointer():
#
#            attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
#            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)
#
#            attrs = {'expr': 'kgen_istrue'}
#            iftrueobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)
#
#            pobj = iftrueobj

        attrs = {'items': [entity_name], 'specs': [kgenunit]}
        if pobj:
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%entity_name, 'REAL(SUM(%s), 8)'%entity_name]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%entity_name, 'REAL(SUM(%s), 8)'%entity_name]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

    def create_read_call(self, kernel_id, partid, callname, entity_name, stmt, var, ename_prefix=''):
        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [ename_prefix+entity_name, 'kgen_unit', '"%s%s"'%(ename_prefix, entity_name)]}
            namedpart_append_genknode(kernel_id, partid, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [ename_prefix+entity_name, 'kgen_unit']}
            namedpart_append_genknode(kernel_id, partid, statements.Call, attrs=attrs)


    def create_write_call(self, kernel_id, partid, callname, entity_name, stmt, var):
        kgenunit = 'kgen_unit'

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [entity_name, kgenunit, '"%s"'%entity_name]}
            namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [entity_name, kgenunit]}
            namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
