# gen_write_typedecl_in_parentblock.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import STATE_PBLOCK_WRITE_IN_ARGS, STATE_PBLOCK_WRITE_IN_LOCALS, STATE_PBLOCK_WRITE_OUT_LOCALS, \
    DRIVER_READ_IN_ARGS, KERNEL_PBLOCK_READ_IN_LOCALS, KERNEL_PBLOCK_READ_OUT_LOCALS, \
    DRIVER_DECL_PART, DRIVER_USE_PART, get_typedecl_writename, get_dtype_writename, state_gencore_contains, \
    get_topname, get_typedecl_readname, get_dtype_readname
from gencore_subr import create_write_subr, create_read_subr

class Gen_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_created_subrs = []
        self.kernel_created_subrs = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.create_subr_write_typedecl_in_parentblock) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.create_subr_read_typedecl_in_parentblock) 

    def typedecl_has_state_parentblock(self, node):
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state(node.kgen_stmt.geninfo) \
            and "parameter" not in node.kgen_stmt.attrspec and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            return True
        return False

    def create_subr_read_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt

        argintype = []
        localintype = []
        localouttype = []
        for uname, req in KGGenType.get_state_in(stmt.geninfo):
            entity_name = uname.firstpartname()
            if entity_name not in argintype and any( attr.startswith('intent') for attr in stmt.attrspec ):
                var = stmt.get_variable(entity_name)

                argintype.append((entity_name, DRIVER_READ_IN_ARGS))

                getinfo('kernel_driver_callsite_args').append(entity_name)

                # add typedecl in driver
                attrs={'type_spec':stmt.__class__.__name__.upper(), 'selector':stmt.selector, 'entity_decls': [entity_name]}
                attrspec = []
                if var.is_array(): attrspec.append('DIMENSION(%s)'%','.join(':'*var.rank))
                if var.is_allocatable(): attrspec.append('ALLOCATABLE')
                    # deallocate
                if var.is_pointer(): attrspec.append('POINTER')
                attrs['attrspec'] = attrspec 
                namedpart_append_genknode(node.kgen_kernel_id, DRIVER_DECL_PART, stmt.__class__, attrs=attrs)

                if hasattr(stmt, 'unknowns'):
                    for uname, req in stmt.unknowns.iteritems():
                        if req.res_stmts[-1].__class__==statements.Use:
                            attrs = {'name':req.res_stmts[-1].name, 'isonly': True, 'items':[uname.firstpartname()]}
                            namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
                        else:
                            attrs = {'name':get_topname(req.res_stmts[-1]), 'isonly': True, 'items':[uname.firstpartname()]}
                            namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
            elif entity_name not in localintype:
                localintype.append((uname.firstpartname(), KERNEL_PBLOCK_READ_IN_LOCALS))
        for uname, req in KGGenType.get_state_out(stmt.geninfo):
            entity_name = uname.firstpartname()
            if entity_name not in localouttype:
                localouttype.append((uname.firstpartname(), KERNEL_PBLOCK_READ_OUT_LOCALS))
        vartypes = { 'argintype': argintype, 'localintype': localintype, 'localouttype': localouttype }

        # for kernel
        for vartypename, vartype in vartypes.iteritems():
            for entity_name, partid in vartype:
                var = stmt.get_variable(entity_name)
                subrname = get_typedecl_readname(stmt, entity_name)
                if var.is_array():
                    if stmt.is_derived():
                        self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                            self.kernel_created_subrs.append(subrname)
                    else: # intrinsic type
                        if var.is_explicit_shape_array():
                            if vartypename=='argintype':
                                self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var, force_allocate=True)
                            else:    
                                self.create_read_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)
                        else: # implicit array
                            self.create_read_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                            if subrname not in self.kernel_created_subrs:
                                create_read_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                self.kernel_created_subrs.append(subrname)
                else: # scalar
                    if stmt.is_derived():
                        subrname = None
                        # TODO : add use and public statement??? in bridge modules?
                        for uname, req in stmt.unknowns.iteritems():
                            if uname.firstpartname()==stmt.name:
                                res = req.res_stmts[0]
                                subrname = get_dtype_readname(res)
                                break
                        if subrname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
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
            if entity_name not in argintype and any( attr.startswith('intent') for attr in stmt.attrspec ):
                argintype.append((entity_name, STATE_PBLOCK_WRITE_IN_ARGS))
            elif entity_name not in localintype:
                localintype.append((uname.firstpartname(), STATE_PBLOCK_WRITE_IN_LOCALS))
        for uname, req in KGGenType.get_state_out(stmt.geninfo):
            entity_name = uname.firstpartname()
            if entity_name not in localouttype:
                localouttype.append((uname.firstpartname(), STATE_PBLOCK_WRITE_OUT_LOCALS))
        vartypes = { 'argintype': argintype, 'localintype': localintype, 'localouttype': localouttype }

        # for state
        for vartypename, vartype in vartypes.iteritems():
            for entity_name, partid in vartype:
                var = stmt.get_variable(entity_name)
                subrname = get_typedecl_writename(stmt, entity_name)
                if var.is_array():
                    if stmt.is_derived():
                        self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                        if subrname not in self.state_created_subrs:
                            create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                            self.state_created_subrs.append(subrname)
                    else: # intrinsic type
                        if var.is_explicit_shape_array():
                            self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)
                        else: # implicit array
                            self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                            if subrname not in self.state_created_subrs:
                                create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                self.state_created_subrs.append(subrname)
                else: # scalar
                    if stmt.is_derived():
                        subrname = None
                        # TODO : add use and public statement??? in bridge modules?
                        for uname, req in stmt.unknowns.iteritems():
                            if uname.firstpartname()==stmt.name:
                                res = req.res_stmts[0]
                                subrname = get_dtype_writename(res)
                                break
                        if subrname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
                        self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                    else: # intrinsic type
                        self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)

    def create_read_intrinsic(self, kernel_id, partid, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(kernel_id, partid, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_genknode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_genknode(kernel_id, partid, statements.Read, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                namedpart_append_genknode(kernel_id, partid, statements.Write, attrs=attrs)

    def create_write_intrinsic(self, kernel_id, partid, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
            ifptrobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

    def create_read_call(self, kernel_id, partid, callname, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_genknode(kernel_id, partid, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_genknode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                namedpart_append_genknode(kernel_id, partid, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                namedpart_append_genknode(kernel_id, partid, statements.Call, attrs=attrs)


    def create_write_call(self, kernel_id, partid, callname, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
            ifptrobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)

