# gen_write_typedecl_in_parentblock.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import STATE_PBLOCK_WRITE_IN_ARGS, STATE_PBLOCK_WRITE_IN_LOCALS, STATE_PBLOCK_WRITE_OUT_LOCALS,\
    get_typedecl_writename, get_dtype_writename, is_zero_array, is_remove_state, namedgen_write_istrue, check_class_derived 
from gencore_subr import create_write_subr

class Gen_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_created_subrs = []

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

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), self.has_implicit_rule_resolver, self.create_write_implicit_rule_in_parentblock) 

        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
        #    typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.remove_read_typedecl_in_parentblock) 

    def has_implicit_rule_resolver(self, node):
        if hasattr(node.kgen_stmt, 'implicit_rule_resolvers'):
            return True
        else: return False
 
    def create_write_implicit_rule_in_parentblock(self, node):
        for resolver in node.kgen_stmt.implicit_rule_resolvers:
            if resolver.name in node.kgen_stmt.args:
                partid = STATE_PBLOCK_WRITE_IN_ARGS
            else:
                partid = STATE_PBLOCK_WRITE_IN_LOCALS

            if KGGenType.has_state_out(resolver.geninfo):
                attrs = {'items': [resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_gensnode(node.kgen_kernel_id, partid, statements.Write, attrs=attrs)

                attrs = {'items': [resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, statements.Write, attrs=attrs)
            else:
                attrs = {'items': [resolver.name], 'specs': ['UNIT = kgen_unit']}
                namedpart_append_gensnode(node.kgen_kernel_id, partid, statements.Write, attrs=attrs)

    def typedecl_has_state_parentblock(self, node):
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state(node.kgen_stmt.geninfo) \
            and "parameter" not in node.kgen_stmt.attrspec and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            for entity_name in [ get_entity_name(decl) for decl in node.kgen_stmt.entity_decls ]:
                var = node.kgen_stmt.get_variable(entity_name)
                if not var.is_parameter():
                    return True
        return False

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

    def create_write_intrinsic(self, kernel_id, partid, entity_name, stmt, var):

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

        if pobj:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%entity_name, 'REAL(SUM(%s), 8)'%entity_name]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                if stmt.is_numeric() and var.is_array():
                    attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%entity_name, 'REAL(SUM(%s), 8)'%entity_name]}
                else:
                    attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)


    def create_write_call(self, kernel_id, partid, callname, entity_name, stmt, var):
        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
            namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
