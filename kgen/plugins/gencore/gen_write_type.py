# gen_write_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_writename, get_typedecl_subpname

class Gen_S_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.is_contains_created = False
        self.created_use_items = []
        self.created_public_items = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.has_state_info, self.create_dtype_write_subr) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, self.has_dtype_res_path, self.add_subrnames_in_use_public) 

    def has_state_info(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0:
            return True
        else: return False

    def has_dtype_res_path(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo'):
            if not node.kgen_stmt.isonly: return False
            for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
                if any(isinstance(req.res_stmts[0], block_statements.Type) for uname, req in reqlist):
                    return True
        return False

    def add_subrnames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_writename(node.kgen_stmt)
                    if subrname not in self.created_use_items and \
                        not has_object_in_part(parent, USE_PART, statements.Use, attrs={'kgen_stmt.items':subrname}, method=lambda x, y: x in y ): 
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        append_item_in_part(parent, USE_PART, gensobj(parent, statements.Use, parent.kgen_kernel_id, attrs=attrs))
                        self.created_use_items.append(subrname)
                    if subrname not in self.created_public_items and \
                        not has_object_in_part(parent, DECL_PART, statements.Use, attrs={'kgen_stmt.items':subrname}, method=lambda x, y: x in y ):
                        attrs = {'items':[subrname]}
                        append_item_in_part(parent, DECL_PART, gensobj(parent, statements.Public, parent.kgen_kernel_id, attrs=attrs))
                        self.created_public_items.append(subrname)

    def create_write_intrinsic(self, subrobj, entity_name, stmt, var):
        pobj = subrobj
        if not var.is_array() and var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
            ifptrobj = gensobj(subrobj, block_statements.IfThen, subrobj.kgen_kernel_id, attrs=attrs)
            append_item_in_part(subrobj, EXEC_PART, ifptrobj)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Assignment, ifptrobj.kgen_kernel_id, attrs=attrs))

            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Else, ifptrobj.kgen_kernel_id))

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Assignment, ifptrobj.kgen_kernel_id, attrs=attrs))

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            append_item_in_part(subrobj, EXEC_PART, gensobj(subrobj, statements.Write, subrobj.kgen_kernel_id, attrs=attrs))

            attrs = {'expr': 'is_true'}
            iftrueobj = gensobj(subrobj, block_statements.IfThen, subrobj.kgen_kernel_id, attrs=attrs)
            append_item_in_part(subrobj, EXEC_PART, iftrueobj)

            pobj = iftrueobj

        attrs = {'items': ['var%%%s'%entity_name], 'specs': ['UNIT = kgen_unit']}
        append_item_in_part(pobj, EXEC_PART, gensobj(pobj, statements.Write, pobj.kgen_kernel_id, attrs=attrs))

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            append_item_in_part(pobj, EXEC_PART, gensobj(pobj, statements.Write, pobj.kgen_kernel_id, attrs=attrs))
        else:
            attrs = {'expr': 'PRESENT( printvar )'}
            ifobj = gensobj(pobj, block_statements.IfThen, pobj.kgen_kernel_id, attrs=attrs)
            append_item_in_part(pobj, EXEC_PART, ifobj)

            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            append_item_in_part(ifobj, EXEC_PART, gensobj(ifobj, statements.Write, ifobj.kgen_kernel_id, attrs=attrs))

    def create_write_call(self, subrobj, callname, entity_name, stmt, var):
        pobj = subrobj
        if not var.is_array() and var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
            ifptrobj = gensobj(subrobj, block_statements.IfThen, subrobj.kgen_kernel_id, attrs=attrs)
            append_item_in_part(subrobj, EXEC_PART, ifptrobj)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Assignment, ifptrobj.kgen_kernel_id, attrs=attrs))

            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Else, ifptrobj.kgen_kernel_id))

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            append_item_in_part(ifptrobj, EXEC_PART, gensobj(ifptrobj, statements.Assignment, ifptrobj.kgen_kernel_id, attrs=attrs))

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            append_item_in_part(subrobj, EXEC_PART, gensobj(subrobj, statements.Write, subrobj.kgen_kernel_id, attrs=attrs))

            attrs = {'expr': 'is_true'}
            iftrueobj = gensobj(subrobj, block_statements.IfThen, subrobj.kgen_kernel_id, attrs=attrs)
            append_item_in_part(subrobj, EXEC_PART, iftrueobj)

            pobj = iftrueobj

        attrs = {'expr': 'PRESENT( printvar )'}
        ifobj = gensobj(pobj, block_statements.IfThen, pobj.kgen_kernel_id, attrs=attrs)
        append_item_in_part(pobj, EXEC_PART, ifobj)

        attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', 'printvar // "%%%s"'%entity_name]}
        append_item_in_part(ifobj, EXEC_PART, gensobj(ifobj, statements.Call, ifobj.kgen_kernel_id, attrs=attrs))

        append_item_in_part(ifobj, EXEC_PART, gensobj(ifobj, statements.Else, ifobj.kgen_kernel_id))

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', '"%%%s"'%entity_name]}
            append_item_in_part(ifobj, EXEC_PART, gensobj(ifobj, statements.Call, ifobj.kgen_kernel_id, attrs=attrs))
        else:
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit']}
            append_item_in_part(ifobj, EXEC_PART, gensobj(ifobj, statements.Call, ifobj.kgen_kernel_id, attrs=attrs))

    # process function
    def create_dtype_write_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_writename(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        if  not has_object_in_part(parent, SUBP_PART, block_statements.Subroutine, attrs={'name':subrname} ):

            if not self.is_contains_created and not has_object_in_part(parent, CONTAINS_PART, statements.Contains ):
                append_comment_in_part(parent, CONTAINS_PART, '')
                append_item_in_part(parent, CONTAINS_PART, gensobj(parent, statements.Contains, node.kgen_kernel_id))
                append_comment_in_part(parent, CONTAINS_PART, '')
                self.is_contains_created = True

            append_comment_in_part(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = gensobj(parent, block_statements.Subroutine, node.kgen_kernel_id, attrs=attrs)
            append_item_in_part(parent, SUBP_PART, subrobj)
            append_comment_in_part(parent, SUBP_PART, '')

            node.kgen_stmt.top.used4genstate = True

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Type, node.kgen_kernel_id, attrs=attrs))

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Character, node.kgen_kernel_id, attrs=attrs))

            # is_true
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_true']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Logical, subrobj.kgen_kernel_id, attrs=attrs))
            append_comment_in_part(subrobj, DECL_PART, '')

            comp_part = get_part(node, TYPE_COMP_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]
                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
                    var = stmt.get_variable(entity_name)
                    callname = get_typedecl_subpname(stmt, entity_name)

                    if var.is_array():
                        if stmt.is_derived():
                            self.create_write_call(subrobj, callname, entity_name, stmt, var)
                        else: # intrinsic type
                            if var.is_explicit_shape_array():
                                self.create_write_intrinsic(subrobj, entity_name, stmt, var)
                            else: # implicit array
                                self.create_write_call(subrobj, callname, entity_name, stmt, var)
                    else: # scalar
                        if stmt.is_derived():
                            callname = None
                            for uname, req in stmt.unknowns.iteritems():
                                if uname.firstpartname()==stmt.name:
                                    res = req.res_stmts[0]
                                    callname = get_dtype_writename(res)
                                    break
                            if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
                            self.create_write_call(subrobj, callname, entity_name, stmt, var)
                        else: # intrinsic type
                            if var.is_pointer():
                                self.create_write_call(subrobj, callname, entity_name, stmt, var)
                            else: # not a pointer
                                self.create_write_intrinsic(subrobj, entity_name, stmt, var)

                append_comment_in_part(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                append_item_in_part(parent, DECL_PART, gensobj(parent, statements.Public, node.kgen_kernel_id, attrs=attrs))

