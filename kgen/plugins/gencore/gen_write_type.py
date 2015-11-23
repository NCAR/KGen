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
                    subrname = get_dtype_writename(req.res_stmts[0])
                    checks = lambda n: isinstance(n.kgen_stmt, statements.Use) and (not n.kgen_stmt.isonly or \
                        subrname in [ item.split('=>')[0].strip() for item in n.kgen_stmt.items])
                    if subrname not in self.created_use_items and not part_has_node(parent, USE_PART, checks):
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        part_append_gensnode(parent, USE_PART, statements.Use, attrs=attrs)
                        self.created_use_items.append(subrname)
                        parent.kgen_stmt.top.used4genstate = True

                    checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and subrname in n.kgen_stmt.items
                    if subrname not in self.created_public_items and not part_has_node(parent, DECL_PART, checks):
                        attrs = {'items':[subrname]}
                        part_append_gensnode(parent, DECL_PART, statements.Public, attrs=attrs)
                        self.created_public_items.append(subrname)
                        parent.kgen_stmt.top.used4genstate = True

    def create_write_intrinsic(self, subrobj, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
            ifptrobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        attrs = {'items': ['var%%%s'%entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'expr': 'PRESENT( printvar )'}
            ifobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_write_call(self, subrobj, callname, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
            ifptrobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        attrs = {'expr': 'PRESENT( printvar )'}
        ifobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', 'printvar // "%%%s"'%entity_name]}
        part_append_gensnode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

        part_append_gensnode(ifobj, EXEC_PART, statements.Else)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', '"%%%s"'%entity_name]}
            part_append_gensnode(ifobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit']}
            part_append_gensnode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

    # process function
    def create_dtype_write_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_writename(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if  not part_has_node(parent, SUBP_PART, checks):

            checks = lambda n: isinstance(n.kgen_stmt, statements.Contains)
            if not self.is_contains_created and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_gensnode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                self.is_contains_created = True

            part_append_comment(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = part_append_gensnode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            node.kgen_stmt.top.used4genstate = True

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

            # is_true
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_true']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

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
                            self.create_write_intrinsic(subrobj, entity_name, stmt, var)

                part_append_comment(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_gensnode(parent, DECL_PART, statements.Public, attrs=attrs)

