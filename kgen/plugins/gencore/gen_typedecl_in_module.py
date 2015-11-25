# gen_write_typedecl_in_module.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_topname, get_typedecl_writename, get_dtype_writename, get_module_writename, STATE_PBLOCK_WRITE_IN_EXTERNS, \
    STATE_PBLOCK_USE_PART, state_gencore_contains, get_typedecl_readname, get_dtype_readname, get_module_readname, \
    KERNEL_PBLOCK_USE_PART, KERNEL_PBLOCK_READ_IN_EXTERNS
from gencore_subr import create_write_subr, create_read_subr

class Gen_Typedecl_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_module = None
        self.state_subrobj = None
        self.state_created_subrs = []

        self.kernel_module = None
        self.kernel_subrobj = None
        self.kernel_created_subrs = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.has_externs_in_module, self.create_state_module_parts) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.has_externs_in_module, self.create_kernel_module_parts) 

    def has_externs_in_module(self, node):
        checks = lambda n: hasattr(n.kgen_stmt, 'geninfo') and len(n.kgen_stmt.geninfo)>0 \
            and isinstance(n.kgen_stmt, typedecl_statements.TypeDeclarationStatement) \
            and "parameter" not in n.kgen_stmt.attrspec
        if part_has_node(node, DECL_PART, checks):
            return True
        return False

    def is_extern_in_kernel_module(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0 and \
            node.kgen_parent==self.kernel_module and 'parameter' not in node.kgen_stmt.attrspec:
            return True
        return False

    def is_extern_in_state_module(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0 and \
            node.kgen_parent==self.state_module and 'parameter' not in node.kgen_stmt.attrspec:
            return True
        return False

    def create_kernel_module_parts(self, node):
        self.kernel_module = node
        subrname = get_module_readname(node.kgen_stmt)
        attrs = {'name': subrname, 'args': ['kgen_unit']}
        part_append_comment(self.kernel_module, SUBP_PART, 'write state subroutine for %s'%subrname)
        self.kernel_subrobj = part_append_genknode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(self.kernel_module, SUBP_PART, '')

        # kgen_unit
        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
        part_append_genknode(self.kernel_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        part_append_comment(self.kernel_subrobj, DECL_PART, '')

        # add public stmt
        attrs = {'items':[self.kernel_subrobj.name]}
        part_append_genknode(node, DECL_PART, statements.Public, attrs=attrs)

        # register event per typedecl 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_extern_in_kernel_module, self.create_subr_read_typedecl_in_module) 

        # register event per module
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Module, self.has_externs_in_module, self.create_kernel_stmts_in_callsite) 

    def create_state_module_parts(self, node):
        node.kgen_stmt.top.used4genstate = True

        self.state_module = node
        subrname = get_module_writename(node.kgen_stmt)
        attrs = {'name': subrname, 'args': ['kgen_unit']}
        part_append_comment(self.state_module, SUBP_PART, 'write state subroutine for %s'%subrname)
        self.state_subrobj = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(self.state_module, SUBP_PART, '')

        # kgen_unit
        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
        part_append_gensnode(self.state_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        part_append_comment(self.state_subrobj, DECL_PART, '')

        # add public stmt
        attrs = {'items':[self.state_subrobj.name]}
        part_append_gensnode(node, DECL_PART, statements.Public, attrs=attrs)

        # register event per typedecl 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_extern_in_state_module, self.create_subr_write_typedecl_in_module) 

        # register event per module
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Module, self.has_externs_in_module, self.create_state_stmts_in_callsite) 

    def create_kernel_stmts_in_callsite(self, node):
        checks = lambda n: isinstance(n.kgen_stmt, statements.Use) and n.kgen_stmt.name==self.state_module.name and \
            ( not n.kgen_stmt.isonly or self.kernel_subrobj.name in [ item.split('=>')[0].strip() for item in n.kgen_stmt.items])
        if not namedpart_has_node(node.kgen_kernel_id, KERNEL_PBLOCK_USE_PART, checks):
            attrs = {'name':self.kernel_module.name, 'isonly': True, 'items':[self.kernel_subrobj.name]}
            namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_USE_PART, statements.Use, attrs=attrs)

        attrs = {'designator': self.kernel_subrobj.name, 'items': ['kgen_unit']}
        namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_EXTERNS, statements.Call, attrs=attrs)

    def create_state_stmts_in_callsite(self, node):
        checks = lambda n: isinstance(n.kgen_stmt, statements.Use) and n.kgen_stmt.name==self.state_module.name and \
            ( not n.kgen_stmt.isonly or self.state_subrobj.name in [ item.split('=>')[0].strip() for item in n.kgen_stmt.items])
        if not namedpart_has_node(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, checks):
            attrs = {'name':self.state_module.name, 'isonly': True, 'items':[self.state_subrobj.name]}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, statements.Use, attrs=attrs)

        attrs = {'designator': self.state_subrobj.name, 'items': ['kgen_unit']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, statements.Call, attrs=attrs)

    def create_subr_read_typedecl_in_module(self, node):
        stmt = node.kgen_stmt
        entity_names = [ uname.firstpartname() for uname, req in KGGenType.get_state(stmt.geninfo)]
        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_readname(stmt, entity_name)

            if var.is_array():
                if stmt.is_derived():
                    self.create_read_call(self.kernel_subrobj, subrname, entity_name, stmt, var)
                    if subrname not in self.kernel_created_subrs:
                        create_read_subr(subrname, entity_name, parent, var, stmt)
                        self.kernel_created_subrs.append(subrname)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        self.create_read_intrinsic(self.kernel_subrobj, entity_name, stmt, var)
                    else: # implicit array
                        self.create_read_call(self.kernel_subrobj, subrname, entity_name, stmt, var)
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, parent, var, stmt)
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
                    self.create_read_call(self.kernel_subrobj, subrname, entity_name, stmt, var)
                else: # intrinsic type
                    self.create_read_intrinsic(self.kernel_subrobj, entity_name, stmt, var)

    def create_subr_write_typedecl_in_module(self, node):
        stmt = node.kgen_stmt
        entity_names = [ uname.firstpartname() for uname, req in KGGenType.get_state(stmt.geninfo)]
        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_writename(stmt, entity_name)

            if var.is_array():
                if stmt.is_derived():
                    self.create_write_call(self.state_subrobj, subrname, entity_name, stmt, var)
                    if subrname not in self.state_created_subrs:
                        create_write_subr(subrname, entity_name, parent, var, stmt)
                        self.state_created_subrs.append(subrname)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        self.create_write_intrinsic(self.state_subrobj, entity_name, stmt, var)
                    else: # implicit array
                        self.create_write_call(self.state_subrobj, subrname, entity_name, stmt, var)
                        if subrname not in self.state_created_subrs:
                            create_write_subr(subrname, entity_name, parent, var, stmt)
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
                    self.create_write_call(self.state_subrobj, subrname, entity_name, stmt, var)
                else: # intrinsic type
                    self.create_write_intrinsic(self.state_subrobj, entity_name, stmt, var)

    def create_read_intrinsic(self, subrobj, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_write_intrinsic(self, subrobj, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
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

        attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_read_call(self, subrobj, callname, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
            part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)

    def create_write_call(self, subrobj, callname, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
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

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
            part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
            part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)

