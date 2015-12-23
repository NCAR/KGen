# gen_write_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_writename, get_typedecl_writename, state_gencore_contains, \
    get_dtype_readname, get_typedecl_readname, kernel_gencore_contains, gen_write_istrue, \
    gen_read_istrue

class Gen_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.kernel_created_use_items = []
        self.kernel_created_public_items = []

        self.state_created_use_items = []
        self.state_created_public_items = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.has_state_info, self.create_dtype_write_subr) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, self.has_dtype_res_path, self.add_writenames_in_use_public) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.has_state_info, self.create_dtype_read_subr) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, self.has_dtype_res_path, self.add_readnames_in_use_public) 

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

    def add_readnames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_readname(req.res_stmts[0])
                    checks = lambda n: n.kgen_match_class==statements.Use and n.kgen_stmt and n.kgen_stmt.isonly and \
                        subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_use_items and not part_has_node(parent, USE_PART, checks):
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        part_append_genknode(parent, USE_PART, statements.Use, attrs=attrs)
                        self.kernel_created_use_items.append((id(parent),subrname))

                    checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_public_items and isinstance(parent.kgen_stmt, block_statements.Module) and \
                        not part_has_node(parent, DECL_PART, checks):
                        attrs = {'items':[subrname]}
                        part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)
                        self.kernel_created_public_items.append((id(parent),subrname))

    def add_writenames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_writename(req.res_stmts[0])
                    checks = lambda n: n.kgen_match_class==statements.Use and n.kgen_stmt and n.kgen_stmt.isonly and \
                        subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.state_created_use_items and not part_has_node(parent, USE_PART, checks):
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        part_append_gensnode(parent, USE_PART, statements.Use, attrs=attrs)
                        self.state_created_use_items.append((id(parent),subrname))
                        parent.kgen_stmt.top.used4genstate = True

                    checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.state_created_public_items and isinstance(parent.kgen_stmt, block_statements.Module) and \
                        not part_has_node(parent, DECL_PART, checks):
                        attrs = {'items':[subrname]}
                        part_append_gensnode(parent, DECL_PART, statements.Public, attrs=attrs)
                        self.state_created_public_items.append((id(parent),subrname))
                        parent.kgen_stmt.top.used4genstate = True

    def create_read_intrinsic(self, subrobj, entity_name, stmt, var):

        pobj = gen_read_istrue(subrobj, var, 'var%%%s'%entity_name)

        if var.is_allocatable() or var.is_pointer():
            attrs = {'items': ['var%%%s'%entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['var%%%s'%entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'SUM(var%%%s)'%entity_name]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **" // NEW_LINE("A")'%entity_name, 'var%%%s'%entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'expr': 'PRESENT( printvar )'}
            ifobj = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'SUM(var%%%s)'%entity_name]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **" // NEW_LINE("A")'%entity_name, 'var%%%s'%entity_name]}
            part_append_genknode(ifobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_write_intrinsic(self, subrobj, entity_name, stmt, var):

        pobj = gen_write_istrue(subrobj, var, 'var%%%s'%entity_name)

        attrs = {'items': ['var%%%s'%entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'SUM(var%%%s)'%entity_name]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **" // NEW_LINE("A")'%entity_name, 'var%%%s'%entity_name]}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'expr': 'PRESENT( printvar )'}
            ifobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'SUM(var%%%s)'%entity_name]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **" // NEW_LINE("A")'%entity_name, 'var%%%s'%entity_name]}
            part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_read_call(self, subrobj, callname, entity_name, stmt, var):

        attrs = {'expr': 'PRESENT( printvar )'}
        ifobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', 'printvar // "%%%s"'%entity_name]}
        part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

        part_append_genknode(ifobj, EXEC_PART, statements.Else)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', '"%%%s"'%entity_name]}
            part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit']}
            part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

    def create_write_call(self, subrobj, callname, entity_name, stmt, var):

        attrs = {'expr': 'PRESENT( printvar )'}
        ifobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

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
    def create_dtype_read_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_readname(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if  not part_has_node(parent, SUBP_PART, checks):

            checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
            if not parent in kernel_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_genknode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                kernel_gencore_contains.append(parent)

            part_append_comment(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = part_append_genknode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(INOUT)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

            #comp_part = get_part(node, TYPE_COMP_PART) 
            comp_part = get_part(node, TYPE_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]

                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):

                    if hasattr(item.kgen_stmt, 'exclude_names'):
                        skip_verify = False
                        for exclude_name, actions in item.kgen_stmt.exclude_names.iteritems():
                            if exclude_name==entity_name and 'remove_state' in actions:
                                skip_verify = True
                                break
                        if skip_verify: continue

                    var = stmt.get_variable(entity_name)
                    callname = get_typedecl_readname(stmt, entity_name)

                    if var.is_array():
                        if stmt.is_derived():
                            self.create_read_call(subrobj, callname, entity_name, stmt, var)
                        else: # intrinsic type
                            if var.is_explicit_shape_array():
                                self.create_read_intrinsic(subrobj, entity_name, stmt, var)
                            else: # implicit array
                                self.create_read_call(subrobj, callname, entity_name, stmt, var)
                    else: # scalar
                        if stmt.is_derived():
                            if var.is_allocatable() or var.is_pointer():
                                self.create_read_call(subrobj, callname, entity_name, stmt, var)
                            else:
                                callname = None
                                for uname, req in stmt.unknowns.iteritems():
                                    if uname.firstpartname()==stmt.name:
                                        res = req.res_stmts[0]
                                        callname = get_dtype_readname(res)
                                        break
                                if callname is None:
                                    print 'WARNING: Can not find Type resolver for %s'%stmt.name
                                else:
                                    self.create_read_call(subrobj, callname, entity_name, stmt, var)
                        else: # intrinsic type
                            self.create_read_intrinsic(subrobj, entity_name, stmt, var)

                part_append_comment(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)

    # process function
    def create_dtype_write_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_writename(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if  not part_has_node(parent, SUBP_PART, checks):

            checks = lambda n: n.kgen_match_class==statements.Contains
            if not parent in state_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_gensnode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                state_gencore_contains.append(parent)

            part_append_comment(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = part_append_gensnode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

            #comp_part = get_part(node, TYPE_COMP_PART) 
            comp_part = get_part(node, TYPE_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]

                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):

                    if hasattr(item.kgen_stmt, 'exclude_names'):
                        skip_verify = False
                        for exclude_name, actions in item.kgen_stmt.exclude_names.iteritems():
                            if exclude_name==entity_name and 'remove_state' in actions:
                                skip_verify = True
                                break
                        if skip_verify: continue

                    node.kgen_stmt.top.used4genstate = True

                    var = stmt.get_variable(entity_name)
                    callname = get_typedecl_writename(stmt, entity_name)

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
                            if var.is_allocatable() or var.is_pointer():
                                self.create_write_call(subrobj, callname, entity_name, stmt, var)
                            else:
                                callname = None
                                for uname, req in stmt.unknowns.iteritems():
                                    if uname.firstpartname()==stmt.name:
                                        res = req.res_stmts[0]
                                        callname = get_dtype_writename(res)
                                        break
                                if callname is None:
                                    print 'WARNING: Can not find Type resolver for %s'%stmt.name
                                else:
                                    self.create_write_call(subrobj, callname, entity_name, stmt, var)
                        else: # intrinsic type
                            self.create_write_intrinsic(subrobj, entity_name, stmt, var)

                part_append_comment(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_gensnode(parent, DECL_PART, statements.Public, attrs=attrs)

