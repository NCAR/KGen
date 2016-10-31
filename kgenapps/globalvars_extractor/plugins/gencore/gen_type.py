# gen_print_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_printname, get_typedecl_printname, state_gencore_contains, \
    gen_print_istrue, is_remove_state

class Gen_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.state_created_use_items = []
        self.state_created_public_items = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.has_state_info, self.create_dtype_print_subr) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, self.has_dtype_res_path, self.add_printnames_in_use_public) 

    def has_state_info(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0:
            #print node
            return True
        else: return False

    def has_dtype_res_path(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo'):
            if not node.kgen_stmt.isonly: return False
            for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
                if any(len(req.res_stmts)>0 and isinstance(req.res_stmts[0], block_statements.Type) for uname, req in reqlist):
                    return True
        return False

    def add_printnames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if len(req.res_stmts)>0 and isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_printname(req.res_stmts[0])
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

    #def create_print_intrinsic(self, namelist, subpnode, var):
    def create_print_intrinsic(self, subrobj, namelist, stmt, var):

        entity_name = namelist[-1]
        resstmt = var.parent

        pobj = gen_print_istrue(subrobj, var, 'var%%%s'%entity_name)

        attrs = {'items': ['"Component variable: %s"'%entity_name]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"   used at %s"'%str(namelist)]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)


        if hasattr(resstmt, 'name'):
            resnamelist = [ a.name.lower() for a in resstmt.ancestors() ] + [ resstmt.name ]
        else:
            resnamelist = [ a.name.lower() for a in resstmt.ancestors() ]

        attrs = {'items': ['"   declared at %s"'%str(resnamelist)]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

        if resstmt.is_numeric() and var.is_array():
            attrs = {'items': ['"   REAL(SUM(%s), 8) = "'%entity_name, 'kgen_array_sum' ]}
        else:
            attrs = {'items': ['"   value = ", var%%%s'%entity_name]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_print_call(self, subrobj, callname, namelist, stmt, resstmt):

        entity_name = namelist[-1]

        attrs = {'items': ['"Derivedtype variable: %s"'%entity_name]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"   used at %s"'%str(namelist)]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

        if hasattr(resstmt, 'name'):
            resnamelist = [ a.name.lower() for a in resstmt.ancestors() ] + [ resstmt.name ]
        else:
            resnamelist = [ a.name.lower() for a in resstmt.ancestors() ]

        attrs = {'items': ['"   declared at %s"'%str(resnamelist)]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'designator': callname, 'items': ['var%%%s'%entity_name]}
        part_append_gensnode(subrobj, EXEC_PART, statements.Call, attrs=attrs)

    # process function
    def create_dtype_print_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_printname(node.kgen_stmt)
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
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['var']}
            subrobj = part_append_gensnode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

            part_append_comment(subrobj, DECL_PART, '')

            #comp_part = get_part(node, TYPE_COMP_PART) 
            comp_part = get_part(node, TYPE_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                ancs = [ a.name for a in stmt.ancestors() ]
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]
                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):

                    if is_remove_state(entity_name, stmt): continue

                    node.kgen_stmt.top.used4genstate = True

                    var = stmt.get_variable(entity_name)
                    callname = get_typedecl_printname(stmt, entity_name)
                    namelist = ancs + [ entity_name ]
                    if var.is_array():
                        if stmt.is_derived():
                            resstmt = None
                            for un, res in stmt.unknowns.items():
                                if stmt.name == un.firstpartname():
                                    resstmt = res.res_stmts[0]
                            self.create_print_call(subrobj, callname, namelist, stmt, resstmt)
                        else: # intrinsic type
                            if var.is_explicit_shape_array():
                                self.create_print_intrinsic(subrobj, namelist, stmt, var)
                            else: # implicit array
                                self.create_print_call(subrobj, callname, namelist, stmt, stmt)
                    else: # scalar
                        if stmt.is_derived():
                            resstmt = None
                            for un, res in stmt.unknowns.items():
                                if stmt.name == un.firstpartname():
                                    resstmt = res.res_stmts[0]

                            if var.is_allocatable() or var.is_pointer():
                                self.create_print_call(subrobj, callname, namelist, stmt, resstmt)
                            else:
                                callname = None
                                for uname, req in stmt.unknowns.iteritems():
                                    if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                        res = req.res_stmts[0]
                                        callname = get_dtype_printname(res)
                                        break
                                if callname is None:
                                    print 'WARNING: Can not find Type resolver for %s'%stmt.name
                                    part_append_comment(subrobj, EXEC_PART, \
                                        'ERROR: "%s" is not resolved. Call statement to print "%s" is not created here.'%\
                                        (stmt.name, stmt.name))
                                else:
                                    self.create_print_call(subrobj, callname, namelist, stmt, resstmt)
                        else: # intrinsic type
                            self.create_print_intrinsic(subrobj, namelist, stmt, var)

                part_append_comment(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_gensnode(parent, DECL_PART, statements.Public, attrs=attrs)

