# verify_write_typedecl_in_module.py
 
from parser import statements, block_statements, typedecl_statements
from collections import OrderedDict
from kgplugin import Kgen_Plugin

from verify_utils import get_module_verifyname, kernel_verify_contains, VERIFY_PBLOCK_USE_PART, VERIFY_PBLOCK_EXTERNS, \
    get_typedecl_verifyname, get_dtype_verifyname, is_remove_state, is_remove_io, is_zero_array, check_class_derived
from verify_subr import create_verify_subr

class Verify_Typedecl_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.verify_externs_subrs = OrderedDict()
        self.verify_callsite_use_stmts = []
        self.verify_callsite_call_stmts = []
        self.verify_extern = []
        self.verify_module_subrnames = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.has_out_externs_in_module, self.create_verify_module_parts) 

    def is_out_extern_in_verify_module(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0 and \
            KGGenType.has_state_out(node.kgen_stmt.geninfo) and isinstance(node.kgen_parent.kgen_stmt, block_statements.Module):
            return True
        return False

    def has_out_externs_in_module(self, node):
#        checks = lambda n: hasattr(n.kgen_stmt, 'geninfo') and len(n.kgen_stmt.geninfo)>0 \
#            and isinstance(n.kgen_stmt, typedecl_statements.TypeDeclarationStatement) \
#            and KGGenType.has_state_out(n.kgen_stmt.geninfo)
#        if part_has_node(node, DECL_PART, checks):
#            return True
#        return False
        for stmt in node.kgen_stmt.content:
            if isinstance(stmt, typedecl_statements.TypeDeclarationStatement) and \
                hasattr(stmt, 'geninfo') and len(stmt.geninfo)>0 and KGGenType.has_state_out(stmt.geninfo) :
                return True
        return False

    def create_verify_module_parts(self, node):

        attrs = {'name': 'kgen_utils_mod', 'isonly': True, 'items':['check_t', 'kgen_init_check', 'kgen_tolerance', \
            'kgen_minvalue', 'CHECK_IDENTICAL', 'CHECK_IN_TOL', 'CHECK_OUT_TOL']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        subrname = get_module_verifyname(node.kgen_stmt)
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if not part_has_node(node, SUBP_PART, checks):

            checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
            if not node in kernel_verify_contains and not part_has_node(node, CONTAINS_PART, checks):
                part_append_comment(node, CONTAINS_PART, '')
                part_append_genknode(node, CONTAINS_PART, statements.Contains)
                part_append_comment(node, CONTAINS_PART, '')
                kernel_verify_contains.append(node)

            part_append_comment(node, SUBP_PART, 'verify state subroutine for %s'%subrname)
            attrs = {'name': subrname, 'args': ['check_status']}
            subrobj = part_append_genknode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            self.verify_externs_subrs[node] = subrobj

            # check_status
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(INOUT)'], 'selector':(None, 'check_t'), 'entity_decls': ['check_status']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

            part_append_comment(node, SUBP_PART, '')

            # add public stmt
            attrs = {'items':[subrname]}
            part_append_genknode(node, DECL_PART, statements.Public, attrs=attrs)

            # register event per typedecl 
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
                typedecl_statements.TypeDeclarationStatement, self.is_out_extern_in_verify_module, self.create_subr_verify_typedecl_in_module) 

            # register event per module
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
                block_statements.Module, self.has_out_externs_in_module, self.create_verify_stmts_in_callsite) 

    def create_verify_stmts_in_callsite(self, node):
        if not self.verify_externs_subrs[node] in self.verify_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
            attrs = {'name':node.name, 'isonly': True, 'items':[self.verify_externs_subrs[node].name]}
            namedpart_append_genknode(node.kgen_kernel_id, VERIFY_PBLOCK_USE_PART, statements.Use, attrs=attrs)
            self.verify_callsite_use_stmts.append(self.verify_externs_subrs[node])

        if not self.verify_externs_subrs[node] in self.verify_callsite_call_stmts:
            attrs = {'designator': self.verify_externs_subrs[node].name, 'items': ['check_status']}
            namedpart_append_genknode(node.kgen_kernel_id, VERIFY_PBLOCK_EXTERNS, statements.Call, attrs=attrs)
            self.verify_callsite_call_stmts.append(self.verify_externs_subrs[node])

    def create_subr_verify_typedecl_in_module(self, node):
        stmt = node.kgen_stmt
        is_class_derived = check_class_derived(stmt)
        entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])
        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            if entity_name in self.verify_extern: continue
            var = stmt.get_variable(entity_name)

            if is_remove_state(entity_name, stmt): continue
            if is_remove_io(entity_name, stmt): continue
            if var.is_array() and is_zero_array(var, stmt): continue

            self.verify_extern.append(entity_name)
            subrname = get_typedecl_verifyname(stmt, entity_name)

            if subrname not in self.verify_module_subrnames:
                self.verify_module_subrnames.append(subrname)

                if stmt.is_derived():
                    if var.is_pointer() or var.is_array():
                        create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                    else:
                        subrname = None
                        for uname, req in stmt.unknowns.iteritems():
                            if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                            #if uname.firstpartname()==stmt.name:
                                if len(req.res_stmts)>0:
                                    subrname = get_dtype_verifyname(req.res_stmts[0])
                                    break
                else:
                    create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)

            if subrname:
                attrs = {'designator': subrname, 'items': ['"%s"'%entity_name, 'check_status', entity_name, 'kgenref_%s'%entity_name]}
                part_append_genknode(self.verify_externs_subrs[node.kgen_parent], EXEC_PART, statements.Call, attrs=attrs)


