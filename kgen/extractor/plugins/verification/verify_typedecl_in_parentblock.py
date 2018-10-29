# gen_write_typedecl_in_parentblock.py
 
from parser import statements, block_statements, typedecl_statements
from kgplugin import Kgen_Plugin
from verify_utils import VERIFY_PBLOCK_LOCALS, get_typedecl_verifyname, get_dtype_verifyname, is_remove_state, is_remove_io, is_zero_array, check_class_derived
from verify_subr import create_verify_subr

class Verify_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.verify_locals = []
        self.verify_parentblock_subrnames = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per typedecl 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_out_locals_in_parentblock, self.create_subr_verify_typedecl_in_parentblock) 

    def is_out_locals_in_parentblock(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0 and \
            KGGenType.has_state_out(node.kgen_stmt.geninfo) and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            return True
        return False

    def create_subr_verify_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt
        is_class_derived = check_class_derived(stmt)
        entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])
        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            if entity_name in self.verify_locals: continue

            var = stmt.get_variable(entity_name)

            if is_remove_state(entity_name, stmt): continue
            if is_remove_io(entity_name, stmt): continue
            if var.is_array() and is_zero_array(var, stmt): continue

            self.verify_locals.append(entity_name)
            subrname = get_typedecl_verifyname(stmt, entity_name)

            if subrname not in self.verify_parentblock_subrnames:

                if stmt.is_derived() or is_class_derived:
                    if var.is_pointer() or var.is_array():
                        self.verify_parentblock_subrnames.append(subrname)
                        create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                    else:
                        subrname = None
                        for uname, req in stmt.unknowns.iteritems():
                            if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                            #if uname.firstpartname()==stmt.name:
                                if len(req.res_stmts)>0:
                                    subrname = get_dtype_verifyname(req.res_stmts[0])
                                    break
                        if subrname:
                            self.verify_parentblock_subrnames.append(subrname)
                else:
                    self.verify_parentblock_subrnames.append(subrname)
                    create_verify_subr(subrname, entity_name, node.kgen_parent, var, stmt)

            if subrname:
                attrs = {'designator': subrname, 'items': ['"%s"'%entity_name, 'check_status', entity_name, 'kgenref_%s'%entity_name]}
                namedpart_append_genknode(node.kgen_kernel_id, VERIFY_PBLOCK_LOCALS, statements.Call, attrs=attrs)
