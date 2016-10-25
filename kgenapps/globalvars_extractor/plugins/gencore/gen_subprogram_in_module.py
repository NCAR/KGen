# gen_write_typedecl_in_module.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin


class Gen_SubProgram_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.SubProgramStatement, self.has_globalvars_in_module, self.create_globalvar_status) 

    def has_globalvars_in_module(self, node):
        if hasattr(node, 'kgen_stmt') and hasattr(node.kgen_stmt, 'globalvars'):
            return True
        return False

    def create_globalvar_status(self, node):
        for namelist, res in node.kgen_stmt.globalvars.items():
            attrs = {'items': ['"%s"'%str(namelist)] }
            part_insert_gensnode(node, EXEC_PART, statements.Write, attrs=attrs, index=0)
