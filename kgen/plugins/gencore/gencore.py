# gencore.py
 
import block_statements
from kgen_plugin import Kgen_Plugin

class Gen_K_Program(Kgen_Plugin):
    def is_driver_name(self, node):
        if node.name==UI_INFO.KERNEL_DRIVER_NAME: return True
        else: return False

    def create_stmt_parts(self, node):
        print 'DRIVER is created.'
        #import pdb; pdb.set_trace()
        pass

    def recover_stmt_parts(self, node):
        print 'DRIVER parts are recovered.'
        #import pdb; pdb.set_trace()
        pass

    def register(self, msg):
        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Program, self.is_driver_name, self.create_stmt_parts) 

        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            block_statements.Program, self.is_driver_name, self.recover_stmt_parts) 

class Gen_K_Module(Kgen_Plugin):
    def match_func(self, node):
        return False

    def event_occurred(self, node):
        pass

    def register(self, msg):
        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.match_func, self.event_occurred) 
