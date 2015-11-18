# gencore.py

import block_statements
from kgen_plugin import Kgen_Plugin

class Gen_K_Program(Kgen_Plugin):
    def is_driver(self, obj):
        if obj.name=='kernel_driver': return True
        else: return False

    def create_driver_header(self, obj):
        print 'POOOO: '
        pass

    def register(self, msg):
        msg.add_event(msg.NODE_CREATED, block_statements.Program, self.is_driver, self.create_driver_header) 

class Gen_K_Module(Kgen_Plugin):
    def match_func(self, obj):
        return False

    def event_occurred(self, obj):
        pass

    def register(self, msg):
        msg.add_event(msg.NODE_CREATED, block_statements.Module, self.match_func, self.event_occurred) 
