
from kgplugin import Kgen_Plugin
from parser import block_statements

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):

        self.frame_msg = msg

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.IfThen, None, self.proc_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.ElseIf, None, self.proc_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Else, None, self.proc_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.proc_ifstmt)

    def proc_ifthen(self, node):
        getinfo('logger').info('Begin proc_ifthen')
        #import pdb; pdb.set_trace()
        node.kgen_stmt.top.genspair.used4coverage = True

    def proc_elseif(self, node):
        getinfo('logger').info('Begin proc_elseif')
        #import pdb; pdb.set_trace()
        node.kgen_stmt.top.genspair.used4coverage = True

    def proc_else(self, node):
        getinfo('logger').info('Begin proc_else')
        #import pdb; pdb.set_trace()
        node.kgen_stmt.top.genspair.used4coverage = True

    #def proc_ifstmt(self, node):
    #    getinfo('logger').info('Begin proc_ifstmt')
    #    node.kgen_stmt.top.genspair.used4coverage = True
