
import statements
from kgen_plugin import Kgen_Plugin

BEFORE_CALLSITE = 'before_callsite'
AFTER_CALLSITE = 'after_callsite'

class KernelEtime(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        topblock_stmt = getinfo('topblock_stmt')
        parentblock_stmt = getinfo('parentblock_stmt')
        callsite_stmts = getinfo('callsite_stmts')

        # register event per function 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            topblock_stmt, None, self.module_update)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            parentblock_stmt, None, self.function_update)

        if len(callsite_stmts)>1:
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
                callsite_stmts[0], None, self.create_beginpart)
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
                callsite_stmts[-1], None, self.create_endpart)
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
                callsite_stmts[0], None, self.add_timing_begin)
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
                callsite_stmts[-1], None, self.add_timing_end)

        else:
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
                callsite_stmts[0], None, self.create_beginendpart)
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
                callsite_stmts[0], None, self.add_timing_block)

    def create_beginpart(self, node):
        index, partname, part = get_part_index(node)
        namedpart_create_subpart(node.kgen_parent, BEFORE_CALLSITE, EXEC_PART, index=index)

    def create_endpart(self, node):
        index, partname, part = get_part_index(node)
        namedpart_create_subpart(node.kgen_parent, AFTER_CALLSITE, EXEC_PART, index=(index+1))

    def create_beginendpart(self, node):
        self.create_beginpart(node)
        self.create_endpart(node)

    def module_update(self, node):
        pass

    def function_update(self, node):
        pass

    def add_timing_begin(self, node):
        attrs = {'items': ['ttt'], 'specs': ['UNIT = kgen_unit']}
        namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Write, attrs=attrs)

    def add_timing_end(self, node):
        attrs = {'items': ['uuu'], 'specs': ['UNIT = kgen_unit']}
        namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Write, attrs=attrs)

    def add_timing_block(self, node):
        self.add_timing_begin(node)
        self.add_timing_end(node)

