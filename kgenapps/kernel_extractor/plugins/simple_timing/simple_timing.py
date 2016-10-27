# simple_timing.py

#import os 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

KERNEL_PBLOCK_TIMING = 'KPBTIM'

class Simple_Timing(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per function 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.register_event) 

    def ispstmt(self, stmt, cmpstmt, limitstmt):
        if stmt == cmpstmt:
            return True
        elif stmt == limitstmt:
            return False
        elif hasattr(stmt, 'parent'):
            return self.ispstmt(stmt.parent, cmpstmt, limitstmt)
        else:
            return False

    def register_event(self, node):

        attrs = {'type_spec': 'INTEGER', 'selector': ('8', None), \
            'entity_decls': ['kgen_intvar', 'kgen_start_clock', 'kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER'], 'entity_decls': ['kgen_maxiter = %s'%getinfo('repeat_count')]}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        prenode = getinfo('blocknode_aftercallsite_main')
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            prenode, None, self.add_execblock)

    def add_execblock(self, node):

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_start_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'loopcontrol': 'kgen_intvar = 1, kgen_maxiter'}
        doobj = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)
           
        execpart = get_part(node, EXEC_PART)
        namedpart_create_subpart(doobj, KERNEL_PBLOCK_TIMING, EXEC_PART)

        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, 'TEST!!!!')

        #attrs = {'variable': 'kgen_resetinvoke', 'sign': '=', 'expr': '.TRUE.'}
        #namedpart_append_gensnode(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, statements.Assignment, attrs=attrs)

        for elem in execpart:
            if hasattr(elem, 'kgen_stmt') and self.ispstmt(getinfo('callsite_stmts')[0], elem.kgen_stmt, node.kgen_stmt):
                namedpart_append_node(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, elem)

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'variable': 'kgen_elapsed_time', 'sign': '=', 'expr': '1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'items': ['"%s : Time per call (usec): "'%getinfo('kernel_name'), 'kgen_elapsed_time']}
        part_append_gensnode(node, EXEC_PART, statements.Write, attrs=attrs)
