# simple_timing.py

#import os 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin


class Simple_Timing(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per function 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), None, self.add_simple_timing) 

    def add_simple_timing(self, node):

        attrs = {'type_spec': 'INTEGER', 'selector': ('8', None), \
            'entity_decls': ['kgen_intvar', 'kgen_start_clock', 'kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER'], 'entity_decls': ['kgen_maxiter = %s'%getinfo('repeat_count')]}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'REAL', 'selector': (None, 'kgen_dp'), 'entity_decls': ['kgen_elapsed_time']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_start_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'loopcontrol': 'kgen_intvar = 1, kgen_maxiter'}
        doobj = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)

        callsite_stmts = getinfo('callsite_stmts')
        start = callsite_stmts[0].item.span[0]-1
        end = callsite_stmts[-1].item.span[1]
        lines = callsite_stmts[0].top.prep[start:end]
        lines_str = '\n'.join(lines)
        dummy_node = part_append_genknode(doobj, EXEC_PART, statements.Call)
        dummy_node.kgen_stmt = callsite_stmts[0]
        dummy_node.kgen_forced_line = lines_str
        
        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'variable': 'kgen_elapsed_time', 'sign': '=', 'expr': '1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'items': ['"%s : Time per call (usec): "'%getinfo('kernel_name'), 'kgen_elapsed_time']}
        part_append_gensnode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': 'kgen_total_time + kgen_elapsed_time'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

