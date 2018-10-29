# simple_timing.py

#import os 
from parser import statements, block_statements, typedecl_statements
from kgplugin import Kgen_Plugin

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

        if getinfo('is_papi_enabled'):
            part_append_comment(node, DECL_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['DIMENSION(1)'], \
                'entity_decls': ['kgen_papi_events = (/ KGENPAPIEVENT /)']}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['DIMENSION(2)'], 'selector': (None, '8'), \
                'entity_decls': ['kgen_papi_values']}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_retval']}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            part_append_comment(node, DECL_PART, '#else', style='rawtext')

        attrs = {'type_spec': 'INTEGER', 'selector': ('8', None), \
            'entity_decls': ['kgen_start_clock', 'kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        if getinfo('is_papi_enabled'):
            part_append_comment(node, DECL_PART, '#endif', style='rawtext')

        #attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER'], 'entity_decls': ['kgen_maxiter = %s'%getinfo('repeat_count')]}
        #part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        prenode = getinfo('blocknode_aftercallsite_main')
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            prenode, None, self.add_execblock)

    def add_execblock(self, node):

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'designator': 'PAPIF_start_counters', 'items': ['kgen_papi_events', '1', 'kgen_retval']}
            part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

            part_append_comment(node, EXEC_PART, '#else', style='rawtext')


        attrs = {'designator': 'mpi_barrier', 'items': [getinfo('mpi_comm'), 'kgen_ierr']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_start_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#endif', style='rawtext')

        #execpart = get_part(node, EXEC_PART)
        #namedpart_create_subpart(doobj, KERNEL_PBLOCK_TIMING, EXEC_PART)

        kernel_stmts = getinfo('callsite_stmts')
        start = kernel_stmts[0].item.span[0]-1
        end = kernel_stmts[-1].item.span[1]
        lines = kernel_stmts[0].top.prep[start:end]
        lines_str = '\n'.join(lines)
        dummy_node = part_append_genknode(node, EXEC_PART, statements.Call)
        dummy_node.kgen_stmt = getinfo('dummy_stmt')
        dummy_node.kgen_forced_line = lines_str

        #namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, 'TEST!!!!')

        #attrs = {'variable': 'kgen_resetinvoke', 'sign': '=', 'expr': '.TRUE.'}
        #namedpart_append_gensnode(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, statements.Assignment, attrs=attrs)
        #for elem in execpart:
        #    print 'AAA', hasattr(elem, 'kgen_stmt') , self.ispstmt(getinfo('callsite_stmts')[0], elem.kgen_stmt, node.kgen_stmt)    
        #    if hasattr(elem, 'kgen_stmt') and self.ispstmt(getinfo('callsite_stmts')[0], elem.kgen_stmt, node.kgen_stmt):
        #        namedpart_append_node(node.kgen_kernel_id, KERNEL_PBLOCK_TIMING, elem)
        #import pdb; pdb.set_trace()

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'designator': 'PAPIF_read_counters', 'items': ['kgen_papi_values(1)', '1', 'kgen_retval']}
            part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'PAPIF_stop_counters', 'items': ['kgen_papi_values(2)', '1', 'kgen_retval']}
            part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'variable': 'kgen_measure', 'sign': '=', 'expr': 'kgen_papi_values(1)'}
            part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['"%s : KGENPAPIEVENT per call: "'%getinfo('kernel_name'), 'kgen_measure']}
            part_append_gensnode(node, EXEC_PART, statements.Write, attrs=attrs)

            part_append_comment(node, EXEC_PART, '#else', style='rawtext')

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        attrs = {'designator': 'mpi_barrier', 'items': [getinfo('mpi_comm'), 'kgen_ierr']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)


        attrs = {'variable': 'kgen_measure', 'sign': '=', 'expr': '1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        #jgw# add mpi rank to timing result. assumes kgen_mpirank defined.
        attrs = {'items': ['"%s : Time per call (usec): "'%getinfo('kernel_name'), 'kgen_mpirank', 'kgen_measure']}
        part_append_gensnode(node, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#endif', style='rawtext')

