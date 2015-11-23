# gen_write_callsite_file.py

import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import PARENTBLOCK_USE_PART, PARENTBLOCK_DECL_PART, PARENTBLOCK_EXEC_PART, \
    PARENTBLOCK_CONTAINS_PART, PARENTBLOCK_SUBP_PART, PARENTBLOCK_WRITE_IN_EXTERNS, PARENTBLOCK_WRITE_IN_LOCALS, \
    PARENTBLOCK_WRITE_OUT_EXTERNS, PARENTBLOCK_WRITE_OUT_LOCALS, TOPBLOCK_USE_PART, TOPBLOCK_DECL_PART, \
    TOPBLOCK_CONTAINS_PART, TOPBLOCK_SUBP_PART, PARENTBLOCK_WRITE_IN_ARGS, gencore_contains

BEFORE_CALLSITE = 'before_callsite'
AFTER_CALLSITE = 'after_callsite'

class Gen_S_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('callsite_stmt'), None, self.create_callsite_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.create_parentblock_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.create_topblock_parts)


    def create_parentblock_parts(self, node):

        namedpart_link_part(node, PARENTBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, PARENTBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, PARENTBLOCK_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, PARENTBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, PARENTBLOCK_SUBP_PART, SUBP_PART)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            node, None, self.modify_parentblock_stmts)

        # ensure contains
        checks = lambda n: isinstance(n.kgen_stmt, statements.Contains)
        if not node in gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
            part_append_comment(node, CONTAINS_PART, '')
            part_append_gensnode(node, CONTAINS_PART, statements.Contains)
            part_append_comment(node, CONTAINS_PART, '')
            gencore_contains.append(node)

        #FUNCTION kgen_get_newunit() RESULT(new_unit)
        attrs = {'name': 'kgen_get_newunit', 'result': 'new_unit' }
        unitsubr = part_append_gensnode(node, SUBP_PART, block_statements.Function, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER'], 'entity_decls': ['UNIT_MIN=100', 'UNIT_MAX=1000000']}
        part_append_gensnode(unitsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)
    
        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_opened']}
        part_append_gensnode(unitsubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['nunit', 'new_unit', 'counter']}
        part_append_gensnode(unitsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        part_append_comment(unitsubr, DECL_PART, '')

        attrs = {'variable': 'new_unit', 'sign': '=', 'expr': '-1'}
        part_append_gensnode(unitsubr, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'loopcontrol': 'counter=UNIT_MIN, UNIT_MAX'}
        docnt = part_append_gensnode(unitsubr, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'specs': ['UNIT=counter', 'OPENED=is_opened']}
        part_append_gensnode(unitsubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. is_opened'}
        ifopen = part_append_gensnode(docnt, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'new_unit', 'sign': '=', 'expr': 'counter'}
        part_append_gensnode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_gensnode(ifopen, EXEC_PART, statements.Exit, attrs=attrs)

        #SUBROUTINE kgen_error_stop( msg )
        attrs = {'name': 'kgen_error_stop', 'args': ['msg']}
        stopsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['msg'], 'selector':('*', None)}
        part_append_gensnode(stopsubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'items': ['msg']}
        part_append_gensnode(stopsubr, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'code': '1'}
        part_append_gensnode(stopsubr, EXEC_PART, statements.Stop)

        #SUBROUTINE kgen_print_counter( counter )
        attrs = {'name': 'kgen_print_counter', 'args': ['counter']}
        cntsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['counter']}
        part_append_gensnode(cntsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': ['"KGEN writes input state variables at count = "', 'counter']}
        part_append_gensnode(cntsubr, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_mpi_app'):
            #SUBROUTINE kgen_print_mpirank_counter( rank, counter )
            attrs = {'name': 'kgen_print_mpirank_counter', 'args': ['rank', 'counter']}
            mpisubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(node, SUBP_PART, '')

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['rank', 'counter']}
            part_append_gensnode(mpisubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'items': ['"KGEN writes input state variables at count = "', 'counter']}
            part_append_gensnode(mpisubr, EXEC_PART, statements.Write, attrs=attrs)

    def create_topblock_parts(self, node):
        node.kgen_stmt.top.used4genstate = True

        namedpart_link_part(node, TOPBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, TOPBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, TOPBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, TOPBLOCK_SUBP_PART, SUBP_PART)

    def modify_parentblock_stmts(self, node):
        # modify arguments and others
        pass

    def create_callsite_parts(self, node):

        if getinfo('is_mpi_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank', 'kgen_mpi_size', 'kgen_cur_rank']}
            namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)
        
            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_mpi_rank_conv'], 'selector':('16', None)}
            namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Character, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('mpi_rank_size')], \
                'entity_decls': ['kgen_mpi_rank_at = (/ %s /)'%', '.join(getinfo('mpi_ranks'))]}
            namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit']}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_indexes'], 'attrspec': ['DIMENSION(3,10)']}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_counter = 1'], 'attrspec': ['SAVE']}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_counter_conv'], 'selector':('16', None)}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('invocation_size')], \
            'entity_decls': ['kgen_counter_at = (/ %s /)'%', '.join(getinfo('invocation_numbers'))]}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        namedpart_append_gensnode(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, typedecl_statements.Character, attrs=attrs)

        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_DECL_PART, '')

        part, index = get_part_index(node)

        namedpart_create_subpart(node.kgen_parent, BEFORE_CALLSITE, EXEC_PART, index=index)

        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'MASTER', style='openmp')

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'mpi_comm_rank', 'items': [getinfo('mpi_comm'), 'kgen_mpi_rank', 'kgen_ierr']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'expr': 'kgen_ierr /= 0'}
            iferrobj = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['"MPI ERROR"']}
            part_append_gensnode(iferrobj, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'mpi_comm_size', 'items': [getinfo('mpi_comm'), 'kgen_mpi_size', 'kgen_ierr']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'expr': 'kgen_ierr /= 0'}
            iferr = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['"MPI ERROR"']}
            part_append_gensnode(iferr, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'variable': 'kgen_cur_rank', 'sign': '=', 'expr': '0'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'loopcontrol': 'WHILE( kgen_cur_rank < kgen_mpi_size )'}
            dorank = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Do, attrs=attrs)

            attrs = {'expr': 'ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank'}
            ifrank = part_append_gensnode(dorank, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'kgen_cur_rank', 'sign': '=', 'expr': 'kgen_cur_rank + 1'}
            part_append_gensnode(dorank, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['kgen_mpi_rank'], 'specs': ['kgen_mpi_rank_conv', '*']}
            part_append_gensnode(ifrank, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'expr': 'ANY(kgen_counter == kgen_counter_at)'}
            ifcnt = part_append_gensnode(ifrank, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['kgen_counter'], 'specs': ['kgen_counter_conv', '*']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': \
                '"%s/%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
                (geninfo('kernel_path'), geninfo('kernel_name'))}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Assignment, attrs=attrs)

        else:
            attrs = {'expr': 'ANY(kgen_counter == kgen_counter_at)'}
            ifcnt = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['kgen_counter'], 'specs': ['kgen_counter_conv', '*']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': \
                '"%s/%s." // TRIM(ADJUSTL(kgen_counter_conv))'%(getinfo('kernel_path'), getinfo('kernel_name'))}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_gensnode(ifcnt, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="REPLACE"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"']}
        part_append_gensnode(ifcnt, EXEC_PART, statements.Open, attrs=attrs)


        attrs = {'expr': 'kgen_ierr /= 0'}
        iferr = part_append_gensnode(ifcnt, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': 'kgen_error_stop', 'items': ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))']}
        part_append_gensnode(iferr, EXEC_PART, statements.Call, attrs=attrs)


        if getinfo('is_mpi_app'):
            attrs = {'designator': 'kgen_print_mpirank_counter', 'items': ['kgen_mpi_rank', 'kgen_counter']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': 'kgen_print_counter', 'items': ['kgen_counter']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Call, attrs=attrs)

        namedpart_create_subpart(ifcnt, PARENTBLOCK_WRITE_IN_ARGS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_ARGS, '')
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_ARGS, 'argument input variables')

        namedpart_create_subpart(ifcnt, PARENTBLOCK_WRITE_IN_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_EXTERNS, 'extern input variables')

        namedpart_create_subpart(ifcnt, PARENTBLOCK_WRITE_IN_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_IN_LOCALS, 'local input variables')

        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END MASTER', style='openmp')
        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, '')

        namedpart_create_subpart(node.kgen_parent, AFTER_CALLSITE, EXEC_PART, index=index+2)

        namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, '')
        namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, 'MASTER', style='openmp')

        if getinfo('is_mpi_app'):
            attrs = {'variable': 'kgen_cur_rank', 'sign': '=', 'expr': '0'}
            namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'loopcontrol': 'WHILE( kgen_cur_rank < kgen_mpi_size )'}
            dorank = namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Do, attrs=attrs)

            attrs = {'expr': 'ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank'}
            ifrank = part_append_gensnode(dorank, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'expr': 'ANY(kgen_counter == kgen_counter_at)'}
            ifcnt = part_append_gensnode(ifrank, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['"KGEN writes output state variables at count = "', 'kgen_counter', '" on mpirank = "', 'kgen_mpi_rank']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Write, attrs=attrs)

        else:
            attrs = {'expr': 'ANY(kgen_counter == kgen_counter_at)'}
            ifcnt = namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, block_statements.IfThen, attrs=attrs)

        namedpart_create_subpart(ifcnt, PARENTBLOCK_WRITE_OUT_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_OUT_EXTERNS, 'extern output variables')

        namedpart_create_subpart(ifcnt, PARENTBLOCK_WRITE_OUT_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, PARENTBLOCK_WRITE_OUT_LOCALS, 'local output variables')

        attrs = {'specs': [ 'kgen_unit' ]}
        part_append_gensnode(ifcnt, EXEC_PART, statements.Endfile, attrs=attrs)

        if getinfo('is_mpi_app'):
            pass
#            endfileobj = ifcntobj.create_endfile(insert_in=self.insert_in_output_local_state)
#            endfileobj.set_attr('file_unit', 'kgen_unit')
#
#            callsleepobj = ifcntobj.create_callstmt(insert_in=self.insert_in_output_local_state)
#            callsleepobj.set_attr('name', 'sleep')
#            callsleepobj.set_attr('args', ['1'])
#
#            closeobj = ifcntobj.create_close(insert_in=self.insert_in_output_local_state)
#            closeobj.set_attr('close_specs', ['UNIT=kgen_unit'])
#
#            endifcntobj = ifcntobj.create_endobj(insert_in=self.insert_in_output_local_state) 
#            endifcntobj.set_attr('blockname', 'IF')
#
#            endifrankobj = ifrankobj.create_endobj(insert_in=self.insert_in_output_local_state) 
#            endifrankobj.set_attr('blockname', 'IF')
#
#            incobj = dorankobj.create_assignstmt(insert_in=self.insert_in_output_local_state)
#            incobj.set_attr('lhs', 'kgen_cur_rank')
#            incobj.set_attr('rhs', 'kgen_cur_rank + 1')
#            
#            enddorankobj = dorankobj.create_endobj(insert_in=self.insert_in_output_local_state) 
#            enddorankobj.set_attr('blockname', 'DO')
#
#            ifmaxcobj = self.create_ifthen(insert_in=self.insert_in_output_local_state)
#            ifmaxcobj.set_attr('expr', 'kgen_counter > maxval(kgen_counter_at)')
#
#            callsleepobj = ifmaxcobj.create_callstmt(insert_in=self.insert_in_output_local_state)
#            callsleepobj.set_attr('name', 'sleep')
#            callsleepobj.set_attr('args', ['1'])
#
#            wstopobj = ifmaxcobj.create_write(insert_in=self.insert_in_output_local_state)
#            wstopobj.set_attr('item_list', ['"All state data is collected.  Stopping program..."'])
#
#            callmpiaobj = ifmaxcobj.create_callstmt(insert_in=self.insert_in_output_local_state)
#            callmpiaobj.set_attr('name', 'mpi_abort')
#            callmpiaobj.set_attr('args', [Config.mpi['comm'], '1', 'kgen_ierr'])
#
#            elsemaxcobj = ifmaxcobj.create_else(insert_in=self.insert_in_output_local_state)
#
#            wcntobj = ifmaxcobj.create_write(insert_in=self.insert_in_output_local_state)
#            wcntobj.set_attr('item_list', ['"kgen_counter = "', 'kgen_counter', '" at rank "', 'kgen_mpi_rank'])
#           
#            endifmaxcobj = ifmaxcobj.create_endobj(insert_in=self.insert_in_output_local_state)
#            endifmaxcobj.set_attr('blockname', 'IF') 

        else:
            attrs = {'designator': 'sleep', 'items': ['1']}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'specs': [ 'kgen_unit' ]}
            part_append_gensnode(ifcnt, EXEC_PART, statements.Close, attrs=attrs)

            attrs = {'expr': 'kgen_counter > maxval(kgen_counter_at)'}
            ifmax = namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'sleep', 'items': ['1']}
            part_append_gensnode(ifmax, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'items': ['"All state data is collected.  Stopping program..."']}
            part_append_gensnode(ifmax, EXEC_PART, statements.Write, attrs=attrs)

            part_append_gensnode(ifmax, EXEC_PART, statements.Stop)

            part_append_gensnode(ifmax, EXEC_PART, statements.Else)

            attrs = {'items': ['"kgen_counter = "', 'kgen_counter']}
            part_append_gensnode(ifmax, EXEC_PART, statements.Write, attrs=attrs)


        attrs = {'variable': 'kgen_counter', 'sign': '=', 'expr': 'kgen_counter + 1'}
        namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

        namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, 'END MASTER', style='openmp')

