
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

BEFORE_CALLSITE = 'before_callsite'
AFTER_CALLSITE = 'after_callsite'

state_gencore_contains = []
WRITE_FORMAT = '"(2I5)"'

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
            
        if getinfo('is_mpi_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_mpi_rank_conv'], 'selector':('16', None)}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_unit=-1', 'kgen_counter=0']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_counter_conv'], 'selector':('16', None)}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

    def function_update(self, node):

        # ensure contains
        checks = lambda n: n.kgen_match_class==statements.Contains
        if not node in state_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
            part_append_comment(node, CONTAINS_PART, '')
            part_append_gensnode(node, CONTAINS_PART, statements.Contains)
            part_append_comment(node, CONTAINS_PART, '')
            state_gencore_contains.append(node)

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
        part_append_gensnode(docnt, EXEC_PART, statements.Inquire, attrs=attrs)

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

            attrs = {'items': ['"KGEN writes input state variables at count = "', 'counter', '" on mpirank = "', 'rank']}
            part_append_gensnode(mpisubr, EXEC_PART, statements.Write, attrs=attrs)

        pass

    def add_timing_begin(self, node):

        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'MASTER', style='openmp')

        attrs = {'variable': 'kgen_counter', 'sign': '=', 'expr': 'kgen_counter + 1'}
        namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'mpi_comm_rank', 'items': [getinfo('mpi_comm'), 'kgen_mpi_rank', 'kgen_ierr']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'items': ['kgen_mpi_rank'], 'specs': ['kgen_mpi_rank_conv', '*']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Write, attrs=attrs)

        attrs = {'expr': 'kgen_unit .lt. 0'}
        ifobj = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_gensnode(ifobj, EXEC_PART, statements.Assignment, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"invocations." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'}
            part_append_gensnode(ifobj, EXEC_PART, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"invocations"'}
            part_append_gensnode(ifobj, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="REPLACE"', 'ACCESS="SEQUENTIAL"', \
            'FORM="FORMATTED"', 'ACTION="WRITE"', 'IOSTAT=kgen_ierr']}
        part_append_gensnode(ifobj, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr /= 0'}
        iferr = part_append_gensnode(ifobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': 'kgen_error_stop', 'items': ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))']}
        part_append_gensnode(iferr, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'items': ['kgen_counter', '0'], 'specs': ['UNIT = kgen_unit', 'FMT=%s'%WRITE_FORMAT]}
        namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Write, attrs=attrs)

        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END MASTER', style='openmp')

    def add_timing_end(self, node):

        namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, 'MASTER', style='openmp')

        attrs = {'items': ['kgen_counter', '0'], 'specs': ['UNIT = kgen_unit', 'FMT=%s'%WRITE_FORMAT]}
        namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Write, attrs=attrs)

        namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, 'END MASTER', style='openmp')

    def add_timing_block(self, node):
        self.add_timing_begin(node)
        self.add_timing_end(node)

