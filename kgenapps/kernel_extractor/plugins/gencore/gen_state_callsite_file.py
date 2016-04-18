# gen_write_callsite_file.py

import base_classes
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import STATE_PBLOCK_USE_PART, STATE_PBLOCK_DECL_PART, STATE_PBLOCK_EXEC_PART, \
    STATE_PBLOCK_CONTAINS_PART, STATE_PBLOCK_SUBP_PART, STATE_PBLOCK_WRITE_IN_EXTERNS, STATE_PBLOCK_WRITE_IN_LOCALS, \
    STATE_PBLOCK_WRITE_OUT_EXTERNS, STATE_PBLOCK_WRITE_OUT_LOCALS, STATE_TBLOCK_USE_PART, STATE_TBLOCK_DECL_PART, \
    STATE_TBLOCK_CONTAINS_PART, STATE_TBLOCK_SUBP_PART, STATE_PBLOCK_WRITE_IN_ARGS, state_gencore_contains

BEFORE_CALLSITE = 'before_callsite'
AFTER_CALLSITE = 'after_callsite'

class Gen_S_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg
        callsite_stmts = getinfo('callsite_stmts')

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            callsite_stmts[0], None, self.create_callsite_parts1)

#        if isinstance(callsite_stmts[-1], base_classes.EndStatement):
#            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#                callsite_stmts[-1].parent, None, self.create_callsite_parts2)
#        else:
#            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#                callsite_stmts[-1], None, self.create_callsite_parts2)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.create_parentblock_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.create_topblock_parts)


    def create_parentblock_parts(self, node):

        namedpart_link_part(node, STATE_PBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, STATE_PBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, STATE_PBLOCK_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, STATE_PBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, STATE_PBLOCK_SUBP_PART, SUBP_PART)

        attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, statements.Use, attrs=attrs)

        # ensure contains
        checks = lambda n: n.kgen_match_class==statements.Contains
        if not node in state_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
            part_append_comment(node, CONTAINS_PART, '')
            part_append_gensnode(node, CONTAINS_PART, statements.Contains)
            part_append_comment(node, CONTAINS_PART, '')
            state_gencore_contains.append(node)

        #SUBROUTINE kgen_init_vars
        attrs = {'name': 'kgen_init_vars', 'args': ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'invoke_e', 'msize', \
            'osize', 'lockpath', 'last_invoke', 'isstop']}
        initvarsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': \
            ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'invoke_e', 'msize', 'osize'], }
        part_append_gensnode(initvarsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(OUT)', 'DIMENSION(0:osize-1)'], 'entity_decls': ['last_invoke']}
        part_append_gensnode(initvarsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)', 'DIMENSION(0:msize-1,0:osize-1)'], 'entity_decls': ['isstop']}
        part_append_gensnode(initvarsubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(OUT)', 'DIMENSION(0:msize-1,0:osize-1)'], \
            'entity_decls': ['lockpath'], 'selector':('1024', None)}
        part_append_gensnode(initvarsubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['mpi_idx', 'openmp_idx']}
        part_append_gensnode(initvarsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'loopcontrol': 'mpi_idx=mpi_s,mpi_e'}
        dompi = part_append_gensnode(initvarsubr, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'loopcontrol': 'openmp_idx=omp_s,omp_e'}
        doopenmp = part_append_gensnode(dompi, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'specs': [ 'lockpath(mpi_idx, openmp_idx)', 'FMT="(A,I0,A,I0)"'], \
            'items': ['"%s/done."'%getinfo('kernel_path'), 'mpi_idx', '"."', 'openmp_idx']}
        part_append_gensnode(doopenmp, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'isstop(mpi_idx, openmp_idx)', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(doopenmp, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'last_invoke(openmp_idx)', 'sign': '=', 'expr': 'invoke_e'}
        part_append_gensnode(doopenmp, EXEC_PART, statements.Assignment, attrs=attrs)

        #SUBROUTINE kgen_check_save
        attrs = {'name': 'kgen_check_save', 'args': ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'invoke_s', 'invoke_e', 'mymid', \
            'myoid', 'osize', 'invoke', 'last_invoke', 'issave', 'islast']}
        checksavesubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': \
            ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'invoke_s', 'invoke_e', 'osize', 'mymid', 'myoid'], }
        part_append_gensnode(checksavesubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)', 'DIMENSION(0:osize-1)'], 'entity_decls': ['invoke', 'last_invoke']}
        part_append_gensnode(checksavesubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)', 'DIMENSION(0:osize-1)'], 'entity_decls': ['issave', 'islast']}
        part_append_gensnode(checksavesubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'expr': '(mymid .GE. mpi_s) .AND. (mymid .LE. mpi_e)'}
        ifmpi = part_append_gensnode(checksavesubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'expr': '(myoid .GE. omp_s) .AND. (myoid .LE. omp_e)'}
        ifopenmp = part_append_gensnode(ifmpi, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'expr': '(invoke(myoid) .GE. invoke_s) .AND. (invoke(myoid) .LE. invoke_e)'}
        ifinvoke = part_append_gensnode(ifopenmp, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'issave(myoid)', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(ifinvoke, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'invoke(myoid) .GE. last_invoke(myoid)'}
        iflast = part_append_gensnode(ifopenmp, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'islast(myoid)', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(iflast, EXEC_PART, statements.Assignment, attrs=attrs)

        #SUBROUTINE kgen_check_stop
        attrs = {'name': 'kgen_check_stop', 'args': ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'msize', \
            'osize', 'myoid', 'lockpath', 'isexist', 'isstop']}
        checkstopsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': \
            ['mpi_s', 'mpi_e', 'omp_s', 'omp_e', 'msize', 'osize', 'myoid'] }
        part_append_gensnode(checkstopsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)'], 'entity_decls': ['isexist'] }
        part_append_gensnode(checkstopsubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'DIMENSION(0:msize-1,0:osize-1)'], \
            'entity_decls': ['lockpath'], 'selector':('1024', None)}
        part_append_gensnode(checkstopsubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)', 'DIMENSION(0:msize-1,0:osize-1)'], 'entity_decls': ['isstop']}
        part_append_gensnode(checkstopsubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['mpi_idx', 'openmp_idx']}
        part_append_gensnode(checkstopsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'loopcontrol': 'mpi_idx=mpi_s,mpi_e'}
        dompi = part_append_gensnode(checkstopsubr, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'loopcontrol': 'openmp_idx=omp_s,omp_e'}
        doopenmp = part_append_gensnode(dompi, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'expr': '.NOT. isstop(mpi_idx, openmp_idx)'}
        ifstop = part_append_gensnode(doopenmp, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['FILE=TRIM(ADJUSTL(lockpath(mpi_idx, openmp_idx))), EXIST=isexist']}
        part_append_gensnode(ifstop, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'variable': 'isstop(mpi_idx, openmp_idx)', 'sign': '=', 'expr': 'isexist'}
        part_append_gensnode(ifstop, EXEC_PART, statements.Assignment, attrs=attrs)

        #SUBROUTINE kgen_error_stop( ierr, errmsg )
        attrs = {'name': 'kgen_error_stop', 'args': ['ierr', 'errmsg']}
        stopsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['ierr']}
        part_append_gensnode(stopsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['errmsg'], 'selector':('*', None)}
        part_append_gensnode(stopsubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
            part_append_gensnode(stopsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'expr': 'ierr /= 0'}
        iferr = part_append_gensnode(stopsubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['errmsg']}
        part_append_gensnode(iferr, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'mpi_abort', 'items': [getinfo('mpi_comm'), '0', 'kgen_ierr']}
            part_append_gensnode(iferr, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'code': '1'}
            part_append_gensnode(iferr, EXEC_PART, statements.Stop, attrs=attrs)

        #SUBROUTINE kgen_print_counter( counter )
        attrs = {'name': 'kgen_print_counter', 'args': ['counter']}
        cntsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['counter']}
        part_append_gensnode(cntsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': ['"KGEN writes input state variables at count = "', 'counter']}
        part_append_gensnode(cntsubr, EXEC_PART, statements.Write, attrs=attrs)


    def create_topblock_parts(self, node):
        node.kgen_stmt.top.used4genstate = True

        namedpart_link_part(node, STATE_TBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, STATE_TBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, STATE_TBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, STATE_TBLOCK_SUBP_PART, SUBP_PART)

    def create_callsite_parts1(self, node):

        # declaration part
        if getinfo('is_mpi_app'):
            mpi_use_stmts = getinfo('mpi_use')
            if mpi_use_stmts and len(mpi_use_stmts)>0:
                for mod_name, use_names in mpi_use_stmts:
                    attrs = {'name':mod_name, 'isonly': True, 'items':use_names}
                    namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, statements.Use, attrs=attrs)

        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, 'kgen variables')

        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE', 'ALLOCATABLE', 'DIMENSION(:)' ], \
            'entity_decls': ['kgen_invoke', 'kgen_last_invoke']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'SAVE', 'ALLOCATABLE', 'DIMENSION(:)' ], \
            'entity_decls': ['kgen_islast', 'kgen_issave']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'attrspec': [ 'SAVE', 'ALLOCATABLE', 'DIMENSION(:,:)' ], \
            'entity_decls': ['kgen_filepath', 'kgen_lockpath'], 'selector':('1024', None)}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'SAVE', 'ALLOCATABLE', 'DIMENSION(:,:)' ], 'entity_decls': ['kgen_isstop']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'attrspec': [ 'SAVE' ], 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Real, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE' ], 'entity_decls': ['kgen_mymid', 'kgen_msize', 'kgen_osize', \
            'kgen_unit', 'kgen_stopunit', 'kgen_ierr']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_NUM_THREADS', 'OMP_GET_THREAD_NUM']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue', 'kgen_isexist']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

        index, partname, part = get_part_index(node)

        namedpart_create_subpart(node.kgen_parent, BEFORE_CALLSITE, EXEC_PART, index=index)

        # initialize variables
        namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'START OF KGEN REGION')

        if getinfo('is_openmp_app'):
            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'CRITICAL (kgen_init)', style='openmp')

        attrs = {'expr': '.NOT. ALLOCATED(kgen_isstop)'}
        ifinit = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'variable': 'kgen_osize', 'sign': '=', 'expr': 'OMP_GET_NUM_THREADS()'}
            part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_osize', 'sign': '=', 'expr': '1'}
            part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'items': ['kgen_islast(0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['kgen_issave(0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['kgen_invoke(0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['kgen_last_invoke(0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'variable': 'kgen_islast(:)', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_issave(:)', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_invoke(:)', 'sign': '=', 'expr': '0'}
        part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_last_invoke(:)', 'sign': '=', 'expr': '0'}
        part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'mpi_comm_rank', 'items': [getinfo('mpi_comm'), 'kgen_mymid', 'kgen_ierr']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['kgen_ierr', '"mpi_comm_rank is failed"']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)
            
            attrs = {'designator': 'mpi_comm_size', 'items': [getinfo('mpi_comm'), 'kgen_msize', 'kgen_ierr']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['kgen_ierr', '"mpi_comm_size is failed"']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

        else:
            attrs = {'variable': 'kgen_mymid', 'sign': '=', 'expr': '0'}
            part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_msize', 'sign': '=', 'expr': '1'}
            part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)


        attrs = {'items': ['kgen_filepath(0:kgen_msize-1, 0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['kgen_lockpath(0:kgen_msize-1, 0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': ['kgen_isstop(0:kgen_msize-1, 0:kgen_osize-1)']}
        part_append_gensnode(ifinit, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'variable': 'kgen_isstop(:,:)', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(ifinit, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'designator': 'SYSTEM', 'items': ['"rm -f %s/state_file.lst 2>/dev/null"'%getinfo('kernel_path')]}
        part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'designator': 'SYSTEM', 'items': ['"rm -f %s/done.*.* 2>/dev/null"'%getinfo('kernel_path')]}
        part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

        for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
            attrs = {'designator': 'kgen_init_vars', 'items': ['INT(%s)'%mpi_s.replace('e', '(kgen_msize-1)'), \
                'INT(%s)'%mpi_e.replace('e', '(kgen_msize-1)'), 'INT(%s)'%openmp_s.replace('e', '(kgen_osize-1)'), \
                'INT(%s)'%openmp_e.replace('e', '(kgen_osize-1)'), 'INT(%s)'%invoke_e, 'kgen_msize', 'kgen_osize', 'kgen_lockpath', \
                'kgen_last_invoke', 'kgen_isstop']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

        # check save
        if getinfo('is_openmp_app'):
            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END CRITICAL (kgen_init)', style='openmp')

            attrs = {'variable': 'kgen_issave(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_islast(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
                attrs = {'designator': 'kgen_check_save', 'items': ['INT(%s)'%mpi_s.replace('e', '(kgen_msize-1)'), \
                    'INT(%s)'%mpi_e.replace('e', '(kgen_msize-1)'), 'INT(%s)'%openmp_s.replace('e', '(kgen_osize-1)'), \
                    'INT(%s)'%openmp_e.replace('e', '(kgen_osize-1)'), 'INT(%s)'%invoke_s, 'INT(%s)'%invoke_e, \
                    'kgen_mymid', 'OMP_GET_THREAD_NUM()', 'kgen_osize', 'kgen_invoke', 'kgen_last_invoke', \
                    'kgen_issave', 'kgen_islast']}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'expr': 'kgen_issave(OMP_GET_THREAD_NUM())'}
            ifsave = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            part_append_comment(ifsave, EXEC_PART, 'CRITICAL (kgen_kernel)', style='openmp')

            l = [ 'kgen_mymid', '"."', 'OMP_GET_THREAD_NUM()', '"."', 'kgen_invoke(OMP_GET_THREAD_NUM())']
            attrs = {'specs': ['kgen_filepath(kgen_mymid, OMP_GET_THREAD_NUM())', 'FMT="(A,I0,A,I0,A,I0)"' ], \
                'items': [ '"%s/%s."'%(getinfo('kernel_path'), getinfo('kernel_name')) ] + l}
            part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['NEWUNIT=kgen_unit', 'FILE=TRIM(ADJUSTL(kgen_filepath(kgen_mymid, OMP_GET_THREAD_NUM())))', \
                'STATUS="REPLACE"', 'ACCESS="STREAM"', 'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['kgen_ierr', '"File open error: " // TRIM(ADJUSTL(kgen_filepath(kgen_mymid, OMP_GET_THREAD_NUM())))']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_issave(0)', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_islast(0)', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)


            for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
                attrs = {'designator': 'kgen_check_save', 'items': ['INT(%s)'%mpi_s.replace('e', '(kgen_msize-1)'), \
                    'INT(%s)'%mpi_e.replace('e', '(kgen_msize-1)'), 'INT(%s)'%openmp_s.replace('e', '(kgen_osize-1)'), \
                    'INT(%s)'%openmp_e.replace('e', '(kgen_osize-1)'), 'INT(%s)'%invoke_s, 'INT(%s)'%invoke_e, \
                    'kgen_mymid', '0', 'kgen_osize', 'kgen_invoke', 'kgen_last_invoke', \
                    'kgen_issave', 'kgen_islast']}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'expr': 'kgen_issave(0)'}
            ifsave = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            l = [ 'kgen_mymid', '"."', '0', '"."', 'kgen_invoke(0)']
            attrs = {'specs': ['kgen_filepath(kgen_mymid, 0)', 'FMT="(A,I0,A,I0,A,I0)"' ], \
                'items': [ '"%s/%s."'%(getinfo('kernel_path'), getinfo('kernel_name')) ] + l}
            part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['NEWUNIT=kgen_unit', 'FILE=TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0)))', \
                'STATUS="REPLACE"', 'ACCESS="STREAM"', 'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'designator': 'kgen_error_stop', 'items': ['kgen_ierr', '"File open error: " // TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0)))']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Call, attrs=attrs)

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_ARGS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_ARGS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_ARGS, 'argument input variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, 'extern input variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_LOCALS, 'local input variables')

        for stmt in getinfo('callsite_stmts'):
            part_append_node(ifsave, EXEC_PART, stmt.genspair)
        
        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_OUT_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_EXTERNS, 'extern output variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_OUT_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, 'local output variables')

        attrs = {'specs': [ 'UNIT=kgen_unit' ]}
        part_append_gensnode(ifsave, EXEC_PART, statements.Close, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'items': ['"Collected Kernel Input/Ouput state from: "', 'kgen_mymid', 'OMP_GET_THREAD_NUM()', 'kgen_invoke(OMP_GET_THREAD_NUM())']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

            part_append_comment(ifsave, EXEC_PART, 'END CRITICAL (kgen_kernel)', style='openmp')
        else:
            attrs = {'items': ['"Collected Kernel Input/Ouput state from: "', 'kgen_mymid', '0', 'kgen_invoke(0)']}
            part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)


        part_append_gensnode(ifsave, EXEC_PART, statements.Else) 

        if getinfo('is_openmp_app'):
            part_append_comment(ifsave, EXEC_PART, 'CRITICAL (kgen_kernel)', style='openmp')

        for stmt in getinfo('callsite_stmts'):
            attrs = {}
            for key, value in stmt.__dict__.items():
                if not key.startswith('_'):
                    attrs[key] = value
            part_append_gensnode(ifsave, EXEC_PART, stmt.__class__, attrs=attrs) 

        if getinfo('is_openmp_app'):
            part_append_comment(ifsave, EXEC_PART, 'END CRITICAL (kgen_kernel)', style='openmp')

        # check stop
        if getinfo('is_openmp_app'):
            attrs = {'expr': 'kgen_islast(OMP_GET_THREAD_NUM())'}
            ifstop = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            part_append_comment(ifstop, EXEC_PART, 'CRITICAL (kgen_fini)', style='openmp')

            attrs = {'specs': ['NEWUNIT=kgen_stopunit', 'FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, OMP_GET_THREAD_NUM())))', \
                'STATUS="NEW"', 'IOSTAT=kgen_ierr']}
            part_append_gensnode(ifstop, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'expr': 'kgen_ierr == 0'}
            ifopen = part_append_gensnode(ifstop, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': [ 'UNIT=kgen_stopunit', 'STATUS="KEEP"' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Close, attrs=attrs)
           
#            attrs = {'specs': ['FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, OMP_GET_THREAD_NUM()))), EXIST=kgen_isexist']}
#            part_append_gensnode(ifstop, EXEC_PART, statements.Inquire, attrs=attrs)
#
#            attrs = {'loopcontrol': 'WHILE( .NOT. kgen_isexist)'}
#            doexist = part_append_gensnode(ifstop, EXEC_PART, block_statements.Do, attrs=attrs)
#
#            attrs = {'specs': ['NEWUNIT=kgen_stopunit', 'FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, OMP_GET_THREAD_NUM())))', \
#                'STATUS="REPLACE"', 'IOSTAT=kgen_ierr']}
#            part_append_gensnode(doexist, EXEC_PART, statements.Open, attrs=attrs)
#
#            attrs = {'specs': ['FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, OMP_GET_THREAD_NUM()))), EXIST=kgen_isexist']}
#            part_append_gensnode(doexist, EXEC_PART, statements.Inquire, attrs=attrs)
#
#            attrs = {'specs': [ 'UNIT=kgen_stopunit', 'STATUS="KEEP"' ]}
#            part_append_gensnode(ifstop, EXEC_PART, statements.Close, attrs=attrs)
#
            for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
                attrs = {'designator': 'kgen_check_stop', 'items': ['INT(%s)'%mpi_s.replace('e', '(kgen_msize-1)'), \
                    'INT(%s)'%mpi_e.replace('e', '(kgen_msize-1)'), 'INT(%s)'%openmp_s.replace('e', '(kgen_osize-1)'), \
                    'INT(%s)'%openmp_e.replace('e', '(kgen_osize-1)'), 'kgen_msize', 'kgen_osize', \
                    'OMP_GET_THREAD_NUM()', 'kgen_lockpath', 'kgen_isexist', 'kgen_isstop']}
                part_append_gensnode(ifstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'expr': 'ALL(kgen_isstop)'}
            ifallstop = part_append_gensnode(ifstop, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'SYSTEM', 'items': ['"cd %s; ls -1 2>/dev/null %s.*.*.* > state_file.lst"'% \
                (getinfo('kernel_path'), getinfo('kernel_name'))]}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'SYSTEM', 'items': ['"rm -f %s/done.*.* 2>/dev/null"'%getinfo('kernel_path')]}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'items': ['"Stopping application..."']}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Write, attrs=attrs)

            if getinfo('is_mpi_app'):
                attrs = {'designator': 'mpi_abort', 'items': [getinfo('mpi_comm'), '0', 'kgen_ierr']}
                part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)
            else:
                attrs = {'code': '0'}
                part_append_gensnode(ifallstop, EXEC_PART, statements.Stop, attrs=attrs)

            part_append_comment(ifstop, EXEC_PART, 'END CRITICAL (kgen_fini)', style='openmp')

            attrs = {'variable': 'kgen_invoke(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_invoke(OMP_GET_THREAD_NUM()) + 1'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END OF KGEN REGION')
        else:
            attrs = {'expr': 'kgen_islast(0)'}
            ifstop = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': ['NEWUNIT=kgen_stopunit', 'FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0)))', \
                'STATUS="NEW"', 'IOSTAT=kgen_ierr']}
            part_append_gensnode(ifstop, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'expr': 'kgen_ierr == 0'}
            ifopen = part_append_gensnode(ifstop, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': [ 'UNIT=kgen_stopunit', 'STATUS="KEEP"' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Close, attrs=attrs)
 
#            attrs = {'specs': ['FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0))), EXIST=kgen_isexist']}
#            part_append_gensnode(ifstop, EXEC_PART, statements.Inquire, attrs=attrs)
#
#            attrs = {'loopcontrol': 'WHILE( .NOT. kgen_isexist )'}
#            doexist = part_append_gensnode(ifstop, EXEC_PART, block_statements.Do, attrs=attrs)
#
#            attrs = {'specs': ['NEWUNIT=kgen_stopunit', 'FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0)))', \
#                'STATUS="REPLACE"', 'IOSTAT=kgen_ierr']}
#            part_append_gensnode(doexist, EXEC_PART, statements.Open, attrs=attrs)
#
#            attrs = {'specs': ['FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0))), EXIST=kgen_isexist']}
#            part_append_gensnode(doexist, EXEC_PART, statements.Inquire, attrs=attrs)
#
#            attrs = {'specs': [ 'UNIT=kgen_stopunit', 'STATUS="KEEP"' ]}
#            part_append_gensnode(ifstop, EXEC_PART, statements.Close, attrs=attrs)

            for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
                attrs = {'designator': 'kgen_check_stop', 'items': ['INT(%s)'%mpi_s.replace('e', '(kgen_msize-1)'), \
                    'INT(%s)'%mpi_e.replace('e', '(kgen_msize-1)'), 'INT(%s)'%openmp_s.replace('e', '(kgen_osize-1)'), \
                    'INT(%s)'%openmp_e.replace('e', '(kgen_osize-1)'), 'kgen_msize', 'kgen_osize', \
                    '0', 'kgen_lockpath', 'kgen_isexist', 'kgen_isstop']}
                part_append_gensnode(ifstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'expr': 'ALL(kgen_isstop)'}
            ifallstop = part_append_gensnode(ifstop, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'SYSTEM', 'items': ['"cd %s; ls -1 2>/dev/null %s.*.*.* > state_file.lst"'% \
                (getinfo('kernel_path'), getinfo('kernel_name'))]}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'SYSTEM', 'items': ['"rm -f %s/done.*.* 2>/dev/null"'%getinfo('kernel_path')]}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'items': ['"Stopping application..."']}
            part_append_gensnode(ifallstop, EXEC_PART, statements.Write, attrs=attrs)

            if getinfo('is_mpi_app'):
                attrs = {'designator': 'mpi_abort', 'items': [getinfo('mpi_comm'), '0', 'kgen_ierr']}
                part_append_gensnode(ifallstop, EXEC_PART, statements.Call, attrs=attrs)
            else:
                attrs = {'code': '0'}
                part_append_gensnode(ifallstop, EXEC_PART, statements.Stop, attrs=attrs)

            attrs = {'variable': 'kgen_invoke(0)', 'sign': '=', 'expr': 'kgen_invoke(0) + 1'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END OF KGEN REGION')

