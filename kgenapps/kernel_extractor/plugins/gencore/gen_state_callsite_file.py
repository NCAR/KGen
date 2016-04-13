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

        if isinstance(callsite_stmts[-1], base_classes.EndStatement):
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
                callsite_stmts[-1].parent, None, self.create_callsite_parts2)
        else:
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
                callsite_stmts[-1], None, self.create_callsite_parts2)

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

        attrs = {'type_spec': 'REAL', 'entity_decls': ['r']}
        part_append_gensnode(unitsubr, DECL_PART, typedecl_statements.Real, attrs=attrs)

        part_append_comment(unitsubr, DECL_PART, '')

        attrs = {'designator': 'RANDOM_SEED'}
        part_append_gensnode(unitsubr, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'variable': 'new_unit', 'sign': '=', 'expr': '-1'}
        part_append_gensnode(unitsubr, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'loopcontrol': 'counter=UNIT_MIN, UNIT_MAX'}
        docnt = part_append_gensnode(unitsubr, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'designator': 'RANDOM_NUMBER', 'items': ['r']}
        part_append_gensnode(docnt, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'specs': ['UNIT=INT(r*UNIT_MAX+UNIT_MIN)', 'OPENED=is_opened']}
        part_append_gensnode(docnt, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. is_opened'}
        ifopen = part_append_gensnode(docnt, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'new_unit', 'sign': '=', 'expr': 'INT(r*UNIT_MAX+UNIT_MIN)'}
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
        part_append_gensnode(stopsubr, EXEC_PART, statements.Stop, attrs=attrs)

        #SUBROUTINE kgen_print_counter( counter )
        attrs = {'name': 'kgen_print_counter', 'args': ['counter']}
        cntsubr = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['counter']}
        part_append_gensnode(cntsubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': ['"KGEN writes input state variables at count = "', 'counter']}
        part_append_gensnode(cntsubr, EXEC_PART, statements.Write, attrs=attrs)


        #SUBROUTINE kgen_check_save(mpi_rank, mpi_size, openmp_num, openmp_size, invoke_num)
        attrs = {'name': 'kgen_check_save', 'prefix': 'LOGICAL', \
            'args': ['mpi_rank', 'mpi_size', 'openmp_num', 'openmp_size', 'invoke_num', 'is_last_invoke']}
        checksubr = part_append_gensnode(node, SUBP_PART, block_statements.Function, attrs=attrs)
        part_append_comment(node, SUBP_PART, '')

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': \
            ['mpi_rank', 'mpi_size', 'openmp_num', 'openmp_size', 'invoke_num']}
        part_append_gensnode(checksubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)'], 'entity_decls': ['is_last_invoke']}
        part_append_gensnode(checksubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
        part_append_gensnode(checksubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['mpi_s', 'mpi_e', 'openmp_s', 'openmp_e', 'invoke_s', 'invoke_e']}
        part_append_gensnode(checksubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['mpi_is_save', 'openmp_is_save', 'invoke_is_save', 'is_stop = .TRUE.']}
        part_append_gensnode(checksubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        idx = 0
        for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):

            # initialize state save range
            attrs = {'variable': 'mpi_s', 'sign': '=', 'expr': 'INT(%s)'%mpi_s.replace('e', '(mpi_size-1)')}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'mpi_e', 'sign': '=', 'expr': 'INT(%s)'%mpi_e.replace('e', '(mpi_size-1)')}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'openmp_s', 'sign': '=', 'expr': 'INT(%s)'%openmp_s.replace('e', '(openmp_size-1)')}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'openmp_e', 'sign': '=', 'expr': 'INT(%s)'%openmp_e.replace('e', '(openmp_size-1)')}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'invoke_s', 'sign': '=', 'expr': 'INT(%s)'%invoke_s}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'invoke_e', 'sign': '=', 'expr': 'INT(%s)'%invoke_e}
            part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)


            attrs = {'expr': '(mpi_rank .GE. mpi_s) .AND. (mpi_rank .LE. mpi_e)'}
            ifmpi = part_append_gensnode(checksubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'expr': '(openmp_num .GE. openmp_s) .AND. (openmp_num .LE. openmp_e)'}
            ifopenmp = part_append_gensnode(ifmpi, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'expr': '(invoke_num .GE. invoke_s) .AND. (invoke_num .LE. invoke_e)'}
            ifinvoke = part_append_gensnode(ifopenmp, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'kgen_check_save', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifinvoke, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'expr': 'invoke_num .EQ. invoke_e'}
            iflast = part_append_gensnode(ifinvoke, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_last_invoke', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(iflast, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(iflast, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_last_invoke', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(iflast, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifinvoke, EXEC_PART, statements.Return)

            idx += 1

        attrs = {'variable': 'kgen_check_save', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(checksubr, EXEC_PART, statements.Assignment, attrs=attrs)

        # INTERNAL SUBROUTINE kgen_check_stop()
        if getinfo('is_mpi_app') or getinfo('is_openmp_app'):
            args = []
            arrs = []
            for idx, dummy in enumerate(getinfo('invocations')):
                args.append('kgen_check_%d'%idx)
                args.append('lbound_%d'%idx)
                args.append('ubound_%d'%idx)
                arrs.append('kgen_check_%d'%idx)

            attrs = {'name': 'kgen_check_stop', 'prefix': 'LOGICAL', 'args': args}
            checkstop = part_append_gensnode(node, SUBP_PART, block_statements.Function, attrs=attrs)
            part_append_comment(node, SUBP_PART, '')

            for idx, arrname in enumerate(arrs):
                attrs = {'type_spec': 'INTEGER', 'attrspec': ['DIMENSION(2)'], 'entity_decls': ['lbound_%d'%idx, 'ubound_%d'%idx]}
                part_append_gensnode(checkstop, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(INOUT)', 'DIMENSION(lbound_%d(1):ubound_%d(1), lbound_%d(2):ubound_%d(2))'%\
                    (idx, idx, idx, idx)], 'entity_decls': [arrname]}
                part_append_gensnode(checkstop, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['mpi_idx', 'openmp_idx', 'kgen_ierr']}
            part_append_gensnode(checkstop, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
            part_append_gensnode(checkstop, DECL_PART, typedecl_statements.Character, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_true']}
            part_append_gensnode(checkstop, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            for idx, arrname in enumerate(arrs):

                attrs = {'loopcontrol': 'mpi_idx=lbound_%d(1), ubound_%d(1)'%(idx, idx)}
                dompi = part_append_gensnode(checkstop, EXEC_PART, block_statements.Do, attrs=attrs)

                attrs = {'loopcontrol': 'openmp_idx=lbound_%d(2), ubound_%d(2)'%(idx, idx)}
                doopenmp = part_append_gensnode(dompi, EXEC_PART, block_statements.Do, attrs=attrs)

                attrs = {'expr': '%s(mpi_idx, openmp_idx) .GE. 0'%arrname}
                ifnotexist = part_append_gensnode(doopenmp, EXEC_PART, block_statements.IfThen, attrs=attrs)

                # check if the last file exists

                l = [ 'mpi_idx', '"."', 'openmp_idx', '"."', '%s(mpi_idx, openmp_idx)'%arrname ]
                attrs = {'specs': ['UNIT=kgen_filepath', 'FMT="(A,I0,A,I0,A,I0)"' ], 'items': [ '"%s/%s."'%(getinfo('kernel_path'), getinfo('kernel_name')) ] + l}
                part_append_gensnode(ifnotexist, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': ['FILE=TRIM(ADJUSTL(kgen_filepath))', 'EXIST=is_true']}
                part_append_gensnode(ifnotexist, EXEC_PART, statements.Inquire, attrs=attrs)

                attrs = {'expr': 'is_true'}
                ifexist = part_append_gensnode(ifnotexist, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'variable': '%s(mpi_idx, openmp_idx)'%arrname, 'sign': '=', 'expr': '-1'}
                part_append_gensnode(ifexist, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_gensnode(ifexist, EXEC_PART, statements.Else)

                attrs = {'variable': 'kgen_check_stop', 'sign': '=', 'expr': '.FALSE.'}
                part_append_gensnode(ifexist, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_gensnode(ifexist, EXEC_PART, statements.Return)


            attrs = {'variable': 'kgen_check_stop', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(checkstop, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'specs': ['FILE="%s/state_file.lst"'%getinfo('kernel_path'), 'EXIST=is_true']}
            part_append_gensnode(checkstop, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': '.NOT. is_true'}
            ifstate = part_append_gensnode(checkstop, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'SYSTEM', 'items': ['"cd %s; ls -1 %s.*.*.* > state_file.lst"'%(getinfo('kernel_path'), getinfo('kernel_name'))]}
            part_append_gensnode(ifstate, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'designator': 'SLEEP', 'items': ['5']}
            part_append_gensnode(ifstate, EXEC_PART, statements.Call, attrs=attrs)

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

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank', 'kgen_mpi_size']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE', 'DIMENSION(0:199)' ], 'entity_decls': ['kgen_invoke = 0']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'DIMENSION(0:199)' ], 'entity_decls': ['kgen_is_save']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:199)' ], 'entity_decls': ['kgen_unit']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        else:
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE' ], 'entity_decls': ['kgen_invoke = 0']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_is_save']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_unit']}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        for idx in range(len(getinfo('invocations'))):
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE', 'ALLOCATABLE', 'DIMENSION(:,:)' ], 'entity_decls': ['kgen_check_%d'%idx]}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_index']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue', 'is_last_invoke']}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_DECL_PART, typedecl_statements.Real, attrs=attrs)

        index, partname, part = get_part_index(node)

        namedpart_create_subpart(node.kgen_parent, BEFORE_CALLSITE, EXEC_PART, index=index)

        # initialize variables
        if getinfo('is_openmp_app'):
            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'CRITICAL', style='openmp')

            attrs = {'variable': 'kgen_invoke(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_invoke(OMP_GET_THREAD_NUM()) + 1'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_is_save(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_invoke', 'sign': '=', 'expr': 'kgen_invoke + 1'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_is_save', 'sign': '=', 'expr': '.FALSE.'}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'is_last_invoke', 'sign': '=', 'expr': '.FALSE.'}
        namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

        l = []
        if getinfo('is_mpi_app'):

            attrs = {'designator': 'mpi_comm_rank', 'items': [getinfo('mpi_comm'), 'kgen_mpi_rank', 'kgen_ierr']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

            attrs = {'designator': 'mpi_comm_size', 'items': [getinfo('mpi_comm'), 'kgen_mpi_size', 'kgen_ierr']}
            namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)


            if getinfo('is_openmp_app'):
                saveattrs = {'expr': 'kgen_check_save(kgen_mpi_rank, kgen_mpi_size, OMP_GET_THREAD_NUM(), OMP_GET_NUM_THREADS(), kgen_invoke(OMP_GET_THREAD_NUM()), is_last_invoke)'}
                l = [ 'kgen_mpi_rank', '"."', 'OMP_GET_THREAD_NUM()', '"."', 'kgen_invoke(OMP_GET_THREAD_NUM())' ]

            else:
                saveattrs = {'expr': 'kgen_check_save(kgen_mpi_rank, kgen_mpi_size, 0, 1, kgen_invoke, is_last_invoke)'}
                l = [ 'kgen_mpi_rank', '"."', '0', '"."', 'kgen_invoke' ]

        else:
            if getinfo('is_openmp_app'):
                saveattrs = {'expr': 'kgen_check_save(0, 1, OMP_GET_THREAD_NUM(), OMP_GET_NUM_THREADS(), kgen_invoke(OMP_GET_THREAD_NUM()), is_last_invoke)'}
                l = [ '0', '"."', 'OMP_GET_THREAD_NUM()', '"."', 'kgen_invoke(OMP_GET_THREAD_NUM())' ]

                attrs = {'expr': 'kgen_file_handle == -1'}
                iffile = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

                attrs = {'specs': ['UNIT=kgen_file_handle', 'FILE="%s/state.check"'%getinfo('kernel_path'), 'STATUS="REPLACE"', \
                    'ACTION="READWRITE"', 'IOSTAT=kgen_ierr']}
                part_append_gensnode(iffile, EXEC_PART, statements.Open, attrs=attrs)

            else:
                saveattrs = {'expr': 'kgen_check_save(0, 1, 0, 1, kgen_invoke, is_last_invoke)'}
                l = [ '0', '"."', '0', '"."', 'kgen_invoke' ]

        attrs = {'expr': '.NOT. ALLOCATED(kgen_check_0)'}
        ifalloc = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=attrs)

        idx = 0
        for (mpi_s, mpi_e), (openmp_s, openmp_e), (invoke_s, invoke_e) in getinfo('invocations'):
            attrs = {'items': ['kgen_check_%d(%s:%s,%s:%s)'%(idx, 'INT(%s)'%mpi_s.replace('e', '(kgen_mpi_size-1)'), \
                'INT(%s)'%mpi_e.replace('e', '(kgen_mpi_size-1)'), 'INT(%s)'%openmp_s.replace('e', '(OMP_GET_NUM_THREADS()-1)'), \
                'INT(%s)'%openmp_e.replace('e', '(OMP_GET_NUM_THREADS()-1)'))]}
            part_append_gensnode(ifalloc, EXEC_PART, statements.Allocate, attrs=attrs)

            attrs = {'variable': 'kgen_check_%d(:,:)'%idx, 'sign': '=', 'expr': 'INT(%s)'%invoke_e}
            part_append_gensnode(ifalloc, EXEC_PART, statements.Assignment, attrs=attrs)

            idx += 1

        ifsave = namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, block_statements.IfThen, attrs=saveattrs)

        attrs = {'specs': ['UNIT=kgen_filepath', 'FMT="(A,I0,A,I0,A,I0)"' ], 'items': [ '"%s/%s."'%(getinfo('kernel_path'), getinfo('kernel_name')) ] + l}
        part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'variable': 'kgen_unit(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_get_newunit()'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'specs': ['UNIT=kgen_unit(OMP_GET_THREAD_NUM())', 'FILE=TRIM(ADJUSTL(kgen_filepath))', 'STATUS="REPLACE"', 'ACCESS="STREAM"', \
                'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        else:
            attrs = {'specs': ['UNIT=kgen_unit', 'FILE=TRIM(ADJUSTL(kgen_filepath))', 'STATUS="REPLACE"', 'ACCESS="STREAM"', \
                'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_gensnode(ifsave, EXEC_PART, statements.Open, attrs=attrs)

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_ARGS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_ARGS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_ARGS, 'argument input variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, 'extern input variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_IN_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_LOCALS, 'local input variables')

        if getinfo('is_openmp_app'):
            attrs = {'variable': 'kgen_is_save(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

            namedpart_append_comment(node.kgen_kernel_id, BEFORE_CALLSITE, 'END CRITICAL', style='openmp')
        else:
            attrs = {'variable': 'kgen_is_save', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)


    def create_callsite_parts2(self, node):

        index, partname, part = get_part_index(node)
        namedpart_create_subpart(node.kgen_parent, AFTER_CALLSITE, EXEC_PART, index=index+1)

        if getinfo('is_openmp_app'):
            namedpart_append_comment(node.kgen_kernel_id, AFTER_CALLSITE, 'CRITICAL', style='openmp')

            attrs = {'expr': 'kgen_is_save(OMP_GET_THREAD_NUM())'}
        else:
            attrs = {'expr': 'kgen_is_save'}
        ifsave = namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, block_statements.IfThen, attrs=attrs)

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_OUT_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_EXTERNS, 'extern output variables')

        namedpart_create_subpart(ifsave, STATE_PBLOCK_WRITE_OUT_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_LOCALS, 'local output variables')

        if getinfo('is_openmp_app'):
            attrs = {'specs': [ 'UNIT=kgen_unit(OMP_GET_THREAD_NUM())' ]}
            part_append_gensnode(ifsave, EXEC_PART, statements.Close, attrs=attrs)

            attrs = {'variable': 'kgen_is_save(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

        else:
            attrs = {'specs': [ 'UNIT=kgen_unit' ]}
            part_append_gensnode(ifsave, EXEC_PART, statements.Close, attrs=attrs)

            attrs = {'variable': 'kgen_is_save', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'is_last_invoke'}
        iflast = part_append_gensnode(ifsave, EXEC_PART, block_statements.IfThen, attrs=attrs)

        if getinfo('is_mpi_app') or getinfo('is_openmp_app'):
            arrs = []
            for idx, dummy in enumerate(getinfo('invocations')):
                arrs.append('kgen_check_%d'%idx)
                arrs.append('LBOUND(kgen_check_%d)'%idx)
                arrs.append('UBOUND(kgen_check_%d)'%idx)

            attrs = {'expr': 'kgen_check_stop(%s)'%' ,'.join(arrs)}
            ifstop = part_append_gensnode(iflast, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['"KGEN collected all state data. KGEN forces application to stop. "']}
            part_append_gensnode(ifstop, EXEC_PART, statements.Write, attrs=attrs)
            
            attrs = {'designator': 'mpi_abort', 'items': [getinfo('mpi_comm'), '0', 'kgen_ierr']}
            part_append_gensnode(ifstop, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'code': '0'}
            part_append_gensnode(ifstop, EXEC_PART, statements.Stop, attrs=attrs)

        else:
            attrs = {'code': '0'}
            part_append_gensnode(iflast, EXEC_PART, statements.Stop, attrs=attrs)
