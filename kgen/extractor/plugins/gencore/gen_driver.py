# gen_driver.py
 
from kgplugin import Kgen_Plugin
from parser import statements, block_statements, typedecl_statements
from gencore_utils import DRIVER_USE_PART, DRIVER_READ_IN_ARGS, DRIVER_CALLSITE_PART, DRIVER_DECL_PART, \
    DRIVER_EXEC_PART, DRIVER_CONTAINS_PART, DRIVER_SUBP_PART, DRIVER_ALLOC_PART, DRIVER_DEALLOC_PART, \
    shared_objects, DRIVER_READ_IN_EXTERNS

class Gen_K_Driver(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial event
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Program, self.is_driver_name, self.create_kernel_driver_parts) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            block_statements.Program, self.is_driver_name, self.finalize_kernel_driver_parts) 
    # match functions
    def is_driver_name(self, node):
        if node.name==getinfo('kernel_driver_name'): return True
        else: return False

    def finalize_kernel_driver_parts(self, node):

        # add callsite stmts
        attrs = {'designator': getinfo('parentblock_stmt').name, 'items': getinfo('kernel_driver_callsite_args')}
        namedpart_append_genknode(node.kgen_kernel_id, DRIVER_CALLSITE_PART, statements.Call, attrs=attrs)

    #  process after node creation
    def create_kernel_driver_parts(self, node):

        shared_objects['driver_object'] = node

        namedpart_link_part(node, DRIVER_USE_PART, USE_PART)
        namedpart_link_part(node, DRIVER_DECL_PART, DECL_PART)
        namedpart_link_part(node, DRIVER_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, DRIVER_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, DRIVER_SUBP_PART, SUBP_PART)

        #jgw# add kgen_mpifile
        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp', 'kgen_array_sumcheck', 'kgen_rankthreadinvoke','kgen_mpifile']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        #jgw# add itt_notify
        part_append_comment(node, USE_PART, '#ifdef __ITT_NOTIFY__', style='rawtext')
        attrs = {'name':'ittnotify', 'isonly': False, 'items':[]}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)
        part_append_comment(node, USE_PART, '#endif', style='rawtext')

        attrs = {'name':'tprof_mod', 'isonly': True, 'items':['tstart', 'tstop', 'tnull', 'tprnt']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        #attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
        #part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'name':getinfo('topblock_stmt').name, 'isonly': True, 'items':[getinfo('parentblock_stmt').name]}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        #jgw# add use mpi
        part_append_comment(node, USE_PART, 'forced mpi from gen_driver.py')
        attrs = {'name':'mpi', 'isonly': False, 'items':[]}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)
        part_append_comment(node, USE_PART, '')

        part_append_genknode(node, IMPLICIT_PART, typedecl_statements.Implicit)
        part_append_comment(node, IMPLICIT_PART, '')

        if getinfo('add_mpi_frame'):
            part_append_comment(node, DECL_PART, 'Uncomment following line to include a MPI header file.')
            part_append_comment(node, DECL_PART, 'include "mpif.h"')
            part_append_comment(node, DECL_PART, '')

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_isverified']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr_list', 'kgen_unit_list']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit', 'kgen_case_count', 'kgen_count_verified']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        part_append_genknode(node, DECL_PART, typedecl_statements.Character, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(node, DECL_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'type_spec': 'INTEGER', 'selector': (None, '8'), 'entity_decls': \
                ['kgen_measure', 'kgen_total_count', 'kgen_min_count', 'kgen_max_count'], }
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            part_append_comment(node, DECL_PART, '#else', style='rawtext')

        attrs = {'type_spec': 'REAL', 'selector': (None, 'kgen_dp'), 'entity_decls': \
            ['kgen_measure', 'kgen_total_time', 'kgen_min_time', 'kgen_max_time'], }
        part_append_genknode(node, DECL_PART, typedecl_statements.Real, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(node, DECL_PART, '#endif', style='rawtext')

        attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Real, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        #jgw# add NProcs and ProcID
        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['NProcs', 'ProcID']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_mainstage']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke', 'kgen_mainstage') ) ]}
        part_append_gensnode(node, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(node, DECL_PART, '')

        if getinfo('add_mpi_frame'):
            part_append_comment(node, EXEC_PART, 'Uncomment following MPI_INIT block to initialize MPI.')
            part_append_comment(node, EXEC_PART, 'CALL MPI_INIT(kgen_ierr)')
            part_append_comment(node, EXEC_PART, 'IF (kgen_ierr .NE. MPI_SUCCESS) THEN')
            part_append_comment(node, EXEC_PART, '  PRINT *, "MPI Initialization is failed."')
            part_append_comment(node, EXEC_PART, '  CALL MPI_ABORT(MPI_COMM_WORLD, -1, kgen_ierr)')
            part_append_comment(node, EXEC_PART, 'END IF')
            part_append_comment(node, EXEC_PART, '')

        #jgw# Add calls to itt_pause
        part_append_comment(node, EXEC_PART, '#ifdef __ITT_NOTIFY__', style='rawtext')
        attrs = {'designator': 'ITT_PAUSE', 'items':('',)}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        part_append_comment(node, EXEC_PART, '#endif', style='rawtext')

        #jgw# Add calls to MPI_Init, MPI_Comm_Size, MPI_Comm_Rank
        attrs = {'designator': 'MPI_Init', 'items': ( 'kgen_ierr', ) }
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        attrs = {'designator': 'MPI_Comm_Size', 'items': ( 'MPI_COMM_WORLD', 'NProcs', 'kgen_ierr', ) }
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        attrs = {'designator': 'MPI_Comm_Rank', 'items': ( 'MPI_COMM_WORLD', 'ProcID', 'kgen_ierr', ) }
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        part_append_comment(node, EXEC_PART, '')
        #jgw#

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'variable': 'kgen_total_count', 'sign': '=', 'expr': '0_8'}
            part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_min_count', 'sign': '=', 'expr': 'HUGE(0_8)'}
            part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_max_count', 'sign': '=', 'expr': '0_8'}
            part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_comment(node, EXEC_PART, '#else', style='rawtext')

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_min_time', 'sign': '=', 'expr': 'HUGE(0.0_kgen_dp)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_max_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(node, EXEC_PART, '#endif', style='rawtext')

        attrs = {'variable': 'kgen_case_count', 'sign': '=', 'expr': '0'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_count_verified', 'sign': '=', 'expr': '0'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_comment(node, EXEC_PART, '')

        attrs = {'variable': 'kgen_unit_list', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)
      
        attrs = {'specs': ['UNIT=kgen_unit_list', 'FILE="kgen_statefile.lst"', 'STATUS="OLD"', 'IOSTAT=kgen_ierr_list']}
        part_append_genknode(node, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr_list .NE. 0'}
        iflist = part_append_genknode(node, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': 'SYSTEM', 'items': ['"ls -1 %s.*.*.* > kgen_statefile.lst"'%getinfo('kernel_name')]}
        part_append_genknode(iflist, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'designator': 'SLEEP', 'items': ['1']}
        part_append_genknode(iflist, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'variable': 'kgen_unit_list', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(iflist, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit_list', 'FILE="kgen_statefile.lst"', 'STATUS="OLD"', 'IOSTAT=kgen_ierr_list']}
        part_append_genknode(iflist, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr_list .NE. 0'}
        iflist2 = part_append_genknode(node, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(iflist2, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"ERROR: ""kgen_statefile.lst"" is not found in current directory."']}
        part_append_genknode(iflist2, EXEC_PART, statements.Write, attrs=attrs)

        part_append_genknode(iflist2, EXEC_PART, statements.Stop)

        # eval
        attrs = {'loopcontrol': 'WHILE ( kgen_ierr_list .EQ. 0 )'}
        #jgw# Get rid of while loop
        #doobj1 = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)
        doobj1 = node

        attrs = {'items': ['kgen_filepath'], 'specs': ['UNIT = kgen_unit_list', 'FMT="(A)"', 'IOSTAT=kgen_ierr_list']}
        part_append_genknode(doobj1, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'expr': 'kgen_ierr_list .EQ. 0'}
        ifread = part_append_genknode(doobj1, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(ifread, EXEC_PART, statements.Assignment, attrs=attrs)

        #jgw# call kgen_mpifile
        #attrs = {'designator': 'kgen_rankthreadinvoke', 'items': ( 'TRIM(ADJUSTL(kgen_filepath))', 'kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke' ) }
        attrs = {'designator': 'kgen_mpifile', 'items': ( 'kgen_filepath', 'ProcID', '0', '0' ) }
        part_append_genknode(ifread, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=TRIM(ADJUSTL(kgen_filepath))', 'STATUS="OLD"', 'ACCESS="STREAM"', \
#jgw#            'FORM="UNFORMATTED"', 'ACTION="READ"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'IOSTAT=kgen_ierr']}
        part_append_genknode(ifread, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr == 0'}
        ifopen = part_append_genknode(ifread, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"***************** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' *****************"']}
        part_append_genknode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

        # main
        attrs = {'variable': 'kgen_mainstage', 'sign': '=', 'expr': '.TRUE.'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        #attrs = {'items': ['"***************** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' *****************"']}
        #part_append_genknode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'kgen_case_count', 'sign': '=', 'expr': 'kgen_case_count + 1'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_isverified', 'sign': '=', 'expr': '.FALSE.'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_comment(ifopen, EXEC_PART, '')

        # register gencore parts
        namedpart_create_subpart(ifopen, DRIVER_ALLOC_PART, EXEC_PART)
        namedpart_create_subpart(ifopen, DRIVER_READ_IN_ARGS, EXEC_PART)
        namedpart_create_subpart(ifopen, DRIVER_READ_IN_EXTERNS, EXEC_PART)
        namedpart_create_subpart(ifopen, DRIVER_CALLSITE_PART, EXEC_PART)
        namedpart_create_subpart(ifopen, DRIVER_DEALLOC_PART, EXEC_PART)


        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_ARGS, '')
        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_ARGS, 'driver read in arguments')

        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_EXTERNS, 'extern input variables')

        namedpart_append_comment(node.kgen_kernel_id, DRIVER_CALLSITE_PART, '')
        namedpart_append_comment(node.kgen_kernel_id, DRIVER_CALLSITE_PART, 'callsite part')


        if getinfo('is_papi_enabled'):
            part_append_comment(ifopen, EXEC_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'variable': 'kgen_total_count', 'sign': '=', 'expr': 'kgen_total_count + kgen_measure'}
            part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_min_count', 'sign': '=', 'expr': 'MIN( kgen_min_count, kgen_measure )'}
            part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_max_count', 'sign': '=', 'expr': 'MAX( kgen_max_count, kgen_measure )'}
            part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_comment(ifopen, EXEC_PART, '#else', style='rawtext')

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': 'kgen_total_time + kgen_measure'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_min_time', 'sign': '=', 'expr': 'MIN( kgen_min_time, kgen_measure )'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_max_time', 'sign': '=', 'expr': 'MAX( kgen_max_time, kgen_measure )'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(ifopen, EXEC_PART, '#endif', style='rawtext')

        attrs = {'expr': 'kgen_isverified'}
        ifverified = part_append_genknode(ifopen, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_count_verified', 'sign': '=', 'expr': 'kgen_count_verified + 1'}
        part_append_genknode(ifverified, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit']}
        part_append_genknode(ifread, EXEC_PART, statements.Close, attrs=attrs)

        part_append_comment(node, EXEC_PART, '')

        attrs = {'specs': ['UNIT=kgen_unit_list']}
        part_append_genknode(node, EXEC_PART, statements.Close, attrs=attrs)

        part_append_comment(node, EXEC_PART, '')

        #jgw# add ifzero node and replace several nodes below with ifzero
        attrs = {'expr': 'ProcID == 0'}
        ifzero = part_append_genknode(node, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(ifzero, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(ifzero, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"kernel execution summary: %s"'%getinfo('kernel_name')], 'specs': [ '*', '"(4X,A)"' ]}
        part_append_genknode(ifzero, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(ifzero, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'expr': 'kgen_case_count == 0'}
        ifcount = part_append_genknode(ifzero, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['"No data file is verified."'] }
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        part_append_genknode(ifcount, EXEC_PART, statements.Else)

        attrs = {'items': ['"Total number of verification cases  "', '":"', 'kgen_case_count'], 'specs': [ '*', '"(4X, A36, A1, I6)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Number of verification-passed cases "', '":"', 'kgen_count_verified'], 'specs': [ '*', '"(4X, A36, A1, I6)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        #attrs = {'items': ['"Verification tolerance "', '":"', '%s'%getinfo('verify_tol')], 'specs': [ '*', '"(4X, A22, A1, E24.16)"' ]}
        #part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(ifcount, EXEC_PART, '#ifdef KGEN_PAPI', style='rawtext')

            attrs = {'items': ['"Average KGENPAPIEVENT: ", NINT(kgen_total_count / DBLE(kgen_case_count))'], 'specs': [ '*', '"(4X, A, I16)"' ]}
            part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Minimum KGENPAPIEVENT: ", kgen_min_count'], 'specs': [ '*', '"(4X, A, I16)"' ]}
            part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Maximum KGENPAPIEVENT: ", kgen_max_count'], 'specs': [ '*', '"(4X, A, I16)"' ]}
            part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

            part_append_comment(ifcount, EXEC_PART, '#else', style='rawtext')

        attrs = {'items': ['"Average call time (usec): ", kgen_total_time / DBLE(kgen_case_count)'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Minimum call time (usec): ", kgen_min_time'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Maximum call time (usec): ", kgen_max_time'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_papi_enabled'):
            part_append_comment(ifcount, EXEC_PART, '#endif', style='rawtext')

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(ifzero, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('add_mpi_frame'):
            part_append_comment(node, EXEC_PART, '')
            part_append_comment(node, EXEC_PART, 'Uncomment following mpi_finalize statement to finalize MPI.')
            part_append_comment(node, EXEC_PART, 'CALL mpi_finalize(kgen_ierr)')
            part_append_comment(node, EXEC_PART, '')

        part_append_comment(node, EXEC_PART, '')
        attrs = {'designator': 'MPI_Finalize', 'items': ( 'kgen_ierr', ) }
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)
        part_append_comment(node, EXEC_PART, '')

        # Block Data
        attrs = {'name': 'KGEN'}
        cblock = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.BlockData, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank = 0', 'kgen_openmptid = 0', 'kgen_kernelinvoke = 0']}
        part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_mainstage = .FALSE.']}
        part_append_gensnode(cblock, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke', 'kgen_mainstage') ) ]}
        part_append_gensnode(cblock, DECL_PART, statements.Common, attrs=attrs)

