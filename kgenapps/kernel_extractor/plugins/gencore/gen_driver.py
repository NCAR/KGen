# gen_driver.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

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
            block_statements.Program, self.is_driver_name, self.add_callsite_stmt) 
    # match functions
    def is_driver_name(self, node):
        if node.name==getinfo('kernel_driver_name'): return True
        else: return False

    def add_callsite_stmt(self, node):
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

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp', 'kgen_array_sumcheck']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        #attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
        #part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'name':getinfo('topblock_stmt').name, 'isonly': True, 'items':[getinfo('parentblock_stmt').name]}
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

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit', 'kgen_count', 'kgen_count_verified']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'selector': (None, 'kgen_dp'), 'entity_decls': \
            ['kgen_elapsed_time', 'kgen_total_time', 'kgen_min_time', 'kgen_max_time'], }
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Real, attrs=attrs)

        part_append_comment(node, DECL_PART, '')

        if getinfo('add_mpi_frame'):
            part_append_comment(node, EXEC_PART, 'Uncomment following MPI_INIT block to initialize MPI.')
            part_append_comment(node, EXEC_PART, 'CALL MPI_INIT(kgen_ierr)')
            part_append_comment(node, EXEC_PART, 'IF (kgen_ierr .NE. MPI_SUCCESS) THEN')
            part_append_comment(node, EXEC_PART, '  PRINT *, "MPI Initialization is failed."')
            part_append_comment(node, EXEC_PART, '  CALL MPI_ABORT(MPI_COMM_WORLD, -1, kgen_ierr)')
            part_append_comment(node, EXEC_PART, 'END IF')
            part_append_comment(node, EXEC_PART, '')

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_min_time', 'sign': '=', 'expr': 'HUGE(0.0_kgen_dp)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_max_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_count', 'sign': '=', 'expr': '0'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_count_verified', 'sign': '=', 'expr': '0'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_comment(node, EXEC_PART, '')

        attrs = {'variable': 'kgen_unit_list', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)
      
        attrs = {'specs': ['UNIT=kgen_unit_list', 'FILE="kgen_statefile.lst"', 'STATUS="OLD"', 'IOSTAT=kgen_ierr_list']}
        part_append_genknode(node, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr_list /= 0'}
        iflist = part_append_genknode(node, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"ERROR: ""kgen_statefile.lst"" is not found in current directory."']}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"""kgen_statefile.lst"" is a text file that has paths to state data files."']}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"If state data files exist, create ""kgen_statefile.lst"" manually or"']}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"by executing ""ls -1 %s.*.*.* > kgen_statefile.lst"""'%getinfo('kernel_name')]}
        part_append_genknode(iflist, EXEC_PART, statements.Write, attrs=attrs)

        part_append_genknode(iflist, EXEC_PART, statements.Stop)

        attrs = {'loopcontrol': 'WHILE ( kgen_ierr_list == 0 )'}
        doobj = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)

        attrs = {'items': ['kgen_filepath'], 'specs': ['UNIT = kgen_unit_list', 'FMT="(A)"', 'IOSTAT=kgen_ierr_list']}
        part_append_genknode(doobj, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'expr': 'kgen_ierr_list == 0'}
        ifread = part_append_genknode(doobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(ifread, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=TRIM(ADJUSTL(kgen_filepath))', 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_genknode(ifread, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr == 0'}
        ifopen = part_append_genknode(ifread, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"***************** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' *****************"']}
        part_append_genknode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'kgen_count', 'sign': '=', 'expr': 'kgen_count + 1'}
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

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': 'kgen_total_time + kgen_elapsed_time'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_min_time', 'sign': '=', 'expr': 'MIN( kgen_min_time, kgen_elapsed_time )'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_max_time', 'sign': '=', 'expr': 'MAX( kgen_max_time, kgen_elapsed_time )'}
        part_append_genknode(ifopen, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'kgen_isverified'}
        ifverified = part_append_genknode(ifopen, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_count_verified', 'sign': '=', 'expr': 'kgen_count_verified + 1'}
        part_append_genknode(ifverified, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_unit']}
        part_append_genknode(ifopen, EXEC_PART, statements.Close, attrs=attrs)
        part_append_comment(node, EXEC_PART, '')

        attrs = {'specs': ['UNIT=kgen_unit_list']}
        part_append_genknode(node, EXEC_PART, statements.Close, attrs=attrs)

        part_append_comment(node, EXEC_PART, '')

        attrs = {'items': ['""']}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"kernel execution summary: %s"'%getinfo('kernel_name')], 'specs': [ '*', '"(10X,A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'expr': 'kgen_count == 0'}
        ifcount = part_append_genknode(node, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['"No data file is verified."'] }
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        part_append_genknode(ifcount, EXEC_PART, statements.Else)

        attrs = {'items': ['"Total number of verification cases  "', '":"', 'kgen_count'], 'specs': [ '*', '"(4X, A36, A1, I6)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Number of verification-passed cases "', '":"', 'kgen_count_verified'], 'specs': [ '*', '"(4X, A36, A1, I6)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        #attrs = {'items': ['"Verification tolerance "', '":"', '%s'%getinfo('verify_tol')], 'specs': [ '*', '"(4X, A22, A1, E24.16)"' ]}
        #part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Average call time (usec): ", kgen_total_time / REAL(kgen_count)'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Minimum call time (usec): ", kgen_min_time'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Maximum call time (usec): ", kgen_max_time'], 'specs': [ '*', '"(4X, A, E10.3)"' ]}
        part_append_genknode(ifcount, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('add_mpi_frame'):
            part_append_comment(node, EXEC_PART, '')
            part_append_comment(node, EXEC_PART, 'Uncomment following mpi_finalize statement to finalize MPI.')
            part_append_comment(node, EXEC_PART, 'CALL mpi_finalize(kgen_ierr)')
            part_append_comment(node, EXEC_PART, '')
