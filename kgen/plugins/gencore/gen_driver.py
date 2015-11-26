# gen_driver.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import DRIVER_USE_PART, DRIVER_READ_IN_ARGS, DRIVER_CALLSITE_PART, DRIVER_DECL_PART, \
    DRIVER_EXEC_PART, DRIVER_CONTAINS_PART, DRIVER_SUBP_PART, DRIVER_ALLOC_PART, DRIVER_DEALLOC_PART, \
    shared_objects

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

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'name':getinfo('topblock_stmt').name, 'isonly': True, 'items':[getinfo('parentblock_stmt').name]}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)
        part_append_comment(node, USE_PART, '')

        part_append_genknode(node, IMPLICIT_PART, typedecl_statements.Implicit)
        part_append_comment(node, IMPLICIT_PART, '')

        if getinfo('is_mpi_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank']}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        
            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_mpi_rank_conv'], 'selector':('16', None)}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('mpi_rank_size')], \
                'entity_decls': ['kgen_mpi_rank_at = (/ %s /)'%', '.join(getinfo('mpi_ranks'))]}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit', 'kgen_counter', 'kgen_repeat_counter']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_counter_conv'], 'selector':('16', None)}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('invocation_size')], \
            'entity_decls': ['kgen_counter_at = (/ %s /)'%', '.join(getinfo('invocation_numbers'))]}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['total_time'], 'selector': (None, 'kgen_dp')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        part_append_comment(node, DECL_PART, '')

        attrs = {'variable': 'total_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)
        part_append_comment(node, EXEC_PART, '')
       
        # file open head
        if getinfo('is_mpi_app'):
            repeat_count = getinfo('mpi_rank_size') * getinfo('invocation_size')
        else:
            repeat_count = getinfo('invocation_size')

        attrs = {'loopcontrol': 'kgen_repeat_counter = 0, %d'%(repeat_count-1)}
        doobj = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)
        part_append_comment(doobj, EXEC_PART, '')

        if getinfo('is_mpi_app'):
            attrs = {'variable': 'kgen_mpi_rank', 'sign': '=', 'expr': 'kgen_mpi_rank_at(kgen_repeat_counter/%d + 1)'%getinfo('invocation_size')}
            part_append_genknode(doobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['kgen_mpi_rank'], 'specs': ['kgen_mpi_rank_conv', '*']}
            part_append_genknode(doobj, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'kgen_counter', 'sign': '=', 'expr': 'kgen_counter_at(mod(kgen_repeat_counter, %d) + 1)'%getinfo('invocation_size')}
        part_append_genknode(doobj, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'items': ['kgen_counter'], 'specs': ['kgen_counter_conv', '*']}
        part_append_genknode(doobj, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
                getinfo('kernel_name')}
            part_append_genknode(doobj, EXEC_PART, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv))'%getinfo('kernel_name')}
            part_append_genknode(doobj, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        part_append_genknode(doobj, EXEC_PART, statements.Assignment, attrs=attrs)
        part_append_comment(doobj, EXEC_PART, '')

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"']}
        part_append_genknode(doobj, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'expr': 'kgen_ierr /= 0'}
        ifobj = part_append_genknode(doobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': 'kgen_error_stop', 'items': ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))']}
        part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)
        part_append_comment(doobj, EXEC_PART, '')

        attrs = {'items': ['"** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' **"']}
        part_append_genknode(doobj, EXEC_PART, statements.Write, attrs=attrs)
        part_append_comment(doobj, EXEC_PART, '')

        # register gencore parts
        namedpart_create_subpart(doobj, DRIVER_ALLOC_PART, EXEC_PART)
        namedpart_create_subpart(doobj, DRIVER_READ_IN_ARGS, EXEC_PART)
        namedpart_create_subpart(doobj, DRIVER_CALLSITE_PART, EXEC_PART)
        namedpart_create_subpart(doobj, DRIVER_DEALLOC_PART, EXEC_PART)


        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_ARGS, '')
        namedpart_append_comment(node.kgen_kernel_id, DRIVER_READ_IN_ARGS, 'driver read in arguments')

        namedpart_append_comment(node.kgen_kernel_id, DRIVER_CALLSITE_PART, '')
        namedpart_append_comment(node.kgen_kernel_id, DRIVER_CALLSITE_PART, 'callsite part')

        attrs = {'specs': ['UNIT=kgen_unit']}
        part_append_genknode(doobj, EXEC_PART, statements.Close, attrs=attrs)
        part_append_comment(doobj, EXEC_PART, '')

        part_append_comment(node, EXEC_PART, '')

        attrs = {'items': ['""']}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"******************************************************************************"']}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"%s summary: Total number of verification cases: %d"'%(getinfo('parentblock_stmt').name, repeat_count)]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"%s summary: Total time of all calls (usec): ", total_time'%getinfo('parentblock_stmt').name]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"******************************************************************************"']}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)
