# gen_driver.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

EXEC1_PART = 'exec1'
DRIVER_IN_LOCAL_PART = 'driver_in_local'
CALLSITE_PART = 'callsite'
EXEC2_PART = 'exec2'

class Gen_K_Program(Kgen_Plugin):
    # registration
    def register(self, msg):
        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Program, self.is_driver_name, self.create_kernel_driver_parts) 

        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Program, self.is_driver_name, self.create_driver_stmts) 

        msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            block_statements.Program, self.is_driver_name, self.recover_stmt_parts) 

    # match functions
    def is_driver_name(self, node):
        if node.name==getinfo('kernel_driver_name'): return True
        else: return False

    #  process after node creation
    def create_kernel_driver_parts(self, node):
        create_part(node, EXEC2_PART)
        create_part(node, CALLSITE_PART)
        create_part(node, DRIVER_IN_LOCAL_PART)
        create_part(node, EXEC1_PART)

        node.idx_exec = get_index_part_order(node, EXEC_PART)
        unregister_part_from_part_order(node, EXEC_PART)

        register_part_to_part_order(node, node.idx_exec, EXEC2_PART)
        register_part_to_part_order(node, node.idx_exec, CALLSITE_PART)
        register_part_to_part_order(node, node.idx_exec, DRIVER_IN_LOCAL_PART)
        register_part_to_part_order(node, node.idx_exec, EXEC1_PART)

        discard_items_in_part(node, EXEC_PART)

    #  finalize process 
    def recover_stmt_parts(self, node):
        for item in get_part(node, EXEC1_PART):
            append_item_in_part(node, EXEC_PART, item)
        unregister_part_from_part_order(node, EXEC1_PART)

        for item in get_part(node, DRIVER_IN_LOCAL_PART):
            append_item_in_part(node, EXEC_PART, item)
        unregister_part_from_part_order(node, DRIVER_IN_LOCAL_PART)

        for item in get_part(node, CALLSITE_PART):
            append_item_in_part(node, EXEC_PART, item)
        unregister_part_from_part_order(node, CALLSITE_PART)

        for item in get_part(node, EXEC2_PART):
            append_item_in_part(node, EXEC_PART, item)
        unregister_part_from_part_order(node, EXEC2_PART)

        register_part_to_part_order(node, node.idx_exec, EXEC_PART)

        remove_part(node, EXEC2_PART)
        remove_part(node, CALLSITE_PART)
        remove_part(node, DRIVER_IN_LOCAL_PART)
        remove_part(node, EXEC1_PART)

        delattr(node, 'idx_exec')

    # process function
    def create_driver_stmts(self, node):

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp']}
        append_item_in_part(node, USE_PART, genkobj(node, statements.Use, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(node, USE_PART, '')

        append_item_in_part(node, IMPLICIT_PART, genkobj(node, typedecl_statements.Implicit, node.kgen_kernel_id))
        append_comment_in_part(node, IMPLICIT_PART, '')

        if getinfo('is_mpi_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank']}
            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
        
            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_mpi_rank_conv'], 'selector':('16', None)}
            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('mpi_rank_size')], \
                'entity_decls': ['kgen_mpi_rank_at = (/ %s /)'%', '.join(getinfo('mpi_ranks'))]}
            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit', 'kgen_counter', 'kgen_repeat_counter']}
        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_counter_conv'], 'selector':('16', None)}
        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('invocation_size')], \
            'entity_decls': ['kgen_counter_at = (/ %s /)'%', '.join(getinfo('invocation_numbers'))]}
        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))

        attrs = {'type_spec': 'REAL', 'entity_decls': ['total_time'], 'selector': (None, 'kgen_dp')}
        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(node, DECL_PART, '')

        attrs = {'variable': 'total_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
        append_item_in_part(node, EXEC1_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(node, EXEC1_PART, '')
       
        # file open head
        if getinfo('is_mpi_app'):
            len = getinfo('mpi_rank_size') * getinfo('invocation_size')
        else:
            len = getinfo('invocation_size')

        attrs = {'loopcontrol': 'kgen_repeat_counter = 0, %d'%(len-1)}
        doobj = genkobj(node, block_statements.Do, node.kgen_kernel_id, attrs=attrs)
        append_item_in_part(node, EXEC1_PART, doobj)
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'variable': 'kgen_counter', 'sign': '=', 'expr': 'kgen_counter_at(mod(kgen_repeat_counter, %d)+1)'%getinfo('invocation_size')}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))

        attrs = {'items': ['kgen_counter'], 'specs': ['kgen_counter_conv', '*']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

        if getinfo('is_mpi_app'):
            attrs = {'variable': 'kgen_mpi_rank', 'sign': '=', 'expr': 'kgen_mpi_rank_at(mod(kgen_repeat_counter, %d)+1)'%getinfo('mpi_rank_size')}
            append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))

            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
                getinfo('kernel_name')}
            append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))
        else:
            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv))'%getinfo('kernel_name')}
            append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))

        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Open, node.kgen_kernel_id, attrs=attrs))

        attrs = {'expr': 'kgen_ierr /= 0'}
        ifobj = genkobj(node, block_statements.IfThen, node.kgen_kernel_id, attrs=attrs)
        append_item_in_part(doobj, EXEC_PART, ifobj)

        attrs = {'designator': 'kgen_error_stop', 'items': ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))']}
        append_item_in_part(ifobj, EXEC_PART, genkobj(node, statements.Call, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'items': ['"** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' **"']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'designator': getinfo('parentblock_subp_name'), 'items': getinfo('kernel_driver_args')}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Call, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'specs': ['UNIT=kgen_unit']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Close, node.kgen_kernel_id, attrs=attrs))
        append_comment_in_part(doobj, EXEC_PART, '')

        attrs = {'items': ['""']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

        attrs = {'items': ['"******************************************************************************"']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

        attrs = {'items': ['"%s summary: Total number of verification cases: %d"'%(getinfo('parentblock_subp_name'), len)]}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

        attrs = {'items': ['"%s summary: Total time of all calls (usec): ", total_time'%getinfo('parentblock_subp_name')]}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

        attrs = {'items': ['"******************************************************************************"']}
        append_item_in_part(doobj, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))

