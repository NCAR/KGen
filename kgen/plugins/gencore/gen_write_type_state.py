# gen_write_type-state.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_writename

class Gen_S_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.is_contains_created = False # adding item within in the same process fuctions does not updated.

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial event
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.is_valid, self.create_dtype_write_subr) 

    # match functions
    def is_valid(self, node):
        return node.kgen_isvalid


    # process function
    def create_dtype_write_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        subrname = get_dtype_writename(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        if not has_object_in_part(parent, SUBP_PART, block_statements.Subroutine, attrs={'name':subrname} ):

            if not self.is_contains_created and not has_object_in_part(parent, CONTAINS_PART, statements.Contains ):
                append_comment_in_part(parent, CONTAINS_PART, '')
                append_item_in_part(parent, CONTAINS_PART, gensobj(parent, statements.Contains, node.kgen_kernel_id))
                append_comment_in_part(parent, CONTAINS_PART, '')
                self.is_contains_created = True

            append_comment_in_part(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
            attrs = {'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = gensobj(parent, block_statements.Subroutine, node.kgen_kernel_id, attrs=attrs)
            append_item_in_part(parent, SUBP_PART, subrobj)
            append_comment_in_part(parent, SUBP_PART, '')

            node.kgen_stmt.top.used4genstate = True


            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Type, node.kgen_kernel_id, attrs=attrs))

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Type, node.kgen_kernel_id, attrs=attrs))

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Type, node.kgen_kernel_id, attrs=attrs))

            # is_true
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_true']}
            append_item_in_part(subrobj, DECL_PART, gensobj(subrobj, typedecl_statements.Logical, node.kgen_kernel_id, attrs=attrs))
            append_comment_in_part(subrobj, DECL_PART, '')
#
#            # call to write components
#            self.write_components(subrobj)
#

            # create public stmt
            attrs = {'items': [subrname]}
            append_item_in_part(parent, DECL_PART, gensobj(parent, statements.Public, node.kgen_kernel_id, attrs=attrs))

#######################################################################################################################
#        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp']}
#        append_item_in_part(node, USE_PART, genkobj(node, statements.Use, node.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(node, USE_PART, '')
#
#        append_item_in_part(node, IMPLICIT_PART, genkobj(node, typedecl_statements.Implicit, node.kgen_kernel_id))
#        append_comment_in_part(node, IMPLICIT_PART, '')
#
#        if getinfo('is_mpi_app'):
#            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpi_rank']}
#            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#        
#            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_mpi_rank_conv'], 'selector':('16', None)}
#            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#            attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('mpi_rank_size')], \
#                'entity_decls': ['kgen_mpi_rank_at = (/ %s /)'%', '.join(getinfo('mpi_ranks'))]}
#            append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr', 'kgen_unit', 'kgen_counter', 'kgen_repeat_counter']}
#        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_counter_conv'], 'selector':('16', None)}
#        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER', 'DIMENSION(%s)'%getinfo('invocation_size')], \
#            'entity_decls': ['kgen_counter_at = (/ %s /)'%', '.join(getinfo('invocation_numbers'))]}
#        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_filepath'], 'selector':('1024', None)}
#        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'type_spec': 'REAL', 'entity_decls': ['total_time'], 'selector': (None, 'kgen_dp')}
#        append_item_in_part(node, DECL_PART, genkobj(node, typedecl_statements.Integer, node.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(node, DECL_PART, '')
#
#        attrs = {'variable': 'total_time', 'sign': '=', 'expr': '0.0_kgen_dp'}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Assignment, node.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(node, EXEC_PART, '')
#       
#        # file open head
#        if getinfo('is_mpi_app'):
#            repeat_count = getinfo('mpi_rank_size') * getinfo('invocation_size')
#        else:
#            repeat_count = getinfo('invocation_size')
#
#        attrs = {'loopcontrol': 'kgen_repeat_counter = 0, %d'%(repeat_count-1)}
#        doobj = genkobj(node, block_statements.Do, node.kgen_kernel_id, attrs=attrs)
#        append_item_in_part(node, EXEC_PART, doobj)
#        append_comment_in_part(doobj, EXEC_PART, '')
#
#        attrs = {'variable': 'kgen_counter', 'sign': '=', 'expr': 'kgen_counter_at(mod(kgen_repeat_counter, %d)+1)'%getinfo('invocation_size')}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Assignment, doobj.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'items': ['kgen_counter'], 'specs': ['kgen_counter_conv', '*']}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Write, doobj.kgen_kernel_id, attrs=attrs))
#
#        if getinfo('is_mpi_app'):
#            attrs = {'variable': 'kgen_mpi_rank', 'sign': '=', 'expr': 'kgen_mpi_rank_at(mod(kgen_repeat_counter, %d)+1)'%getinfo('mpi_rank_size')}
#            append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Assignment, doobj.kgen_kernel_id, attrs=attrs))
#
#            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
#                getinfo('kernel_name')}
#            append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Assignment, doobj.kgen_kernel_id, attrs=attrs))
#        else:
#            attrs = {'variable': 'kgen_filepath', 'sign': '=', 'expr': '"%s." // TRIM(ADJUSTL(kgen_counter_conv))'%getinfo('kernel_name')}
#            append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Assignment, doobj.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'variable': 'kgen_unit', 'sign': '=', 'expr': 'kgen_get_newunit()'}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Assignment, doobj.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(doobj, EXEC_PART, '')
#
#        attrs = {'specs': ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="OLD"', 'ACCESS="STREAM"', \
#            'FORM="UNFORMATTED"', 'ACTION="READ"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"']}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Open, doobj.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'expr': 'kgen_ierr /= 0'}
#        ifobj = genkobj(doobj, block_statements.IfThen, doobj.kgen_kernel_id, attrs=attrs)
#        append_item_in_part(doobj, EXEC_PART, ifobj)
#
#        attrs = {'designator': 'kgen_error_stop', 'items': ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))']}
#        append_item_in_part(ifobj, EXEC_PART, genkobj(ifobj, statements.Call, ifobj.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(doobj, EXEC_PART, '')
#
#        attrs = {'items': ['"** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' **"']}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Write, doobj.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(doobj, EXEC_PART, '')
#
#        # insertion location for driver in state read and callsite call
#        doobj_exec_part = get_part(doobj, EXEC_PART) 
#        self.insertion_index = len(doobj_exec_part)
#
##        attrs = {'designator': getinfo('parentblock_subp_name'), 'items': getinfo('kernel_driver_args')}
##        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Call, doobj.kgen_kernel_id, attrs=attrs))
##        append_comment_in_part(doobj, EXEC_PART, '')
#
#        attrs = {'specs': ['UNIT=kgen_unit']}
#        append_item_in_part(doobj, EXEC_PART, genkobj(doobj, statements.Close, doobj.kgen_kernel_id, attrs=attrs))
#        append_comment_in_part(doobj, EXEC_PART, '')
#
#        append_comment_in_part(node, EXEC_PART, '')
#
#        attrs = {'items': ['""']}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'items': ['"******************************************************************************"']}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'items': ['"%s summary: Total number of verification cases: %d"'%(getinfo('parentblock_subp_name'), repeat_count)]}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'items': ['"%s summary: Total time of all calls (usec): ", total_time'%getinfo('parentblock_subp_name')]}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
#
#        attrs = {'items': ['"******************************************************************************"']}
#        append_item_in_part(node, EXEC_PART, genkobj(node, statements.Write, node.kgen_kernel_id, attrs=attrs))
#
#        # register next event
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
#            doobj, None, self.recover_stmt_parts) 
#
