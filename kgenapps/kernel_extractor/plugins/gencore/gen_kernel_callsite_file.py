# gen_read_callsite_file.py

import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

# TODO: adjust in and out state of variables for verification

from gencore_utils import KERNEL_PBLOCK_USE_PART, KERNEL_PBLOCK_DECL_PART, KERNEL_PBLOCK_EXEC_PART, \
    KERNEL_PBLOCK_CONTAINS_PART, KERNEL_PBLOCK_SUBP_PART, KERNEL_PBLOCK_READ_IN_LOCALS, \
    KERNEL_PBLOCK_READ_OUT_EXTERNS, KERNEL_PBLOCK_READ_OUT_LOCALS, KERNEL_TBLOCK_USE_PART, KERNEL_TBLOCK_DECL_PART, \
    KERNEL_TBLOCK_CONTAINS_PART, KERNEL_TBLOCK_SUBP_PART, kernel_gencore_contains, KERNEL_PBLOCK_BEFORE_KERNEL, KERNEL_PBLOCK_AFTER_KERNEL

class Gen_K_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('callsite_stmts')[0], None, self.create_callsite_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('callsite_stmts')[0], None, self.invalid_kernel_stmts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.create_parentblock_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('parentblock_stmt'), None, self.set_args)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.create_topblock_parts)

    def set_args(self, node):

        node.new_args = getinfo('kernel_driver_callsite_args')
        node.tosubr = True
        node.kgen_use_tokgen = True

        node.kgen_end_obj.tosubr = True
        node.kgen_end_obj.kgen_use_tokgen = True

    def create_parentblock_parts(self, node):

        namedpart_link_part(node, KERNEL_PBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_SUBP_PART, SUBP_PART)

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_dp', 'kgen_array_sumcheck']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)
           
        #attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
        #part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'attrspec': ['INTENT(OUT)'], 'entity_decls': ['kgen_elapsed_time'], \
            'selector': (None, 'kgen_dp')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'attrspec': ['INTENT(OUT)'], 'entity_decls': ['kgen_isverified']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Real, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_evalstage', 'kgen_warmupstage', 'kgen_mainstage']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke', 'kgen_evalstage', 'kgen_warmupstage', 'kgen_mainstage') ) ]}
        part_append_gensnode(node, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(node, DECL_PART, '')

        namedpart_create_subpart(node, KERNEL_PBLOCK_READ_IN_LOCALS, EXEC_PART, index=0)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, 'local input variables')

        namedpart_create_subpart(node, KERNEL_PBLOCK_READ_OUT_EXTERNS, EXEC_PART, index=1)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, 'extern output variables')

        namedpart_create_subpart(node, KERNEL_PBLOCK_READ_OUT_LOCALS, EXEC_PART, index=2)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, 'local output variables')

#        kernel_stmts = getinfo('callsite_stmts')
#        if len(kernel_stmts)!=1 or not isinstance(kernel_stmts[0], statements.Call):
#            # ensure contains
#            checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
#            if not node in kernel_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
#                part_append_comment(node, CONTAINS_PART, '')
#                part_append_genknode(node, CONTAINS_PART, statements.Contains)
#                part_append_comment(node, CONTAINS_PART, '')
#                kernel_gencore_contains.append(node)
#
#
#            part_append_comment(node, SUBP_PART, 'kgen kernel subroutine')
#            attrs = {'name': 'kgen_kernel'}
#            subrobj = part_append_genknode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
#            part_append_comment(node, SUBP_PART, '')
#
#            start = kernel_stmts[0].item.span[0]-1
#            end = kernel_stmts[-1].item.span[1]
#            lines = kernel_stmts[0].top.prep[start:end]
#            lines_str = '\n'.join(lines)
#            dummy_node = part_append_genknode(subrobj, EXEC_PART, statements.Call)
#            dummy_node.kgen_stmt = getinfo('dummy_stmt')
#            dummy_node.kgen_forced_line = lines_str
        # debug
#        attrs = {'variable': 'kgen_region_started', 'sign': '=', 'expr': '.TRUE.'}
#        part_insert_gensnode(node, EXEC_PART, statements.Assignment, attrs=attrs, index=0)
#
#        attrs = {'variable': 'kgen_region_started', 'sign': '=', 'expr': '.FALSE.'}
#        part_append_gensnode(node, EXEC_PART, statements.Assignment, attrs=attrs)

    def create_topblock_parts(self, node):

        namedpart_link_part(node, KERNEL_TBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, KERNEL_TBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, KERNEL_TBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, KERNEL_TBLOCK_SUBP_PART, SUBP_PART)

        # ensure public for parentblock stmt
        checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and \
            getinfo('parentblock_stmt').name in n.kgen_stmt.items
        if not part_has_node(node, DECL_PART, checks):
            attrs = {'items':[getinfo('parentblock_stmt').name]}
            part_append_genknode(node, DECL_PART, statements.Public, attrs=attrs)

    def create_callsite_parts(self, node):
        index, partname, part = get_part_index(node)

#        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_IN_LOCALS, EXEC_PART, index=index)
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, '')
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, 'local input variables')
#
#        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_OUT_EXTERNS, EXEC_PART, index=index+1)
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, '')
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, 'extern output variables')
#
#        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_OUT_LOCALS, EXEC_PART, index=index+2)
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, '')
#        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, 'local output variables')

        idx = index

        attrs = {'expr': 'kgen_evalstage'}
        ifeval = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_beforecallsite_eval', ifeval)
        idx += 1

        attrs = {'expr': 'kgen_warmupstage'}
        ifwarmup = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_beforecallsite_warmup', ifwarmup)
        idx += 1

        attrs = {'expr': 'kgen_mainstage'}
        ifmain = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_beforecallsite_main', ifmain)
        idx += 1

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_BEFORE_KERNEL, EXEC_PART, index=idx)
        self.plugin_common[node.kgen_kernel_id]['ext.gencore']['blocks']['before_kernel'] = KERNEL_PBLOCK_BEFORE_KERNEL
        idx += 1

        part_insert_comment(node.kgen_parent, EXEC_PART, idx, '')
        idx += 1
        part_insert_comment(node.kgen_parent, EXEC_PART, idx, 'call to kgen kernel')
        idx += 1

        attrs = {'variable': 'kgen_warmupstage', 'sign': '=', 'expr': '.TRUE.'}
        part_insert_gensnode(node, EXEC_PART, statements.Assignment, attrs=attrs, index=idx)
        idx += 1
#
#        kernel_stmts = getinfo('callsite_stmts')
#        if len(kernel_stmts)!=1 or not isinstance(kernel_stmts[0], statements.Call):
#            attrs = {'designator': 'kgen_kernel'}
#            part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Call, attrs=attrs, index=index+3)
#        else:
#            start = node.kgen_stmt.item.span[0]-1
#            end = node.kgen_stmt.item.span[1]
#            lines = node.kgen_stmt.top.prep[start:end]
#            lines_str = '\n'.join(lines)
#            dummy_node = part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Call, index=index+3)
#            dummy_node.kgen_stmt = node.kgen_stmt
#            dummy_node.kgen_forced_line = lines_str

        kernel_stmts = getinfo('callsite_stmts')
        start = kernel_stmts[0].item.span[0]-1
        end = kernel_stmts[-1].item.span[1]
        lines = kernel_stmts[0].top.prep[start:end]
        lines_str = '\n'.join(lines)
        dummy_node = part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Call, index=idx)
        dummy_node.kgen_stmt = getinfo('dummy_stmt')
        dummy_node.kgen_forced_line = lines_str
        idx += 1

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_AFTER_KERNEL, EXEC_PART, index=idx)
        self.plugin_common[node.kgen_kernel_id]['ext.gencore']['blocks']['after_kernel'] = KERNEL_PBLOCK_AFTER_KERNEL
        idx += 1

        attrs = {'expr': 'kgen_mainstage'}
        ifmain = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_aftercallsite_main', ifmain)
        idx += 1

        attrs = {'expr': 'kgen_warmupstage'}
        ifwarmup = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_aftercallsite_warmup', ifwarmup)
        idx += 1

        attrs = {'expr': 'kgen_evalstage'}
        ifeval = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        setinfo('blocknode_aftercallsite_eval', ifeval)

    def invalid_kernel_stmts(self, node):
        kernel_stmts = getinfo('callsite_stmts')

        for stmt in kernel_stmts:
            stmt.genkpair.kgen_forced_line = False
