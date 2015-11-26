# gen_read_callsite_file.py

import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import KERNEL_PBLOCK_USE_PART, KERNEL_PBLOCK_DECL_PART, KERNEL_PBLOCK_EXEC_PART, \
    KERNEL_PBLOCK_CONTAINS_PART, KERNEL_PBLOCK_SUBP_PART, KERNEL_PBLOCK_READ_IN_EXTERNS, KERNEL_PBLOCK_READ_IN_LOCALS, \
    KERNEL_PBLOCK_READ_OUT_EXTERNS, KERNEL_PBLOCK_READ_OUT_LOCALS, KERNEL_TBLOCK_USE_PART, KERNEL_TBLOCK_DECL_PART, \
    KERNEL_TBLOCK_CONTAINS_PART, KERNEL_TBLOCK_SUBP_PART, kernel_gencore_contains

class Gen_K_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('callsite_stmt'), None, self.create_callsite_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.create_parentblock_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.create_topblock_parts)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('callsite_stmt'), None, self.process_ifstmt)


    def create_parentblock_parts(self, node):

        namedpart_link_part(node, KERNEL_PBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, KERNEL_PBLOCK_SUBP_PART, SUBP_PART)

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_dp']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['total_time'], \
            'selector': (None, 'kgen_dp')}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        part_append_comment(node, DECL_PART, '')

#        # ensure contains
#        checks = lambda n: isinstance(n.kgen_stmt, statements.Contains)
#        if not node in kernel_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
#            part_append_comment(node, CONTAINS_PART, '')
#            part_append_genknode(node, CONTAINS_PART, statements.Contains)
#            part_append_comment(node, CONTAINS_PART, '')
#            kernel_gencore_contains.append(node)

        node.new_args = getinfo('kernel_driver_callsite_args')
        node.tosubr = True
        node.kgen_use_tokgen = True

        node.kgen_end_obj.tosubr = True
        node.kgen_end_obj.kgen_use_tokgen = True

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
        part, index = get_part_index(node)

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_IN_EXTERNS, EXEC_PART, index=index)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_EXTERNS, 'extern input variables')

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_IN_LOCALS, EXEC_PART, index=index+1)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_LOCALS, 'local input variables')

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_OUT_EXTERNS, EXEC_PART, index=index+2)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, 'extern output variables')

        namedpart_create_subpart(node.kgen_parent, KERNEL_PBLOCK_READ_OUT_LOCALS, EXEC_PART, index=index+3)
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_LOCALS, 'local output variables')

    def process_ifstmt(self, node):
        if node.kgen_stmt.__class__ in [ block_statements.If ]:
            node.kgen_forced_line = node.kgen_stmt.content[0].tokgen()

