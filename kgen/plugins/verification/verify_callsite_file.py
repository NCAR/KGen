# gen_read_callsite_file.py

import statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from verify_utils import VERIFY_PBLOCK_USE_PART, VERIFY_PBLOCK_DECL_PART, VERIFY_PBLOCK_EXEC_PART, \
    VERIFY_PBLOCK_CONTAINS_PART, VERIFY_PBLOCK_SUBP_PART, VERIFY_PBLOCK_EXTERNS, VERIFY_PBLOCK_LOCALS, \
    VERIFY_PBLOCK_INIT

class Verify_K_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.create_parentblock_parts)

    def create_parentblock_parts(self, node):

        namedpart_link_part(node, VERIFY_PBLOCK_USE_PART, USE_PART)
        namedpart_link_part(node, VERIFY_PBLOCK_DECL_PART, DECL_PART)
        namedpart_link_part(node, VERIFY_PBLOCK_EXEC_PART, EXEC_PART)
        namedpart_link_part(node, VERIFY_PBLOCK_CONTAINS_PART, CONTAINS_PART)
        namedpart_link_part(node, VERIFY_PBLOCK_SUBP_PART, SUBP_PART)

        namedpart_create_subpart(node, VERIFY_PBLOCK_INIT, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_INIT, '')
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_INIT, 'verify init')

        namedpart_create_subpart(node, VERIFY_PBLOCK_EXTERNS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_EXTERNS, '')
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_EXTERNS, 'extern verify variables')

        namedpart_create_subpart(node, VERIFY_PBLOCK_LOCALS, EXEC_PART)
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_LOCALS, '')
        namedpart_append_comment(node.kgen_kernel_id, VERIFY_PBLOCK_LOCALS, 'local verify variables')

        attrs = {'name': 'kgen_utils_mod', 'isonly': True, 'items':['check_t', 'kgen_init_check']}
        part_append_gensnode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'type_spec': 'TYPE', 'selector':(None, 'check_t'), 'entity_decls': ['check_status']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Type, attrs=attrs)

        attrs = {'designator': 'kgen_init_check', 'items': ['check_status']}
        namedpart_append_genknode(node.kgen_kernel_id, VERIFY_PBLOCK_INIT, statements.Call, attrs=attrs)

