# perturb_kernel_callsite_file.py

import statements
from kgen_plugin import Kgen_Plugin

class Perturb_K_Callsite_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.gen_perturb)

    def gen_perturb(self, node):
        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_perturb_real']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)
        partname = self.plugin_common[node.kgen_kernel_id]['gencore']['blocks']['before_kernel']

        namedpart_append_comment(node.kgen_kernel_id, partname, '')
        namedpart_append_comment(node.kgen_kernel_id, partname, 'Uncomment following call statement to turn on perturbation experiment.')
        namedpart_append_comment(node.kgen_kernel_id, partname, 'Adjust perturbation value and/or kind parameter if required.')
        namedpart_append_comment(node.kgen_kernel_id, partname, 'CALL kgen_perturb_real( your_variable, 1.0E-15_8 )')
        namedpart_append_comment(node.kgen_kernel_id, partname, '')
