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
            getinfo('callsite_stmts')[0], None, self.create_perturb_stmts)

    def create_perturb_stmts(self, node):

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_perturb_real']}
        part_append_genknode(node.kgen_parent, USE_PART, statements.Use, attrs=attrs)

        index, partname, part = get_part_index(node)

        part_insert_comment(node.kgen_parent, partname, index, '')
        part_insert_comment(node.kgen_parent, partname, index+1, 'Uncomment following call statement to turn on perturbation experiment.')
        part_insert_comment(node.kgen_parent, partname, index+2, 'CALL kgen_perturb_real( your_variable, 1.0E-15 )')
        part_insert_comment(node.kgen_parent, partname, index+3, '')
