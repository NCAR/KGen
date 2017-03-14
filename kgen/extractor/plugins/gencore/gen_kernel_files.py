# gen_read_callsite_file.py

import os
from parser import block_statements
from kgplugin import Kgen_Plugin

class Gen_K_Files(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            block_statements.BeginSource, None, self.add_file_header)

    def add_file_header(self, node):
        from time import strftime

        part_insert_comment(node, UNIT_PART, 0, 'KGEN-generated Fortran source file')
        part_insert_comment(node, UNIT_PART, 1, '')
        part_insert_comment(node, UNIT_PART, 2, 'Generated at : %s'%strftime("%Y-%m-%d %H:%M:%S"))
        part_insert_comment(node, UNIT_PART, 3, 'KGEN version : %s'%getinfo('kgen_version'))
        part_insert_comment(node, UNIT_PART, 4, '')
        #part_insert_comment(node, UNIT_PART, 5, 'KERNEL LICENSE      : Please see KERNEL_LICENSE.txt for details.')
