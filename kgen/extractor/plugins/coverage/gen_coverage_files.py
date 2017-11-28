
import os
import random
import json
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

RECL = 10
META = 'metadata.json'

# TODO: gen_coverage subroutine should contains MPI communicator if MPI is enabled

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.paths = collections.OrderedDict()
        self.logger = getinfo('logger')

    # registration
    def register(self, msg):

        self.frame_msg = msg

        # read map
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.read_maps)

    ##################################
    # printing paths
    ##################################

    def read_maps(self, node):

        # generate metadata.json for filemap
        coverage_model_file = '%s/__data__/%s/files'%(getinfo('model_path'), getinfo('coverage_typeid'))
        import pdb; pdb.set_trace()
        if os.path.exists(coverage_model_file):
            with open(coverage_model_file, 'w') as fm:
                import pdb; pdb.set_trace()
                cm = json.load(fm)
        else:
            raise Exception('Coverage model data does not exist.')
        #setinfo('coverage_paths', self.paths)
