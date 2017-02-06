
from kgplugin import Kgen_Plugin
from parser import block_statements

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        #self.data = {} # { filepath: { lineno: { mpi : { openmp: count } } } }
        self.data = {} # { filepath: [ lineno, lineno, ... ] }
        self.logger = getinfo('logger')

    # registration
    def register(self, msg):

        self.frame_msg = msg

        # when created, save locations, and calculate total number of blocks
        # filepath idx, linenumber, mpi rank, openmp tid, time, counter, type id(invocation, papi l1 cache miss or elapsed time, ...)
        # simple case: filepath idx, linenumber, mpi rank, openmp tid
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.IfThen, None, self.preprocess_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.ElseIf, None, self.preprocess_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Else, None, self.preprocess_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.preprocess_ifstmt)

        # when begin process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.IfThen, None, self.addstmts_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.ElseIf, None, self.addstmts_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Else, None, self.addstmts_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.addstmts_ifstmt)

    ##################################
    # preprocessing
    ##################################

    def preprocess_ifthen(self, node):
        self.logger.info('Begin preprocess_ifthen')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id not in self.data:
            self.data[node.kgen_stmt.reader.id] = {}

        if node.kgen_stmt.item.span[1] not in self.data[node.kgen_stmt.reader.id]:
            self.data[node.kgen_stmt.reader.id][node.kgen_stmt.item.span[1]] = None

    def preprocess_elseif(self, node):
        self.logger.info('Begin preprocess_elseif')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id not in self.data:
            self.data[node.kgen_stmt.reader.id] = {}

        if node.kgen_stmt.item.span[1] not in self.data[node.kgen_stmt.reader.id]:
            self.data[node.kgen_stmt.reader.id][node.kgen_stmt.item.span[1]] = None

    def preprocess_else(self, node):
        self.logger.info('Begin preprocess_else')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id not in self.data:
            self.data[node.kgen_stmt.reader.id] = {}

        if node.kgen_stmt.item.span[1] not in self.data[node.kgen_stmt.reader.id]:
            self.data[node.kgen_stmt.reader.id][node.kgen_stmt.item.span[1]] = None

    #def preprocess_ifstmt(self, node):
    #    getinfo('logger').info('Begin preprocess_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # adding statements
    ##################################

    def addstmts_ifthen(self, node):
        self.logger.info('Begin addstmts_ifthen')

        import pdb; pdb.set_trace()
    def addstmts_elseif(self, node):
        self.logger.info('Begin addstmts_elseif')

    def addstmts_else(self, node):
        self.logger.info('Begin addstmts_else')

    #def addstmts_ifstmt(self, node):
    #    getinfo('logger').info('Begin addstmts_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True


            #attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_openmp_issave = -1']}
            #part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            #attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_openmp_issave = -1']}
            #part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)

