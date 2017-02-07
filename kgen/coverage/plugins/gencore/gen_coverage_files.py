
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections
from coverage.main import BEGIN_DATA_MARKER, END_DATA_MARKER, BEGIN_PATH_MARKER, END_PATH_MARKER

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.paths = collections.OrderedDict()
        self.logger = getinfo('logger')

    # registration
    def register(self, msg):

        self.frame_msg = msg

        # filepath idx, linenumber, mpi rank, openmp tid, time, counter, type id(invocation, papi l1 cache miss or elapsed time, ...)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.IfThen, None, self.preprocess_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.ElseIf, None, self.preprocess_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Else, None, self.preprocess_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.preprocess_ifstmt)

        # when begin process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.IfThen, None, self.addstmts_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.ElseIf, None, self.addstmts_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Else, None, self.addstmts_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
        #    block_statements.If, None, self.addstmts_ifstmt)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('topblock_stmt'), None, self.save_paths)

        # when finish process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('topblock_stmt'), None, self.add_coverage)

    ##################################
    # preprocessing
    ##################################

    def preprocess_ifthen(self, node):
        self.logger.debug('Begin preprocess_ifthen')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = len(self.paths)

    def preprocess_elseif(self, node):
        self.logger.debug('Begin preprocess_elseif')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = len(self.paths)

    def preprocess_else(self, node):
        self.logger.debug('Begin preprocess_else')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = len(self.paths)

    #def preprocess_ifstmt(self, node):
    #    getinfo('logger').info('Begin preprocess_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # printing paths
    ##################################

    def save_paths(self, node):
        setinfo('coverage_paths', self.paths)

    ##################################
    # adding statements
    ##################################

    def ispure(self, node):

        if hasattr(node, 'kgen_stmt') and node.kgen_stmt and \
            isinstance(node.kgen_stmt, block_statements.SubProgramStatement):        
            if node.kgen_stmt.prefix.lower().find('pure') >= 0:
                return True

        if hasattr(node, 'kgen_parent'):
            return self.ispure(node.kgen_parent)

        return False

    def addstmts_ifthen(self, node):
        self.logger.debug('Begin addstmts_ifthen')

        if not self.ispure(node):
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(self.paths[node.kgen_stmt.reader.id]), str(node.kgen_stmt.item.span[1]) ]}
            part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    def addstmts_elseif(self, node):
        self.logger.debug('Begin addstmts_elseif')

        if not self.ispure(node):
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(self.paths[node.kgen_stmt.reader.id]), str(node.kgen_stmt.item.span[1]) ]}
            part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    def addstmts_else(self, node):
        self.logger.debug('Begin addstmts_else')

        if not self.ispure(node):
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(self.paths[node.kgen_stmt.reader.id]), str(node.kgen_stmt.item.span[1]) ]}
            part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    #def addstmts_ifstmt(self, node):
    #    getinfo('logger').info('Begin addstmts_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # adding coverage subroutine
    ##################################

    def add_coverage(self, node):
        self.logger.debug('Begin add_coverage')

        maxfiles = len(self.paths)
        maxloc = max([ lines for path, lines in self.paths.items() ])

        # add subroutine
        attrs = {'name': 'gen_coverage', 'args': ['fileid', 'lineno']}
        coversubr = part_append_genknode(node.kgen_parent, UNIT_PART, block_statements.Subroutine, attrs=attrs)

        if getinfo('is_mpi_app'):

            for mod_name, use_names in getinfo('mpi_use'):
                attrs = {'name':mod_name, 'isonly': True, 'items':use_names}
                part_append_gensnode(coversubr, USE_PART, statements.Use, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['initialized']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myrank', 'ierror']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['fileid', 'lineno']}
        part_append_genknode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['now']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Real, attrs=attrs)


        if getinfo('is_openmp_app'):

            if getinfo('is_mpi_app'):

                attrs = {'designator': 'MPI_INITIALIZED', 'items': [ 'initialized', 'ierror' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'expr': 'initialized'}
                ifinit = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'designator': 'MPI_COMM_RANK', 'items': [ getinfo('mpi_comm'), 'myrank', 'ierror' ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': [ '"%s"'%BEGIN_DATA_MARKER, 'fileid', 'lineno', 'myrank', 'OMP_GET_THREAD_NUM()', 'now', '"%s"'%END_DATA_MARKER ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Write, attrs=attrs)

            else:
                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': [ '"%s"'%BEGIN_DATA_MARKER, 'fileid', 'lineno', '.', 'OMP_GET_THREAD_NUM()', 'now', '"%s"'%END_DATA_MARKER ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        else:
            if getinfo('is_mpi_app'):

                attrs = {'designator': 'MPI_INITIALIZED', 'items': [ 'initialized', 'ierror' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'expr': 'initialized'}
                ifinit = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'designator': 'MPI_COMM_RANK', 'items': [ getinfo('mpi_comm'), 'myrank', 'ierror' ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': [ '"%s"'%BEGIN_DATA_MARKER, 'fileid', 'lineno', 'myrank', '.', 'now', '"%s"'%END_DATA_MARKER ]}
                part_append_gensnode(ifinit, EXEC_PART, statements.Write, attrs=attrs)

            else:

                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': [ '"%s"'%BEGIN_DATA_MARKER, 'fileid', 'lineno', '.', '.', 'now', '"%s"'%END_DATA_MARKER ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

#        # add block data
#        attrs = {'name': 'KCOVER'}
#        cblock = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.BlockData, attrs=attrs)
#
#        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(%d, %d)'%(maxfiles, maxloc) ], \
#            'entity_decls': ['coveragelist = 0']}
#        part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)
#
#        attrs = {'items': [ ( 'common_coverage', ('coveragelist', ) ) ]}
#        part_append_gensnode(cblock, DECL_PART, statements.Common, attrs=attrs)

        #import pdb; pdb.set_trace()

