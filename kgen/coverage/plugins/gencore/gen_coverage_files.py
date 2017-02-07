
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

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
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.addstmts_ifstmt)

        # when finish process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('topblock_stmt'), None, self.add_coverage)

    ##################################
    # preprocessing
    ##################################

    def preprocess_ifthen(self, node):
        self.logger.info('Begin preprocess_ifthen')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id in self.paths:
            self.paths[node.kgen_stmt.reader.id] += 1
        else:
            self.paths[node.kgen_stmt.reader.id] = 1

    def preprocess_elseif(self, node):
        self.logger.info('Begin preprocess_elseif')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id in self.paths:
            self.paths[node.kgen_stmt.reader.id] += 1
        else:
            self.paths[node.kgen_stmt.reader.id] = 1

    def preprocess_else(self, node):
        self.logger.info('Begin preprocess_else')
        node.kgen_stmt.top.genspair.used4coverage = True

        if node.kgen_stmt.reader.id in self.paths:
            self.paths[node.kgen_stmt.reader.id] += 1
        else:
            self.paths[node.kgen_stmt.reader.id] = 1
    #def preprocess_ifstmt(self, node):
    #    getinfo('logger').info('Begin preprocess_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # adding statements
    ##################################

    def addstmts_ifthen(self, node):
        self.logger.info('Begin addstmts_ifthen')

        attrs = {'designator': 'save_coverage', 'items': \
            [ str(self.paths.keys().index(node.kgen_stmt.reader.id)+1), str(node.kgen_stmt.item.span[1]) ]}
        part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    def addstmts_elseif(self, node):
        self.logger.info('Begin addstmts_elseif')

        attrs = {'designator': 'save_coverage', 'items': \
            [ str(self.paths.keys().index(node.kgen_stmt.reader.id)+1), str(node.kgen_stmt.item.span[1]) ]}
        part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    def addstmts_else(self, node):
        self.logger.info('Begin addstmts_else')

        attrs = {'designator': 'save_coverage', 'items': \
            [ str(self.paths.keys().index(node.kgen_stmt.reader.id)+1), str(node.kgen_stmt.item.span[1]) ]}
        part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    #def addstmts_ifstmt(self, node):
    #    getinfo('logger').info('Begin addstmts_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # adding coverage subroutine
    ##################################

    def add_coverage(self, node):
        self.logger.info('Begin add_coverage')

        maxfiles = len(self.paths)
        maxloc = max([ lines for path, lines in self.paths.items() ])

        # add subroutine
        attrs = {'name': 'save_coverage', 'args': ['fileid', 'lineno']}
        coversubr = part_append_genknode(node.kgen_parent, UNIT_PART, block_statements.Subroutine, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['fileid', 'lineno']}
        part_append_genknode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['now']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Real, attrs=attrs)

#        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(%d, %d)'%(maxfiles, maxloc) ], \
#            'entity_decls': ['coveragelist']}
#        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)
#
#        attrs = {'items': [ ( 'common_coverage', ('coveragelist', ) ) ]}
#        part_append_gensnode(coversubr, DECL_PART, statements.Common, attrs=attrs)
#
#
#        attrs = {'variable': 'coveragelist(fileid, lineno)', 'sign': '=', 'expr': 'coveragelist(fileid, lineno) + 1'}
#        part_append_gensnode(coversubr, EXEC_PART, statements.Assignment, attrs=attrs)

#        attrs = {'items': ['fileid', 'lineno', 'coveragelist(fileid, lineno)']}
#        part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)
        if getinfo('is_mpi_app'):
            if getinfo('is_openmp_app'):
            else:
########call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': ['fileid', 'lineno', 'now']}
                part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)
        else:
            if getinfo('is_openmp_app'):
                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': ['fileid', 'lineno', 'now', 'OMP_GET_THREAD_NUM()']}
                part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

            else:
                attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
                part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

                attrs = {'items': ['fileid', 'lineno', 'now']}
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

