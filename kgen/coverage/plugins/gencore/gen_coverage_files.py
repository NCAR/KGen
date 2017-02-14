
import os
import random
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections
from coverage.main import BEGIN_DATA_MARKER, END_DATA_MARKER, BEGIN_PATH_MARKER, END_PATH_MARKER

RECL = 10

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
            statements.ElseIf, None, self.preprocess_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            statements.Else, None, self.preprocess_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.If, None, self.preprocess_ifstmt)

        # when begin process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.IfThen, None, self.addstmts_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.ElseIf, None, self.addstmts_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Else, None, self.addstmts_else)
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
        #    block_statements.If, None, self.addstmts_ifstmt)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('topblock_stmt'), None, self.save_paths)

        # when finish process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('topblock_stmt'), None, self.add_coverage)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('topblock_stmt'), None, self.add_blockdata)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('parentblock_stmt'), None, self.add_commonstmt)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('callsite_stmts')[0], None, self.add_incinvoke)

    ##################################
    # preprocessing
    ##################################

    def preprocess_ifthen(self, node):
        self.logger.debug('Begin preprocess_ifthen')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = ( len(self.paths), collections.OrderedDict() )

        lines = self.paths[node.kgen_stmt.reader.id][1]

        if node.kgen_stmt.item.span[1] not in lines:
            lines[node.kgen_stmt.item.span[1]] = len(lines)

    def preprocess_elseif(self, node):
        self.logger.debug('Begin preprocess_elseif')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = ( len(self.paths), collections.OrderedDict() )

        lines = self.paths[node.kgen_stmt.reader.id][1]

        if node.kgen_stmt.item.span[1] not in lines:
            lines[node.kgen_stmt.item.span[1]] = len(lines)

    def preprocess_else(self, node):
        self.logger.debug('Begin preprocess_else')

        if node.kgen_stmt.reader.id not in self.paths:
            node.kgen_stmt.top.genspair.used4coverage = True
            self.paths[node.kgen_stmt.reader.id] = ( len(self.paths), collections.OrderedDict() )

        lines = self.paths[node.kgen_stmt.reader.id][1]

        if node.kgen_stmt.item.span[1] not in lines:
            lines[node.kgen_stmt.item.span[1]] = len(lines)

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
    # adding coverage statements
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
            path = self.paths[node.kgen_stmt.reader.id]
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(path[0]), str(path[1][node.kgen_stmt.item.span[1]]) ]}
            part_insert_gensnode(node, EXEC_PART, statements.Call, index=0, attrs=attrs)

    def addstmts_elseif(self, node):
        self.logger.debug('Begin addstmts_elseif')

        if not self.ispure(node):
            path = self.paths[node.kgen_stmt.reader.id]
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(path[0]), str(path[1][node.kgen_stmt.item.span[1]]) ]}
            idx, name, part = get_part_index(node)
            part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Call, index=(idx+1), attrs=attrs)

    def addstmts_else(self, node):
        self.logger.debug('Begin addstmts_else')

        if not self.ispure(node):
            path = self.paths[node.kgen_stmt.reader.id]
            attrs = {'designator': 'gen_coverage', 'items': \
                [ str(path[0]), str(path[1][node.kgen_stmt.item.span[1]]) ]}
            idx, name, part = get_part_index(node)
            part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Call, index=(idx+1), attrs=attrs)

    #def addstmts_ifstmt(self, node):
    #    getinfo('logger').info('Begin addstmts_ifstmt')
        #import pdb; pdb.set_trace()
    #    node.kgen_stmt.top.genspair.used4coverage = True

    ##################################
    # adding  invoke increment statement
    ##################################

    def add_incinvoke(self, node):
        index, partname, part = get_part_index(node)

        if getinfo('is_openmp_app'):
            attrs = {'variable': 'kgen_invokes(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_invokes(OMP_GET_THREAD_NUM()) + 1'}
            part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Assignment, index=index, attrs=attrs)
        else:
            attrs = {'variable': 'kgen_invokes', 'sign': '=', 'expr': 'kgen_invokes + 1'}
            part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Assignment, index=index, attrs=attrs)


    ##################################
    # adding common statement
    ##################################

    def add_commonstmt(self, node):
        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        else:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_invokes', ) ) ]}
        part_append_gensnode(node, DECL_PART, statements.Common, attrs=attrs)

    ##################################
    # adding common block data
    ##################################

    def add_blockdata(self, node):
        attrs = {'name': 'KGEN'}
        cblock = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.BlockData, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes = 0']}
            part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        else:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_invokes = 0']}
            part_append_gensnode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_invokes',) ) ]}
        part_append_gensnode(cblock, DECL_PART, statements.Common, attrs=attrs)

    ##################################
    # adding coverage subroutine
    ##################################

    def add_coverage(self, node):
        self.logger.debug('Begin add_coverage')

        maxfiles = len(self.paths)
        maxlines = max([ len(lineids) for fileid, lineids in self.paths.values() ])

        # add subroutine
        attrs = {'name': 'gen_coverage', 'args': ['fileid', 'lineid']}
        coversubr = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.Subroutine, attrs=attrs)

        if getinfo('is_mpi_app'):

            for mod_name, use_names in getinfo('mpi_use'):
                attrs = {'name':mod_name, 'isonly': True, 'items':use_names}
                part_append_gensnode(coversubr, USE_PART, statements.Use, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['initialized']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myrank', 'ierror']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'selector':('10', None), 'entity_decls': ['rankstr']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'PARAMETER' ], 'entity_decls': ['unitstart = %d'%random.randrange(100000, 999999)]}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'selector':('3', None), 'entity_decls': ['threadstr']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        else:
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'PARAMETER' ], 'entity_decls': ['unitstart = '%random.randrange(100000, 999999)]}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('16', None), 'entity_decls': ['invokestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myunit', 'visit', 'idx1', 'idx2']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['isopen']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['fileid', 'lineid']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_invokes', ) ) ]}
        part_append_gensnode(coversubr, DECL_PART, statements.Common, attrs=attrs)

       #attrs = {'type_spec': 'REAL', 'entity_decls': ['now']}
        #part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Real, attrs=attrs)

        ############# exec_part ########################

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'CRITICAL (kgen_cover)', style='openmp')

            attrs = {'variable': 'myunit', 'sign': '=', 'expr': \
                'unitstart + kgen_invokes(OMP_GET_THREAD_NUM())*%d + OMP_GET_THREAD_NUM()'%getinfo('openmp_maxthreads')}
            part_append_gensnode(coversubr, EXEC_PART, statements.Assignment, attrs=attrs)
        else:
            attrs = {'variable': 'myunit', 'sign': '=', 'expr': 'unitstart + kgen_invokes'}

        if getinfo('is_mpi_app'):

            attrs = {'designator': 'MPI_INITIALIZED', 'items': [ 'initialized', 'ierror' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'expr': 'initialized .AND. ( ierror .EQ. MPI_SUCCESS )'}
            ifinit = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'MPI_COMM_RANK', 'items': [ getinfo('mpi_comm'), 'myrank', 'ierror' ]}
            part_append_gensnode(ifinit, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': '.NOT. isopen .AND. ( ierror .EQ. MPI_SUCCESS )'}
            ifopen = part_append_gensnode(ifinit, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': [ 'rankstr', '"(I10)"' ], 'items': [ 'myrank' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'specs': [ 'threadstr', '"(I3)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': [ 'invokestr', '"(I16)"' ], 'items': [ 'kgen_invokes(OMP_GET_THREAD_NUM())' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': ['UNIT=myunit', 'RECL=%d'%RECL, 'FILE="%s/coverage.data." // TRIM(ADJUSTL(rankstr)) // \
                    "." // TRIM(ADJUSTL(threadstr)) // "." // TRIM(ADJUSTL(invokestr))' % os.path.abspath(getinfo('coverage_path')), \
                    'STATUS="REPLACE"', 'ACCESS="DIRECT"', 'FORM="FORMATTED"', 'ACTION="READWRITE"','IOSTAT=ierror']}
                part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)

            else:

                attrs = {'specs': [ 'invokestr', '"(I16)"' ], 'items': [ 'kgen_invokes' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': ['UNIT=myunit', 'RECL=%d'%RECL, 'FILE="%s/coverage.data." // TRIM(ADJUSTL(rankstr)) // \
                    ".-1." // TRIM(ADJUSTL(invokestr)) ' % os.path.abspath(getinfo('coverage_path')), \
                    'STATUS="REPLACE"', 'ACCESS="DIRECT"', 'FORM="FORMATTED"', 'ACTION="READWRITE"','IOSTAT=ierror']}
                part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)


            attrs = {'loopcontrol': 'idx1=0,%d-1'%maxfiles}
            dofile = part_append_gensnode(ifopen, EXEC_PART, block_statements.Do, attrs=attrs)

            attrs = {'loopcontrol': 'idx2=0,%d-1'%maxlines}
            doline = part_append_gensnode(dofile, EXEC_PART, block_statements.Do, attrs=attrs)

            attrs = {'specs': [ 'UNIT=myunit', 'REC=(%d * idx1 + idx2 + 1)'%maxlines, 'FMT="(I%d)"'%RECL ], 'items': [ '0' ] }
            part_append_gensnode(doline, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'UNIT=myunit' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Flush, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': 'isopen .AND. ( ierror .EQ. MPI_SUCCESS )'}
            ifopen2 = part_append_gensnode(ifinit, EXEC_PART, block_statements.IfThen, attrs=attrs)

            #attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
            #part_append_gensnode(ifopen2, EXEC_PART, statements.Call, attrs=attrs)

        else:

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': '.NOT. isopen'}
            ifopen = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'specs': [ 'threadstr', '"(I3)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': [ 'invokestr', '"(I16)"' ], 'items': [ 'kgen_invokes(OMP_GET_THREAD_NUM())' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': ['UNIT=myunit', 'RECL=%d'%RECL, 'FILE="%s/coverage.data.-1." // \
                    TRIM(ADJUSTL(threadstr)) // "." // TRIM(ADJUSTL(invokestr))' % os.path.abspath(getinfo('coverage_path')), \
                    'STATUS="REPLACE"', 'ACCESS="DIRECT"', 'FORM="FORMATTED"', 'ACTION="READWRITE"','IOSTAT=ierror']}
                part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)

            else:
                attrs = {'specs': [ 'invokestr', '"(I16)"' ], 'items': [ 'kgen_invokes' ]}
                part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'specs': ['UNIT=myunit', 'RECL=%d'%RECL, 'FILE="%s/coverage.data.-1.-1." // \
                    TRIM(ADJUSTL(invokestr))' % os.path.abspath(getinfo('coverage_path')), \
                    'STATUS="REPLACE"', 'ACCESS="DIRECT"', 'FORM="FORMATTED"', 'ACTION="READWRITE"','IOSTAT=ierror']}
                part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'loopcontrol': 'idx1=0,%d-1'%maxfiles}
            dofile = part_append_gensnode(ifopen, EXEC_PART, block_statements.Do, attrs=attrs)

            attrs = {'loopcontrol': 'idx2=0,%d-1'%maxlines}
            doline = part_append_gensnode(dofile, EXEC_PART, block_statements.Do, attrs=attrs)

            attrs = {'specs': [ 'UNIT=myunit', 'REC=(%d * idx1 + idx2 + 1)'%maxlines, 'FMT="(I%d)"'%RECL ], 'items': [ '0' ] }
            part_append_gensnode(doline, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': 'isopen'}
            ifopen2 = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            #attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
            #part_append_gensnode(ifopen2, EXEC_PART, statements.Call, attrs=attrs)


        attrs = {'specs': [ 'UNIT=myunit', 'REC=(%d * fileid + lineid + 1)'%maxlines, 'FMT="(I%d)"'%RECL ], 'items': [ 'visit' ]}
        part_append_gensnode(ifopen2, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'variable': 'visit', 'sign': '=', 'expr': 'visit + 1'}
        part_append_gensnode(ifopen2, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': [ 'UNIT=myunit', 'REC=(%d * fileid + lineid + 1)'%maxlines, 'FMT="(I%d)"'%RECL ], 'items': [ 'visit' ]}
        part_append_gensnode(ifopen2, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': [ 'UNIT=myunit' ]}
        part_append_gensnode(ifopen2, EXEC_PART, statements.Flush, attrs=attrs)

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'END CRITICAL (kgen_cover)', style='openmp')

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

