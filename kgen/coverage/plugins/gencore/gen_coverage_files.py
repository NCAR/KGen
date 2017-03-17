
import os
import random
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

RECL = 10

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.paths = collections.OrderedDict()
        self.logger = getinfo('logger')

    def get_filepath(self, fileid):
        for filepath, (fid, lines) in self.paths.items():
            if fileid == fid:
                return str(filepath)

    def get_linepairs(self, fileid):
        for filepath, (fid, lines) in self.paths.items():
            if fileid == fid:
                return [ '%s:%s'%(str(linenum), str(lineid)) for linenum, lineid in lines.items() ]

    def get_linenum(self, fileid, lineid):
        for filepath, (fid, lines) in self.paths.items():
            if fileid == fid:
                for linenum, lid in lines.items():
                    if lineid == lid:
                        return str(linenum)

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
        with open('%s/kcover_filemap.txt'%os.path.abspath(getinfo('coverage_path')), 'w') as f:
            for filepath, (fileid, lines) in self.paths.items():
                f.write('%d %s %s\n'%(fileid, filepath, ' '.join([str(linenum) for linenum in lines.keys()])))
        #setinfo('coverage_paths', self.paths)

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

        if maxfiles == 0:
            self.logger.warn('There is no valid conditional block.')
            return

        maxlines = max([ len(lineids) for fileid, lineids in self.paths.values() ])

        # add subroutine
        attrs = {'name': 'gen_coverage', 'args': ['fileid', 'lineid']}
        coversubr = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.Subroutine, attrs=attrs)
       
        pathlines = []
        for fileid in range(maxfiles):
            pathlines.append('"%s %s"'%(self.get_filepath(fileid), ' '.join(self.get_linepairs(fileid))))

        attrs = {'type_spec': 'CHARACTER', 'selector':('*', None), 'attrspec': [ 'PARAMETER', \
            'DIMENSION(0:%d)'%(maxfiles-1) ], 'entity_decls': ['headlines = (/ %s /)'%','.join(pathlines)]}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('4096', None), 'entity_decls': ['filepath']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['filestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['linestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

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

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'PARAMETER' ], 'entity_decls': \
                ['unitstart = %d'%random.randrange(100000, 999999)]}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        else:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myunit', 'mytid']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['isopen']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['fileid', 'lineid']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_invokes', ) ) ]}
        part_append_gensnode(coversubr, DECL_PART, statements.Common, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['now']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Real, attrs=attrs)

        ############# exec_part ########################

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'CRITICAL (kgen_cover)', style='openmp')

        attrs = {'variable': 'myunit', 'sign': '=', 'expr': 'unitstart + fileid * %d + lineid'%maxlines}
        part_append_gensnode(coversubr, EXEC_PART, statements.Assignment, attrs=attrs)

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

            attrs = {'specs': [ 'filestr', '"(I6)"' ], 'items': [ 'fileid' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'linestr', '"(I6)"' ], 'items': [ 'lineid' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'filepath', '*' ], 'items': [ '"%s/coverage.data." // TRIM(ADJUSTL(rankstr)) // "." \
                // TRIM(ADJUSTL(filestr)) // "." // TRIM(ADJUSTL(linestr))' % os.path.abspath(getinfo('coverage_path')) ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'FILE=TRIM(ADJUSTL(filepath))', \
                'STATUS="REPLACE"', 'ACCESS="APPEND"', 'FORM="FORMATTED"', 'ACTION="WRITE"','IOSTAT=ierror']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)

            #attrs = {'specs': [ 'UNIT=myunit', 'FMT="(A,A,A)"' ], 'items': [ '"%s"'%self.get_filepath(fileid), '" "', '"%s"'%' '.join(self.get_linepairs(fileid)) ]}
            attrs = {'specs': [ 'UNIT=myunit', 'FMT="(A)"' ], 'items': [ 'TRIM(ADJUSTL(headlines(fileid)))']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(ifinit, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': 'isopen .AND. ( ierror .EQ. MPI_SUCCESS )'}
            ifopen2 = part_append_gensnode(ifinit, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
            part_append_gensnode(ifopen2, EXEC_PART, statements.Call, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'variable': 'mytid', 'sign': '=', 'expr': 'OMP_GET_THREAD_NUM()'}
                part_append_gensnode(ifopen2, EXEC_PART, statements.Assignment, attrs=attrs)

                attrs = {'specs': [ 'UNIT=myunit', 'FMT="(I4,1X,I10,1X,F16.7)"' ], 'items': [ 'mytid', \
                    'kgen_invokes(OMP_GET_THREAD_NUM())', 'now' ]}
            else:
                attrs = {'specs': [ 'UNIT=myunit', 'FMT="(I4,1X,I10,1X,F16.7)"' ], 'items': [ '0', \
                    'kgen_invokes', 'now' ]}

            part_append_gensnode(ifopen2, EXEC_PART, statements.Write, attrs=attrs)

        else:
            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': '.NOT. isopen'}
            ifopen = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': [ 'filestr', '"(I6)"' ], 'items': [ 'fileid' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'linestr', '"(I6)"' ], 'items': [ 'lineid' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'filepath', '*' ], 'items': [ '"%s/coverage.data.0." \
                // TRIM(ADJUSTL(filestr)) // "." // TRIM(ADJUSTL(linestr))' % os.path.abspath(getinfo('coverage_path')) ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'FILE=TRIM(ADJUSTL(filepath))', \
                'STATUS="REPLACE"', 'ACCESS="APPEND"', 'FORM="FORMATTED"', 'ACTION="WRITE"','IOSTAT=ierror']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'specs': [ 'UNIT=myunit', 'FMT="(A)"' ], 'items': [ 'TRIM(ADJUSTL(headlines(fileid)))']}
            #attrs = {'specs': [ 'UNIT=myunit', 'FMT="(A,A,A)"' ], 'items': [ '"%s"'%self.get_filepath(fileid), '" "', '"%s"'%' '.join(self.get_linepairs(fileid)) ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=myunit', 'OPENED=isopen']}
            part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

            attrs = {'expr': 'isopen'}
            ifopen2 = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'designator': 'cpu_time', 'items': [ 'now' ]}
            part_append_gensnode(ifopen2, EXEC_PART, statements.Call, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'variable': 'mytid', 'sign': '=', 'expr': 'OMP_GET_THREAD_NUM()'}
                part_append_gensnode(ifopen2, EXEC_PART, statements.Assignment, attrs=attrs)

                attrs = {'specs': [ 'UNIT=myunit', 'FMT="(I4,1X,I10,1X,F16.7)"' ], 'items': [ 'mytid', \
                    'kgen_invokes(OMP_GET_THREAD_NUM())', 'now' ]}
                part_append_gensnode(ifopen2, EXEC_PART, statements.Write, attrs=attrs)

            else:
                attrs = {'specs': [ 'UNIT=myunit', 'FMT="(I4,1X,I10,1X,F16.7)"' ], 'items': [ '0', \
                    'kgen_invokes', 'now' ]}
                part_append_gensnode(ifopen2, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': [ 'UNIT=myunit' ]}
        part_append_gensnode(ifopen2, EXEC_PART, statements.Flush, attrs=attrs)

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'END CRITICAL (kgen_cover)', style='openmp')
