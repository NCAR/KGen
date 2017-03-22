
import os
import random
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

RECL = 10
META = 'metadata.json'

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
                return [ '""%s"":""%s""'%(str(lineid), str(linenum)) for linenum, lineid in lines.items() ]

    def get_linenumbers(self, fileid):
        for filepath, (fid, lines) in self.paths.items():
            if fileid == fid:
                numbers = []
                for idx, linenum in enumerate(lines.keys()):
                    if idx != lines[linenum]:
                        raise Exception('Line number order mismatch.')
                    numbers.append( str(linenum) )
                return numbers

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
        with open('%s/kcover_filemap.txt'%getinfo('coverage_path'), 'w') as f:
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

        part_append_comment(node.kgen_parent, UNIT_PART, '')

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

        part_append_comment(node.kgen_parent, UNIT_PART, '')

        # add subroutine
        attrs = {'name': 'gen_coverage', 'args': ['fileid', 'lineid']}
        coversubr = part_append_gensnode(node.kgen_parent, UNIT_PART, block_statements.Subroutine, attrs=attrs)
  
        lmap = []
        spath = []
        for fileid in range(maxfiles):

            attrs = {'type_spec': 'CHARACTER', 'selector':('*', None), 'attrspec': [ 'PARAMETER' ], \
                'entity_decls': [ 'srcpath_%d = """%s"""'%(fileid, self.get_filepath(fileid)) ]}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'selector':('*', None), 'attrspec': [ 'PARAMETER' ], \
                'entity_decls': [ 'linemap_%d = %s'%(fileid, '"%s"'%', '.join(self.get_linepairs(fileid))) ]}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

            spath.append('srcpath_%d'%fileid)
            lmap.append('linemap_%d'%fileid)

        attrs = {'type_spec': 'CHARACTER', 'selector':('*', None), 'attrspec': [ 'PARAMETER', 'DIMENSION(0:%d)'%(maxfiles-1)], \
            'entity_decls': ['linemap = (/ %s /)'%', '.join(lmap)]}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('*', None), 'attrspec': [ 'PARAMETER', 'DIMENSION(0:%d)'%(maxfiles-1)], \
            'entity_decls': [ 'srcpaths = (/ %s /)'%', '.join(spath)]}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        part_append_comment(coversubr, DECL_PART, '')

        attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'SAVE' ], 'entity_decls': [ 'kgen_initialized = .FALSE.' ]}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('4096', None), 'entity_decls': ['filepath']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['filestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['linestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('10', None), 'entity_decls': ['rankstr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['threadstr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('16', None), 'entity_decls': ['invokestr']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)

        if getinfo('is_mpi_app'):

            for mod_name, use_names in getinfo('mpi_use'):
                attrs = {'name':mod_name, 'isonly': True, 'items':use_names}
                part_append_gensnode(coversubr, USE_PART, statements.Use, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_mpi_initialized']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myrank', 'numranks', 'ierror']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'CHARACTER', 'selector':('10', None), 'entity_decls': ['rankstr', 'numranksstr']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Character, attrs=attrs)


        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM', 'OMP_GET_NUM_THREADS']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        else:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_invokes']}
            part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['visit']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['dataunit', 'codeunit', 'fileunit', 'lineunit', 'mpiunit', 'ompunit', 'invokeunit']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['istrue']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['fileid', 'lineid']}
        part_append_gensnode(coversubr, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_invokes', ) ) ]}
        part_append_gensnode(coversubr, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(coversubr, DECL_PART, '')

        ############# exec_part ########################

        datapath = '%s/__data__'%getinfo('coverage_path')
        codepath = '%s/%d'%(datapath, getinfo('coverage_typeid'))

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'CRITICAL (kgen_cover)', style='openmp')

        attrs = {'specs': [ 'filestr', '"(I6)"' ], 'items': [ 'fileid' ]}
        part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': [ 'linestr', '"(I6)"' ], 'items': [ 'lineid' ]}
        part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'MPI_INITIALIZED', 'items': [ 'kgen_mpi_initialized', 'ierror' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'expr': '.NOT. kgen_mpi_initialized .OR. ( ierror .NE. MPI_SUCCESS )'}
            ifinit = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

            part_append_gensnode(ifinit, EXEC_PART, statements.Return)

            attrs = {'designator': 'MPI_COMM_RANK', 'items': [ getinfo('mpi_comm'), 'myrank', 'ierror' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'specs': [ 'rankstr', '"(I6)"' ], 'items': [ 'myrank' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'specs': [ 'rankstr', '"(I1)"' ], 'items': [ '0' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'specs': [ 'threadstr', '"(I6)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'specs': [ 'threadstr', '"(I1)"' ], 'items': [ '0' ]}
            part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': [ 'filepath', '*' ], 'items': [ '"%s/" // TRIM(ADJUSTL(filestr)) // "/" // \
TRIM(ADJUSTL(linestr)) // "/" // TRIM(ADJUSTL(rankstr)) // "/" // TRIM(ADJUSTL(threadstr))'%codepath ]}
        part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['FILE=TRIM(ADJUSTL(filepath)) // "/%s"'%META, 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. istrue'}
        iffilepath = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': 'SYSTEM', 'items': [ '"mkdir -p " // TRIM(ADJUSTL(filepath))']}
        part_append_gensnode(iffilepath, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'expr': '.NOT. kgen_initialized'}
        ifkgeninit = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        # coverage/__data__/codeid/fileid/lineid/mpiid/openmpid/: [ invokenum, invokenum, ... ]
        # invokenum : numinvokes

        # in datapath
        attrs = {'specs': ['NEWUNIT=dataunit', 'FILE="%s/%s"'%(datapath, META), \
            'STATUS="REPLACE"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Open, attrs=attrs)
        datapath_json = []
        datapath_json.append(u'""datatype"":""coverage""')
        datapath_json.append(u'""datamap"":{""%d"":""%s""}'%(getinfo('coverage_typeid'), getinfo('coverage_typename')))
        attrs = {'specs': [ 'UNIT=dataunit', 'FMT="(A)"' ], 'items': [ u'"{ %s }"'%', '.join(datapath_json) ]}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=dataunit']}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Close, attrs=attrs)

        # in codepath
        attrs = {'specs': ['NEWUNIT=codeunit', 'FILE="%s/%s"'%(codepath, META), \
            'STATUS="REPLACE"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Open, attrs=attrs)

        codepath_json = []
        codepath_json.append(u'""datatype"":""srcfile""')
        filemapstr = ',&\n&'.join([ '""%d"":""%s""'%(fid,fpath) for fpath, (fid, lines) in self.paths.items() ])
        codepath_json.append(u'""datamap"":{%s}'%filemapstr)
        attrs = {'specs': [ 'UNIT=codeunit', 'FMT="(A)"' ], 'items': [ u'"{ %s }"'%', '.join(codepath_json) ]}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=codeunit']}
        part_append_gensnode(ifkgeninit, EXEC_PART, statements.Close, attrs=attrs)

        # in file id
        attrs = {'specs': ['FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/%s"'%(codepath, META), 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. istrue'}
        iffilejson = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=fileunit', 'FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/%s"'%(codepath, META), \
            'STATUS="REPLACE"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(iffilejson, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'specs': [ 'UNIT=fileunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""codeline"", ""datamap"":{ " //  linemap(fileid) // " }}"' ]}
        part_append_gensnode(iffilejson, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=fileunit']}
        part_append_gensnode(iffilejson, EXEC_PART, statements.Close, attrs=attrs)

        # in line id
        attrs = {'specs': ['FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/" // TRIM(ADJUSTL(linestr)) // \
"/%s"'%(codepath, META), 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. istrue'}
        iflinejson = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=lineunit', 'FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/" // TRIM(ADJUSTL(linestr)) // \
"/%s"'%(codepath, META), \
            'STATUS="REPLACE"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(iflinejson, EXEC_PART, statements.Open, attrs=attrs)

        if getinfo('is_mpi_app'):
            attrs = {'designator': 'MPI_COMM_SIZE', 'items': [ getinfo('mpi_comm'), 'numranks', 'ierror' ]}
            part_append_gensnode(iflinejson, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'specs': [ 'numranksstr', '"(I10)"' ], 'items': [ 'numranks' ]}
            part_append_gensnode(iflinejson, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'UNIT=lineunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""mpi"", ""numranks"":"" // TRIM(ADJUSTL(numranksstr)) // ""}"' ]}
            part_append_gensnode(iflinejson, EXEC_PART, statements.Write, attrs=attrs)

        else:
            attrs = {'specs': [ 'UNIT=lineunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""mpi"", ""numranks"":""1""}"' ]}
            part_append_gensnode(iflinejson, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=lineunit']}
        part_append_gensnode(iflinejson, EXEC_PART, statements.Close, attrs=attrs)

        # in mpi id
        attrs = {'specs': ['FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/" // TRIM(ADJUSTL(linestr)) // \
"/" // TRIM(ADJUSTL(rankstr)) // "/%s"'%(codepath, META), 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. istrue'}
        ifmpijson = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=mpiunit', 'FILE="%s/" // TRIM(ADJUSTL(filestr)) // "/" // TRIM(ADJUSTL(linestr)) // \
"/" // TRIM(ADJUSTL(rankstr)) // "/%s"'%(codepath, META), \
            'STATUS="REPLACE"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(ifmpijson, EXEC_PART, statements.Open, attrs=attrs)

        if getinfo('is_openmp_app'):

            attrs = {'specs': [ 'numthreadsstr', '"(I6)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
            part_append_gensnode(ifmpijson, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': [ 'UNIT=mpiunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""openmp"", ""numrthreads"":"" // TRIM(ADJUSTL(numthreadsstr)) // ""}"' ]}
            part_append_gensnode(ifmpijson, EXEC_PART, statements.Write, attrs=attrs)

        else:
            attrs = {'specs': [ 'UNIT=mpiunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""openmp"", ""numthreads"":""1""}"']}
            part_append_gensnode(ifmpijson, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=mpiunit']}
        part_append_gensnode(ifmpijson, EXEC_PART, statements.Close, attrs=attrs)

        # in openmp id
        attrs = {'specs': ['FILE=TRIM(ADJUSTL(filepath)) // "/%s"'%META, 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': '.NOT. istrue'}
        ifompjson = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=ompunit', 'FILE=TRIM(ADJUSTL(filepath)) // "/%s"'%META, \
            'STATUS="NEW"', 'ACTION="WRITE"', 'FORM="FORMATTED"', 'ENCODING="UTF-8"', 'IOSTAT=ierror']}
        part_append_gensnode(ifompjson, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'specs': [ 'UNIT=ompunit', 'FMT="(A)"' ], 'items': [ u'"{""datatype"":""invocation""}"']}
        part_append_gensnode(ifompjson, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=ompunit']}
        part_append_gensnode(ifompjson, EXEC_PART, statements.Close, attrs=attrs)

        # in invoke
        attrs = {'specs': [ 'invokestr', '"(I16)"' ], 'items': [ 'kgen_invokes' ]}
        part_append_gensnode(coversubr, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['FILE=TRIM(ADJUSTL(filepath)) // "/" // TRIM(ADJUSTL(invokestr))', 'EXIST=istrue']}
        part_append_gensnode(coversubr, EXEC_PART, statements.Inquire, attrs=attrs)

        attrs = {'expr': 'istrue'}
        ifexist = part_append_gensnode(coversubr, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=invokeunit', 'FILE=TRIM(ADJUSTL(filepath)) // "/" // TRIM(ADJUSTL(invokestr))', \
            'STATUS="OLD"', 'FORM="FORMATTED"', 'ACCESS="DIRECT"', 'ACTION="READWRITE"', 'RECL=16', 'IOSTAT=ierror']}
        part_append_gensnode(ifexist, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'specs': [ 'UNIT=invokeunit', 'REC=1', 'FMT="(I16)"' ], 'items': [ 'visit' ]}
        part_append_gensnode(ifexist, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'variable': 'visit', 'sign': '=', 'expr': 'visit + 1'}
        part_append_gensnode(ifexist, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'specs': [ 'UNIT=invokeunit', 'REC=1', 'FMT="(I16)"' ], 'items': [ 'visit' ]}
        part_append_gensnode(ifexist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=invokeunit']}
        part_append_gensnode(ifexist, EXEC_PART, statements.Close, attrs=attrs)

        part_append_gensnode(ifexist, EXEC_PART, statements.Else)

        attrs = {'specs': ['NEWUNIT=invokeunit', 'FILE=TRIM(ADJUSTL(filepath)) // "/" // TRIM(ADJUSTL(invokestr))', \
            'STATUS="NEW"', 'FORM="FORMATTED"', 'ACCESS="DIRECT"', 'ACTION="WRITE"', 'RECL=16', 'IOSTAT=ierror']}
        part_append_gensnode(ifexist, EXEC_PART, statements.Open, attrs=attrs)

        attrs = {'specs': [ 'UNIT=invokeunit', 'REC=1', 'FMT="(A16)"' ], 'items': [ '"1"' ]}
        part_append_gensnode(ifexist, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['UNIT=invokeunit']}
        part_append_gensnode(ifexist, EXEC_PART, statements.Close, attrs=attrs)

        if getinfo('is_openmp_app'):
            part_append_comment(coversubr, EXEC_PART, 'END CRITICAL (kgen_cover)', style='openmp')

        part_append_comment(coversubr, EXEC_PART, '')
