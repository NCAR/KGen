
import os
import random
import json
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

BEFORE_CALLSITE = 'before_callsite'
AFTER_CALLSITE = 'after_callsite'

class Gen_PapiCounter_File(Kgen_Plugin):

    def __init__(self):
        self.frame_msg = None
        self.paths = collections.OrderedDict()
        self.logger = getinfo('logger')

    # registration
    def register(self, msg):

        self.frame_msg = msg

        # when begin process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.save_maps)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            getinfo('parentblock_stmt'), None, self.save_papi)

    ##################################
    # printing paths
    ##################################

    def save_maps(self, node):

        # generate metadata.json for coverage
        if os.path.exists('%s/__data__/modeltypes'%getinfo('model_path')):
            json_data = None
            with open('%s/__data__/modeltypes'%getinfo('model_path'), 'r') as cm:
                json_data = json.load(cm)
                if u'"%s"'%getinfo('papi_typeid') not in json_data[u'datamap']:
                    json_data[u'datamap'][u'"%s"'%getinfo('papi_typeid')] = u'"%s"'%getinfo('papi_typename')

            with open('%s/__data__/modeltypes'%getinfo('model_path'), 'w') as cm:
                cm.write(json.dumps(json_data))
        else:
            with open('%s/__data__/modeltypes'%getinfo('model_path'), 'w') as fm:
                fm.write(u'{"datatype": "model", "datamap": { "%s": "%s" }}\n'%\
                    (getinfo('papi_typeid'), getinfo('papi_typename')))


    def save_papi(self, node):

        # add type decls for timers

        datapath = '%s/__data__'%getinfo('model_path')
        papipath = '%s/%s'%(datapath, getinfo('papi_typeid'))

        # mpi_wtime, mpi_wtick
        # omp_get_wtime, omp_get_wtick
        # system_clock
        # PAPI_L1_TCM, PAPI_L2_TCM, PAPI_L3_TCM
        # PAPIF_start_counters
        # PAPIF_read_counters
        # PAPIF_stop_counters
        # PAPI_OK

        attrs = {'type_spec': 'CHARACTER', 'selector':('4096', None), 'entity_decls': ['datapath']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Character, attrs=attrs)

        if getinfo('is_mpi_app'):

            for mod_name, use_names in getinfo('mpi_use'):
                attrs = {'name':mod_name, 'isonly': True, 'items':use_names}
                part_append_gensnode(node, USE_PART, statements.Use, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'SAVE' ], 'entity_decls': ['kgen_initialized = .FALSE.']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['myrank', 'ierror']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'selector': (None, '8'), 'entity_decls': ['MPI_WTIME', 'MPI_WTICK']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        if getinfo('is_openmp_app'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'selector': (None, '8'), \
                'attrspec': [ 'DIMENSION(0:%d, 0:2)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_timer']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE', 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['kgen_invokes = 0']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE', 'DIMENSION(0:%d)'%(getinfo('openmp_maxthreads')-1) ], 'entity_decls': ['dataunit']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)
        else:

            attrs = {'type_spec': 'REAL', 'selector': (None, '8'), \
                'attrspec': [ 'DIMENSION(0:2)' ], 'entity_decls': ['kgen_timer']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'SAVE' ], 'entity_decls': [ 'kgen_invokes = 0']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'selector': ('8', None), \
                'entity_decls': ['kgen_start_clock', 'kgen_stop_clock', 'kgen_count_max', 'kgen_rate_clock']}
            part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['dataunit']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        #attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['numthreadsstr']}
        #part_append_gensnode(node, DECL_PART, typedecl_statements.Character, attrs=attrs)

        #attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'invokes', 'visits', 'intnum' ]}
        #part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        #attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['istrue']}
        #part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('10', None), 'entity_decls': ['rankstr']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Character, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'selector':('6', None), 'entity_decls': ['threadstr']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Character, attrs=attrs)


        # ????measure overhead of timing measurement

        stmts = getinfo('callsite_stmts')

        topobj = stmts[0].parent.genspair

        index, partname, part = get_part_index(stmts[0].genspair)
        namedpart_create_subpart(topobj, BEFORE_CALLSITE, EXEC_PART, index=index)

        index, partname, part = get_part_index(stmts[-1].genspair)
        namedpart_create_subpart(topobj, AFTER_CALLSITE, EXEC_PART, index=index+1)
        
        if getinfo('is_mpi_app'):

            attrs = {'designator': 'MPI_INITIALIZED', 'items': [ 'kgen_initialized', 'ierror' ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'expr': 'kgen_initialized .AND. ( ierror .EQ. 0 )'}
            topobj = part_append_gensnode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if getinfo('is_openmp_app'):
                part_append_comment(topobj, EXEC_PART, 'CRITICAL (kgen_papi)', style='openmp')

            attrs = {'designator': 'MPI_COMM_RANK', 'items': [ getinfo('mpi_comm'), 'myrank', 'ierror' ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Call, attrs=attrs)

            attrs = {'specs': [ 'rankstr', '"(I10)"' ], 'items': [ 'myrank' ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)


            if getinfo('is_openmp_app'):

                attrs = {'specs': [ 'threadstr', '"(I6)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
                part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'kgen_invokes(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_invokes(OMP_GET_THREAD_NUM()) + 1'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # start timer before callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 0)', 'sign': '=', 'expr': 'MPI_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 1)', 'sign': '=', 'expr': 'MPI_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 2)', 'sign': '=', 'expr': 'MPI_WTICK()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)


            else:

                attrs = {'specs': [ 'threadstr', '"(I1)"' ], 'items': [ '0' ]}
                part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'kgen_invokes', 'sign': '=', 'expr': 'kgen_invokes + 1'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # start timer before callsite
                attrs = {'variable': 'kgen_timer(0)', 'sign': '=', 'expr': 'MPI_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(1)', 'sign': '=', 'expr': 'MPI_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(2)', 'sign': '=', 'expr': 'MPI_WTICK()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)


        else:

            attrs = {'specs': [ 'rankstr', '"(I1)"' ], 'items': [ '0' ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

            if getinfo('is_openmp_app'):

                attrs = {'specs': [ 'threadstr', '"(I6)"' ], 'items': [ 'OMP_GET_THREAD_NUM()' ]}
                part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'kgen_invokes(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': 'kgen_invokes(OMP_GET_THREAD_NUM()) + 1'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # start timer before callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 0)', 'sign': '=', 'expr': 'OMP_GET_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 1)', 'sign': '=', 'expr': 'OMP_GET_WTIME()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(OMP_GET_THREAD_NUM(), 2)', 'sign': '=', 'expr': 'OMP_GET_WTICK()'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)


            else:

                attrs = {'specs': [ 'threadstr', '"(I1)"' ], 'items': [ '0' ]}
                part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'kgen_invokes', 'sign': '=', 'expr': 'kgen_invokes + 1'}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # start timer before callsite
                attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_start_clock', 'kgen_rate_clock', 'kgen_count_max']}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Call, attrs=attrs)

                attrs = {'variable': 'kgen_timer(0)', 'sign': '=', 'expr': 'REAL(kgen_start_clock) / REAL(kgen_rate_clock) '}
                namedpart_append_gensnode(node.kgen_kernel_id, BEFORE_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite

                # start timer before callsite
                attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_stop_clock', 'kgen_rate_clock', 'kgen_count_max']}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Call, attrs=attrs)

                attrs = {'variable': 'kgen_timer(1)', 'sign': '=', 'expr': 'REAL(kgen_start_clock) / REAL(kgen_rate_clock) '}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)

                # stop timer after callsite
                attrs = {'variable': 'kgen_timer(2)', 'sign': '=', 'expr': '1.0 / REAL(kgen_rate_clock)'}
                namedpart_append_gensnode(node.kgen_kernel_id, AFTER_CALLSITE, statements.Assignment, attrs=attrs)


        # append mpirank.ompthread: invocation, starttime, stoptime, resolution

        if getinfo('is_openmp_app'):

            attrs = {'specs': [ 'datapath', '*' ], 'items': [ '"%s/" // TRIM(ADJUSTL(rankstr)) // "." // TRIM(ADJUSTL(threadstr))'%papipath ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['NEWUNIT=dataunit(OMP_GET_THREAD_NUM())', 'FILE=TRIM(ADJUSTL(datapath))', 'ACTION="WRITE"', 'ACCESS="APPEND"', 'IOSTAT=ierror']}
            part_append_gensnode(topobj, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'expr': 'ierror .EQ. 0'}
            ifopen = part_append_gensnode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': ['UNIT=dataunit(OMP_GET_THREAD_NUM())', 'FMT="(I16,1X,ES,1X,ES,1X,ES)"' ], \
                'items': [ 'kgen_invokes(OMP_GET_THREAD_NUM())', 'kgen_timer(OMP_GET_THREAD_NUM(), 0)', \
                'kgen_timer(OMP_GET_THREAD_NUM(), 1)', 'kgen_timer(OMP_GET_THREAD_NUM(), 2)' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=dataunit(OMP_GET_THREAD_NUM())']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Close, attrs=attrs)

            part_append_gensnode(ifopen, EXEC_PART, statements.Else)

            attrs = {'items': ['"FILE OPEN ERROR: "', 'TRIM(ADJUSTL(datapath))', 'ierror']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Print, attrs=attrs)

            part_append_comment(topobj, EXEC_PART, 'END CRITICAL (kgen_papi)', style='openmp')

        else:

            attrs = {'specs': [ 'datapath', '*' ], 'items': [ '"%s/" // TRIM(ADJUSTL(rankstr)) // "." // TRIM(ADJUSTL(threadstr))'%papipath ]}
            part_append_gensnode(topobj, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['NEWUNIT=dataunit', 'FILE=TRIM(ADJUSTL(datapath))', 'ACTION="WRITE"', 'ACCESS="APPEND"', 'IOSTAT=ierror']}
            part_append_gensnode(topobj, EXEC_PART, statements.Open, attrs=attrs)

            attrs = {'expr': 'ierror .EQ. 0'}
            ifopen = part_append_gensnode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'specs': ['UNIT=dataunit', 'FMT="(I16,1X,ES,1X,ES,1X,ES)"' ], \
                'items': [ 'kgen_invokes', 'kgen_timer(0)', \
                'kgen_timer(1)', 'kgen_timer(2)' ]}
            part_append_gensnode(ifopen, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'specs': ['UNIT=dataunit']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Close, attrs=attrs)

            part_append_gensnode(ifopen, EXEC_PART, statements.Else)

            attrs = {'items': ['"FILE OPEN ERROR: "', 'TRIM(ADJUSTL(datapath))', 'ierror']}
            part_append_gensnode(ifopen, EXEC_PART, statements.Print, attrs=attrs)

