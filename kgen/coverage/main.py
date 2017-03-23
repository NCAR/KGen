'''KGen coverage detector
'''

import os
import re
import glob
import json
import math
import shutil
import datetime
import collections
import multiprocessing
from collections import OrderedDict

from kgtool import KGTool
from parser.kgparse import KGGenType
from kggenfile import gensobj, KERNEL_ID_0, event_register, Gen_Statement, set_indent
import kgutils
from kgconfig import Config
try:
    import configparser
except:
    import ConfigParser as configparser

BEGIN_DATA_MARKER = r'kgpathbegin'
END_DATA_MARKER = r'kgpathend'
BEGIN_PATH_MARKER = r'kgdatabegin'
END_PATH_MARKER = r'kgdataend'

_DEBUG = True

def readdatafiles(inq, outq):
    visits = {}
    filemap = {}
    linemap = {}

    paths = inq.get()

    for idx, path in enumerate(paths):
        #kgutils.logger.info('%s starts to process "%s"'%(multiprocessing.current_process().name, path))

        try:
            fc = open(path, 'r')

            splitpath = path.split('.')
            ranknum = int(splitpath[-3])
            fileid = int(splitpath[-2])
            lineid = int(splitpath[-1])

            if fileid not in visits:
                visits[fileid] = {}
            if lineid not in visits[fileid]:
                visits[fileid][lineid] = {}
            if ranknum not in visits[fileid][lineid]:
                visits[fileid][lineid][ranknum] = {}

            # headline
            fpath, linepairs = fc.readline().strip().split(' ', 1)
            if fileid not in filemap:
                filemap[fileid] = fpath
            if fileid not in linemap:
                linemap[fileid] = {}
                for pair in linepairs.split():
                    try:
                        lnum, lid = tuple( int(num) for num in pair.split(':') )
                        linemap[fileid][lid] = lnum
                    except Exception as e:
                        kgutils.logger.info('ERROR 1 at %s: %s'%(multiprocessing.current_process().name, str(e)))

            # data lines
            threadnums = visits[fileid][lineid][ranknum]

            #import pdb; pdb.set_trace()
            for line in fc:
                visit = line.strip().split()
                threadnum, invokenum = tuple(int(num) for num in visit[:2])
                timestamp = float(visit[-1])

                if threadnum not in threadnums:
                    threadnums[threadnum] = {}
                if invokenum not in threadnums[threadnum]:
                    threadnums[threadnum][invokenum] = [ timestamp, timestamp ]
                else:
                    pass
                    curmin = threadnums[threadnum][invokenum][0]
                    curmax = threadnums[threadnum][invokenum][1]
                    threadnums[threadnum][invokenum] = [ min(curmin, timestamp), max(curmax, timestamp) ]
            fc.close()
        except Exception as e:
            kgutils.logger.info('ERROR 2 at %s: %s'%(multiprocessing.current_process().name, str(e)))
        finally:
            pass

    #kgutils.logger.info('%s has completed.'%multiprocessing.current_process().name)
 
    #inq.task_done()
    outq.put((visits, filemap, linemap))
    #outq.put()

class Coverage(KGTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting KCover')

        coverage_abspath = os.path.abspath('%s/%s'%(Config.path['outdir'], Config.path['coverage']))

        # create coverage directory
        if not os.path.exists(coverage_abspath):
            os.makedirs(coverage_abspath)

        # build app with instrumntation
        if not os.path.exists(Config.coveragefile) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:
            if len(glob.glob('%s/coverage.data.*'%coverage_abspath)) > 0 and Config.coverage['reuse_rawdata']:
                kgutils.logger.info('Raw data generation is skipped.')
            else:

                # rm data tree
                if os.path.exists('%s/__data__'%coverage_abspath):
                    shutil.rmtree('%s/__data__'%coverage_abspath)

                # create data tree
                os.makedirs('%s/__data__'%coverage_abspath)
                os.makedirs('%s/__data__/%s'%(coverage_abspath, Config.coverage['types']['code']['id']))
                os.makedirs('%s/__data__/__resource__'%coverage_abspath)
                os.makedirs('%s/__data__/__resource__/linemap'%coverage_abspath)

                # generate instrumentation
                for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():
                    if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                        sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                        if filepath == Config.callsite['filepath']:
                            sfile.used4coverage = True
                        else:
                            sfile.used4coverage = False
                        if sfile is None:
                            raise kgutils.ProgramException('Kernel source file is not generated for %s.'%filepath)
                        self.genfiles.append((sfile, filepath))
                        Config.used_srcfiles[filepath] = (sfile, mods_used, units_used)

                # process each nodes in the tree
                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('cover'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.created([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('cover'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.process([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('cover'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.finalize([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('cover'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.flatten(KERNEL_ID_0, [plugin_name])

                # generate source files from each node of the tree
                coverage_files = []
                for sfile, filepath in self.genfiles:
                    filename = os.path.basename(filepath)
                    if sfile.used4coverage:
                        set_indent('')
                        slines = sfile.tostring()
                        if slines is not None:
                            slines = kgutils.remove_multiblanklines(slines)
                            coverage_files.append(filename)
                            with open('%s/%s'%(coverage_abspath, filename), 'wb') as fd:
                                fd.write(slines)
                            with open('%s/%s.kgen'%(coverage_abspath, filename), 'wb') as ft:
                                ft.write('\n'.join(sfile.kgen_stmt.prep))

                self.gen_makefile()

                kgutils.logger.info('Instrumentation for coverage is generated at %s.'%coverage_abspath)


                # TODO: wait until coverage data generation is completed
                # use -K option fir bsub to wait for job completion

                # clean app
                if Config.cmd_clean['cmds']:
                    kgutils.run_shcmd(Config.cmd_clean['cmds'])
                    if Config.state_switch['clean']:
                        kgutils.run_shcmd(Config.state_switch['clean'])

                # TEMP
                out, err, retcode = kgutils.run_shcmd('make', cwd=coverage_abspath)
                if retcode == 0:

                    kgutils.logger.info('Application is built/run with coverage instrumentation.')

                    # collect data
                    attrs = { 'totalfiles': {}, 'totalblocks': {}, 'usedfiles': {}, 'usedblocks': {}, \
                        'numranks': '0', 'numthreads': '0', 'mpivisits': {}, 'ompvisits': {}, 'linevisits': {} }
                    invokes = {} # rank:thread:invoke:(fid, lnum)

                    # generate coverage file
                    self.visit('%s/__data__'%coverage_abspath, invokes, attrs)

                    print 'TTT', attrs

                    with open(Config.coveragefile, 'w') as fd:
                        # summary section
                        fd.write('[summary]\n')
                        fd.write('number_of_files_having_condblocks = %d\n'%len(attrs['totalfiles']))
                        fd.write('number_of_files_invoked = %d\n'%len(attrs['usedfiles']))
                        fd.write('number_of_condblocks_exist = %d\n'%sum( [ len(lines) for fid, lines in attrs['totalblocks'].items() ] ))
                        fd.write('number_of_condblocks_invoked = %d\n'%sum( [ len(lines) for fid, lines in attrs['usedblocks'].items() ] ))
                        fd.write('\n')

                        # file section
                        fd.write('[file]\n')
                        fd.write('; <file number> = <path to file>\n')
                        #for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                        for fileid, filepath in attrs['totalfiles'].items():
                            fd.write('%s = %s/%s.kgen\n'%(fileid, coverage_abspath, os.path.basename(filepath)))
                        fd.write('used_files = %s\n'%', '.join([ fid for fid in attrs['usedfiles'] ]))
                        fd.write('\n')

                        # block section
                        fd.write('[block]\n')
                        fd.write('; <file number> =  <line number> ...\n')
                        #for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                        for fileid, lines in attrs['totalblocks'].items():
                            fd.write('%s = %s\n'%(fileid, ', '.join(lines)))
                        fd.write('used_blocks = %s\n'%', '.join([ '%s:%s'%(fid,lnum) for lnum in lines for fid, lines in attrs['usedblocks'].items() ]))
                        fd.write('\n')


                        # invoke section
                        fd.write('[invoke]\n')
                        fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number><num of invocations> ...\n')

                        for ranknum, threadnums in invokes.items():
                            for threadnum, invokenums in threadnums.items():
                                for invokenum, triples in invokenums.items():
                                    fd.write('%s %s %s = %s\n'%(ranknum, threadnum, invokenum, \
                                        ', '.join( [ '%s:%s:%s'%(fid, lnum, nivks) for fid, lnum, nivks in triples ] )))

                    #kgutils.logger.info('KGen coverage file is generated: %s'%Config.coveragefile)
                    kgutils.logger.info('    ***** Within "%s" kernel *****:'%Config.kernel['name'])
                    kgutils.logger.info('    * %d original source files have conditional blocks.'%len(attrs['totalfiles']))
                    kgutils.logger.info('    * %d original source files are invoked at least once.'%len(attrs['usedfiles']))
                    kgutils.logger.info('    * %d conditional blocks exist in the original source files.'%\
                        sum( [ len(lines) for fid, lines in attrs['totalblocks'].items() ] ))
                    kgutils.logger.info('    * %d conditional blocks are executed at least once among all the conditional blocks.'%\
                        sum( [ len(lines) for fid, lines in attrs['usedblocks'].items() ] ))


                    for fileid, filepath in attrs['usedfiles'].items():
                        basefile = '%s/%s.kgen'%(coverage_abspath, os.path.basename(filepath))
                        with open(basefile, 'r') as fsrc:
                            srclines = fsrc.readlines()

                        filesummary = [ \
                            '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', \
                            '!! %d conditional blocks exist in this file'%len(attrs['totalblocks'][fileid]), \
                            '!! %d conditional blokcs are executed at least once among all the conditional blocks.'%len(attrs['usedblocks'][fileid]), \
                            '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' \
                        ]
                        srclines[0] = '%s\n%s\n'%('\n'.join(filesummary), srclines[0])

                        for linenum in attrs['usedblocks'][fileid]:
                            linevisit = [ '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' ]
                            linevisit.append('!! Total number of visits: %d'%attrs['linevisits'][fileid][linenum])
                            if Config.mpi['enabled']:
                                linevisit.append('!! MPI rank(visits)      : %s' % ' '.join( \
                                    ['%s(%d)'%(r,i) for r,i in attrs['mpivisits'][fileid][linenum].items()]))
                            if Config.openmp['enabled']:
                                linevisit.append('!! OpenMP thread(visits) : %s' % ' '.join( \
                                    ['%s(%d)'%(t,i) for t,i in attrs['mpivisits'][fileid][linenum].items()]))
                            linevisit.append( '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )

                            srclines[int(linenum)-1] = '%s%s\n'%(srclines[int(linenum)-1], '\n'.join(linevisit))

                        coveragefile = '%s/%s.coverage'%(coverage_abspath, os.path.basename(filepath))
                        with open(coveragefile, 'w') as fdst:
                            fdst.write(''.join(srclines))

                else:
                    if not _DEBUG:
                        if os.path.exists(Config.coveragefile):
                            os.remove(Config.coveragefile)
                        shutil.rmtree(coverage_abspath)
                    kgutils.logger.info('Failed to generate coverage information: %s'%err)

        else:
            kgutils.logger.info('Reusing KGen coverage file: %s'%Config.coveragefile)

        if not os.path.exists(Config.coveragefile):
            kgutils.logger.warn('No coverage file is found.')
        else:
            # read ini file
            kgutils.logger.info('Reading %s'%Config.coveragefile)

            cfg = configparser.ConfigParser()
            cfg.optionxform = str
            cfg.read(Config.coveragefile)

            number_of_files_having_condblocks = int(cfg.get('summary', 'number_of_files_having_condblocks'))
            number_of_files_invoked = int(cfg.get('summary', 'number_of_files_invoked'))
            number_of_condblocks_exist = int(cfg.get('summary', 'number_of_condblocks_exist'))
            number_of_condblocks_invoked = int(cfg.get('summary', 'number_of_condblocks_invoked'))

            try:
                filemap = {}
                for opt in cfg.options('file'):
                    if opt.isdigit():
                        filemap[opt] = cfg.get('file', opt) 

                blockmap = {}
                for opt in cfg.options('block'):
                    if opt.isdigit():
                        blockmap[opt] =  tuple( linenum for linenum in cfg.get('block', opt).split() )

                # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number>:<num invokes> ... 
                invokemap = {}
                idx = 0
                for opt in cfg.options('invoke'):
                    idx += 1
                    ranknum, threadnum, invokenum = tuple( num for num in opt.split() )
                    optval = cfg.get('invoke', opt).split(',')
                    triples = tuple( triple.strip().split(':') for triple in optval )

                    invokenum = int(invokenum)
                    if invokenum not in invokemap:
                        invokemap[invokenum] = {}
                    if ranknum not in invokemap[invokenum]:
                        invokemap[invokenum][ranknum] = {}
                    if threadnum not in invokemap[invokenum][ranknum]:
                        threadnums = {}
                        invokemap[invokenum][ranknum][threadnum] = threadnums

                    #threadnums = invokemap[invokenum][ranknum][threadnum]
                    
                    for fidstr, lnumstr, numinvokes in triples:
                        fileid = fidstr
                        linenum = lnumstr
                        if fileid not in threadnums:
                            threadnums[fileid] = {}
                        if linenum not in threadnums[fileid]:
                            threadnums[fileid][linenum] = 0
                        threadnums[fileid][linenum] += int(numinvokes)

                    if idx % 100000 == 0:
                        print 'Processed %d items: %s'%(idx, datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y"))
            except Exception as e:
                raise Exception('Please check the format of coverage file: %s'%str(e))

            THREASHOLD = 0.99
            THREASHOLD_NUM = int(number_of_condblocks_invoked*THREASHOLD)
            collected = []
            triples = {}
            for invokenum in sorted(invokemap.keys()):
                if len(collected) > THREASHOLD_NUM: break
                ranknums = invokemap[invokenum]
                for ranknum in ranknums.keys():
                    if len(collected) > THREASHOLD_NUM: break
                    threadnums = invokemap[invokenum][ranknum]
                    for threadnum in threadnums.keys():
                        if len(collected) > THREASHOLD_NUM: break
                        fileids = invokemap[invokenum][ranknum][threadnum]
                        for fileid in fileids.keys():
                            if len(collected) > THREASHOLD_NUM: break
                            lnums = invokemap[invokenum][ranknum][threadnum][fileid]
                            for lnum, numinvokes in lnums.items():
                                if len(collected) > THREASHOLD_NUM: break
                                if (fileid, lnum) not in collected:
                                    collected.append((fileid, lnum))
                                    if (ranknum, threadnum, invokenum) not in triples:
                                        triples[(ranknum, threadnum, invokenum)] = None

            print 'At least, %s of conditional blocks will be excuted by using following (MPI ranks, OpenMP Threads, Invokes) triples:'%'{:.1%}'.format(THREASHOLD)
            print ','.join([ ':'.join([ str(n) for n in t ]) for t in triples.keys()])
            print ''
            print 'Following (File id, line number) pairs are covered by above triples:'
            print str(collected)

            for ranknum, threadnum, invokenum in triples.keys():
                Config.invocation['triples'].append( ( (str(ranknum), str(ranknum)), (str(threadnum), str(threadnum)), \
                    (str(invokenum), str(invokenum)) ) )

        if len(Config.invocation['triples']) == 0:
            Config.invocation['triples'].append( ( ('0', '0'), ('0', '0'), ('0', '0') ) )

    def write(self, f, line, n=True, t=False):
        nl = ''
        tab = ''
        if n: nl = '\n'
        if t: tab = '\t'
        f.write(tab + line + nl)

    def gen_makefile(self):

        coverage_abspath = os.path.abspath('%s/%s'%(Config.path['outdir'], Config.path['coverage']))

        org_files = [ filepath for filepath, (sfile, mods_used, units_used) in Config.used_srcfiles.iteritems() if sfile.used4coverage ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['filepath'])

        with open('%s/Makefile'%coverage_abspath, 'wb') as f:

            self.write(f, '# Makefile for KGEN-generated instrumentation')
            self.write(f, '')

            cwd = os.path.abspath(Config.cwd)

            prerun_build_str = ''
            if Config.prerun['build']:
                self.write(f, 'PRERUN_BUILD := %s'%Config.prerun['build'])
                prerun_build_str = '${PRERUN_BUILD}; '

            prerun_run_str = ''
            if Config.prerun['run']:
                self.write(f, 'PRERUN_RUN := %s'%Config.prerun['run'])
                prerun_run_str = '${PRERUN_RUN}; '

            self.write(f, '')

            if Config.cmd_run['cmds']>0:
                self.write(f, 'run: build')
                self.write(f, '%scd %s; %s'%(prerun_run_str, cwd, Config.cmd_run['cmds']), t=True)
            else:
                self.write(f, 'echo "No information is provided to run. Please specify run commands using \'state-run\' command line option"; exit -1', t=True)
            self.write(f, '')

            if Config.cmd_build['cmds']>0:
                self.write(f, 'build: %s'%Config.state_switch['type'])
                self.write(f, '%scd %s; %s'%(prerun_build_str, cwd, Config.cmd_build['cmds']), t=True)
                for org_file in org_files:
                    self.write(f, 'mv -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
            else:
                self.write(f, 'echo "No information is provided to build. Please specify build commands using \'state-build\' command line option"; exit -1', t=True)
            self.write(f, '')

            self.write(f, '%s: save'%Config.state_switch['type'])
            if Config.state_switch['type']=='replace':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
            elif Config.state_switch['type']=='copy':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'cp -f %s/%s %s'%(coverage_abspath, basename, Config.state_switch['directory']), t=True)
            self.write(f, '')

            self.write(f, 'recover:')
            for org_file in org_files:
                self.write(f, 'cp -f %s.kgen_org %s'%(os.path.basename(org_file), org_file), t=True)
            self.write(f, '')

            self.write(f, 'recover_from_srcdir:')
            for org_file in org_files:
                self.write(f, 'cp -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
            self.write(f, '')


            self.write(f, 'save:')
            for org_file in org_files:
                self.write(f, 'if [ ! -f %(f)s.kgen_org ]; then cp -f %(f)s %(f)s.kgen_org; fi'%{'f':org_file}, t=True)
                self.write(f, 'if [ ! -f %(g)s.kgen_org ]; then cp -f %(f)s %(g)s.kgen_org; fi'%{'f':org_file, 'g':os.path.basename(org_file)}, t=True)
            self.write(f, '')

            if Config.cmd_clean['cmds']>0:
                self.write(f, 'clean:')
                self.write(f, Config.cmd_clean['cmds'], t=True)
            self.write(f, '')

    def visit(self, path, invokes, attrs, ctype=None, fileid=None, linenum=None, mpirank=None, ompthread=None):

        if not os.path.exists('%s/metadata.json'%path):
            return

        metadata = None
        with open('%s/metadata.json'%path, 'r') as metafile:
            metadata = json.load(metafile)

        if metadata is None:
            return

        datatype = metadata.get('datatype', 'No datatype')
        if datatype == 'coverage':
            ctypes = metadata.get('datamap', {})
            for ctype, cname in ctypes.items():
                if os.path.exists('%s/%s'%(path, ctype)):
                    self.visit('%s/%s'%(path, ctype), invokes, attrs, ctype=ctype)

        elif datatype == 'srcfile':
            attrs['totalfiles'] = metadata.get('datamap', {})
            for fileid, srcpath in attrs['totalfiles'].items():
                if os.path.exists('%s/%s'%(path, fileid)):
                    attrs['usedfiles'][fileid] = srcpath

                    if fileid not in attrs['linevisits']:
                        attrs['linevisits'][fileid] = collections.OrderedDict()

                    if fileid not in attrs['mpivisits']:
                        attrs['mpivisits'][fileid] = collections.OrderedDict()

                    if fileid not in attrs['ompvisits']:
                        attrs['ompvisits'][fileid] = collections.OrderedDict()

                    self.visit('%s/%s'%(path, fileid), invokes, attrs, ctype=ctype, fileid=fileid)

        elif datatype == 'codeline':
            if fileid not in attrs['totalblocks']:
                attrs['totalblocks'][fileid] = []
            lineids = metadata.get('datamap', {})
            for lineid, linenum in lineids.items():
                attrs['totalblocks'][fileid].append(linenum)
                if os.path.exists('%s/%s'%(path, lineid)):
                    if fileid not in attrs['usedblocks']:
                        attrs['usedblocks'][fileid] = []
                    attrs['usedblocks'][fileid].append(linenum)

                    if linenum not in attrs['linevisits'][fileid]:
                        attrs['linevisits'][fileid][linenum] = 0

                    if linenum not in attrs['mpivisits'][fileid]:
                        attrs['mpivisits'][fileid][linenum] = collections.OrderedDict()

                    if linenum not in attrs['ompvisits'][fileid]:
                        attrs['ompvisits'][fileid][linenum] = collections.OrderedDict()

                    self.visit('%s/%s'%(path, lineid), invokes, attrs, ctype=ctype, fileid=fileid, linenum=linenum)

        elif datatype == 'mpi':
            numranks = metadata.get('numranks', '0')
            attrs['numranks'] = numranks
            for mpirank in range(int(numranks)):
                mpirank = str(mpirank)
                if os.path.exists('%s/%s'%(path, mpirank)):
                    if mpirank not in invokes:
                        invokes[mpirank] = collections.OrderedDict()

                    if mpirank not in attrs['mpivisits'][fileid][linenum]:
                        attrs['mpivisits'][fileid][linenum][mpirank] = 0

                    self.visit('%s/%s'%(path, mpirank), invokes, attrs, ctype=ctype, fileid=fileid, linenum=linenum, mpirank=mpirank)

        elif datatype == 'openmp':
            numthreads = metadata.get('numthreads', '0')
            attrs['numthreads'] = numthreads
            for ompthread in range(int(numthreads)):
                ompthread = str(ompthread)
                if os.path.exists('%s/%s'%(path, ompthread)):
                    if ompthread not in invokes[mpirank]:
                        invokes[mpirank][ompthread] = collections.OrderedDict()

                    if ompthread not in attrs['ompvisits'][fileid][linenum]:
                        attrs['ompvisits'][fileid][linenum][ompthread] = 0

                    self.visit('%s/%s'%(path, ompthread), invokes, attrs, ctype=ctype, fileid=fileid, linenum=linenum, mpirank=mpirank, ompthread=ompthread)

        elif datatype == 'invocation':
            for dfile in sorted(glob.glob('%s/*'%path)):
                invoke = dfile.split('/')[-1]
                if invoke.isdigit():
                    if invoke not in invokes[mpirank][ompthread]:
                        invokes[mpirank][ompthread][invoke] = []
                    with open('%s/%s'%(path, invoke), 'r') as f:

                        # invokes
                        numvisit = f.read().strip()
                        invokes[mpirank][ompthread][invoke].append((fileid, linenum, numvisit))

                        numvisit = int(numvisit)

                        # line visits
                        attrs['linevisits'][fileid][linenum] += numvisit
               
                        # mpi visits
                        attrs['mpivisits'][fileid][linenum][mpirank] += numvisit

                        # omp visits
                        attrs['ompvisits'][fileid][linenum][ompthread] += numvisit

        else:
            raise Exception('Unknown data type: %s'%datatype)




