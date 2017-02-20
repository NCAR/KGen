'''KGen coverage detector
'''

import os
import re
import glob
import math
import datetime
import collections
import multiprocessing

from kgtool import KGTool
from parser.kgparse import KGGenType
from kggenfile import gensobj, KERNEL_ID_0, event_register, Gen_Statement
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

DEBUG = True

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

        # create coverage directory
        if not os.path.exists(Config.path['coverage']):
            os.makedirs(Config.path['coverage'])

        # build app with instrumntation
        if not os.path.exists(Config.coveragefile) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            # generate instrumentation
            for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():
                if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                    sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
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

                for sfile, filepath in self.genfiles:
                    sfile.process([plugin_name])

                for sfile, filepath in self.genfiles:
                    sfile.finalize([plugin_name])

                for sfile, filepath in self.genfiles:
                    sfile.flatten(KERNEL_ID_0, [plugin_name])

            # generate source files from each node of the tree
            coverage_files = []
            for sfile, filepath in self.genfiles:
                filename = os.path.basename(filepath)
                if sfile.used4coverage:
                    self.set_indent('')
                    slines = sfile.tostring()
                    if slines is not None:
                        slines = self.remove_multiblanklines(slines)
                        coverage_files.append(filename)
                        with open('%s/%s'%(Config.path['coverage'], filename), 'wb') as fd:
                            fd.write(slines)
                        with open('%s/%s.kgen'%(Config.path['coverage'], filename), 'wb') as ft:
                            ft.write('\n'.join(sfile.kgen_stmt.prep))

            self.gen_makefile()

            kgutils.logger.info('Instrumentation for coverage is generated at %s.'%os.path.abspath(Config.path['coverage']))


            # TODO: wait until coverage data generation is completed
            # use -K option fir bsub to wait for job completion

            # clean app
            if Config.cmd_clean['cmds']:
                kgutils.run_shcmd(Config.cmd_clean['cmds'])

            # TEMP
            out, err, retcode = kgutils.run_shcmd('make', cwd=Config.path['coverage'])
            if retcode != 0:
                #kgutils.logger.info('Failed to generate coverage information: %s : %s'%(out, err))
                kgutils.logger.info('Failed to generate coverage information: %s'%err)

            kgutils.logger.info('Application is built/run with coverage instrumentation.')

            def chunks(l, n):
                for i in range(0, len(l), n):
                    yield l[i:i + n]


            def update(d, u):
                for k, v in u.items():
                    if isinstance(v, collections.Mapping):
                        r = update(d.get(k, {}), v)
                        d[k] = r
                    else:
                        d[k] = u[k]
                return d

            visits = {}  # fileid -> lineid -> ranknum -> threadnum -> invokenum -> [ timestamp, ... ]
            filemap = {} # fileid -> filepath
            linemap = {} # fileid -> lineid -> linenum
            datafiles = glob.glob('%s/coverage.data*'%Config.path['coverage'])
            kgutils.logger.info('%d coverage data files are found.'%len(datafiles))

            nprocs = min( len(datafiles), multiprocessing.cpu_count())

            if nprocs == 0:
                raise Exception('No coverage data files are found.')

            chunks = [ chunk for chunk in chunks(datafiles, int(math.ceil(len(datafiles)/nprocs))) ]
            inqs = []
            outqs = []
            for _ in range(nprocs):
                inqs.append(multiprocessing.Queue())
                outqs.append(multiprocessing.Queue())

            procs = []
            for idx in range(nprocs):
                proc = multiprocessing.Process(target=readdatafiles, args=(inqs[idx], outqs[idx]))
                procs.append(proc)
                proc.start()

            for inq, chunk in zip(inqs,chunks):
                inq.put(chunk)

            for outq in outqs:
                visit, fmap, lmap = outq.get()
                update(visits, visit)
                update(filemap, fmap)
                update(linemap, lmap)

            for idx in range(nprocs):
                procs[idx].join()

            number_of_files_having_condblocks = len(Config.plugindb['coverage_paths'])
            number_of_files_invoked = len(visits)
            number_of_condblocks_exist = sum( len(lines) for lines in linemap.values())
            try:
                number_of_condblocks_invoked = sum( len(lineids) for  lineids in visits.values() )
            except: import pdb; pdb.set_trace()

            with open(Config.coveragefile, 'w') as fd:
                # summary section
                fd.write('[summary]\n')
                fd.write('number_of_files_having_condblocks = %d\n'%number_of_files_having_condblocks)
                fd.write('number_of_files_invoked = %d\n'%number_of_files_invoked)
                fd.write('number_of_condblocks_exist = %d\n'%number_of_condblocks_exist)
                fd.write('number_of_condblocks_invoked = %d\n'%number_of_condblocks_invoked)
                fd.write('\n')

                # file section
                fd.write('[file]\n')
                fd.write('; <file number> = <path to file>\n')
                for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                    fd.write('%d = %s/%s.kgen\n'%(pathnum, os.path.abspath(Config.path['coverage']), os.path.basename(path)))
                fd.write('\n')

                # block section
                fd.write('[block]\n')
                fd.write('; <file number> =  <line number> ...\n')
                for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                    fd.write('%d = %s\n'%(pathnum, ' '.join([ str(lnum) for lnum, lid in lines.items() ])))
                fd.write('\n')


            #kgutils.logger.info('KGen coverage file is generated: %s'%Config.coveragefile)
            kgutils.logger.info('    ***** Within "%s" kernel *****:'%Config.kernel['name'])
            kgutils.logger.info('    * %d original source files have conditional blocks.'%number_of_files_having_condblocks)
            kgutils.logger.info('    * %d original source files are invoked at least once.'%number_of_files_invoked)
            kgutils.logger.info('    * %d conditional blocks exist in the original source files.'%number_of_condblocks_exist)
            kgutils.logger.info('    * %d conditional blocks are executed at least once among all the conditional blocks.'%\
                number_of_condblocks_invoked )

            for fileid, lines in visits.items():
                visitinfo = {}
                for lineid, ranks in lines.items():
                    rankvisits = {}
                    threadvisits = {}
                    for rank, threads in ranks.items(): 
                        rankvisits[rank] = sum( [ sum( len(ts) for ts in ivk.values()) for ivk in threads.values() ] )
                        for tid, ivk in threads.items():
                            if tid in threadvisits:
                                threadvisits[tid] += sum(len(ts) for ts in ivk.values())
                            else:
                                threadvisits[tid] = sum(len(ts) for ts in ivk.values())
                    totalvisits = sum(rankvisits.values())

                    visitinfo[lineid] = [ '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', \
                        '!! Total number of visits: %d'%totalvisits ]
                    if Config.mpi['enabled']:
                        visitinfo[lineid].append('!! MPI rank(visits)      : %s' % ' '.join(['%d(%d)'%(r,i) for r,i in rankvisits.items()]))
                    if Config.openmp['enabled']:
                        visitinfo[lineid].append('!! OpenMP thread(visits) : %s' % ' '.join(['%d(%d)'%(t,i) for t,i in threadvisits.items()]))
                    visitinfo[lineid].append( '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )

                basefile = '%s/%s.kgen'%(os.path.abspath(Config.path['coverage']), os.path.basename(filemap[fileid]))
                with open(basefile, 'r') as fsrc:
                    srclines = fsrc.readlines()

                filesummary = [ \
                    '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', \
                    '!! %d conditional blocks exist in this file'%len(linemap[fileid]), \
                    '!! %d conditional blokcs are executed at least once among all the conditional blocks.'%len(lines), \
                    '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' \
                ]
                srclines[0] = '%s\n%s\n'%('\n'.join(filesummary), srclines[0])

                for lineid, linesummary in visitinfo.items():
                    srclines[linemap[fileid][lineid]-1] = '%s%s\n'%(srclines[linemap[fileid][lineid]-1], '\n'.join(linesummary))

                coveragefile = '%s/%s.coverage'%(os.path.abspath(Config.path['coverage']), os.path.basename(filemap[fileid]))
                with open(coveragefile, 'w') as fdst:
                    fdst.write(''.join(srclines))

            # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number> ...  <first timestamp>:<last timestamp>
            invokes = {}
            for fileid, lines in visits.items():
                for lineid, ranknums in lines.items():
                    linenum = linemap[fileid][lineid]
                    for ranknum, threadnums in ranknums.items():
                        if ranknum not in invokes:
                            invokes[ranknum] = {}
                        for threadnum, invokenums in threadnums.items():
                            if threadnum not in invokes[ranknum]:
                                invokes[ranknum][threadnum] = {}
                            for invokenum, timestamps in invokenums.items():
                                if invokenum not in invokes[ranknum][threadnum]:
                                    invokes[ranknum][threadnum][invokenum] = ( [ (fileid, linenum) ], [ min(timestamps), max(timestamps) ] )
                                else:
                                    pair = invokes[ranknum][threadnum][invokenum][0]
                                    pair.append( (fileid, linenum) )

                                    minmax = invokes[ranknum][threadnum][invokenum][1]
                                    minmax[0] = min( minmax[0], min(timestamps))
                                    minmax[1] = max( minmax[1], max(timestamps))

            with open(Config.coveragefile, 'a') as fd:

                # invoke section
                fd.write('[invoke]\n')
                fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number> ...  <first timestamp>:<last timestamp>\n')

                for ranknum, threadnums in invokes.items():
                    for threadnum, invokenums in threadnums.items():
                        for invokenum, (pair, minmax) in invokenums.items():
                            fd.write('%d %d %d = %s %f:%f\n'%(ranknum, threadnum, invokenum, \
                                ' '.join([ '%d:%d'%(fid, lnum) for fid, lnum in pair ]), minmax[0], minmax[1]))

        else:
            kgutils.logger.info('Reusing KGen coverage file: %s'%Config.coveragefile)

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
                filemap[int(opt)] = cfg.get('file', opt) 

            blockmap = {}
            for opt in cfg.options('block'):
                blockmap[int(opt)] =  tuple( int(linenum) for linenum in cfg.get('block', opt).split() )

            # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number> ...  <first timestamp>:<last timestamp>
            invokemap = {}
            idx = 0
            for opt in cfg.options('invoke'):
                idx += 1
                ranknum, threadnum, invokenum = tuple( int(num) for num in opt.split() )
                optval = cfg.get('invoke', opt).split()
                pairs = tuple( pair.split(':') for pair in optval[:-1])
                ts = cfg.get('invoke', opt).split()[-1].split(':')

                if invokenum not in invokemap:
                    invokemap[invokenum] = {}
                if ranknum not in invokemap[invokenum]:
                    invokemap[invokenum][ranknum] = {}
                if threadnum not in invokemap[invokenum][ranknum]:
                    threadnums = {}
                    invokemap[invokenum][ranknum][threadnum] = threadnums

                #threadnums = invokemap[invokenum][ranknum][threadnum]
                
                for fidstr, lnumstr in pairs:
                    fileid = int(fidstr)
                    linenum = int(lnumstr)
                    if fileid not in threadnums:
                        threadnums[fileid] = []
                    if linenum not in threadnums[fileid]:
                        threadnums[fileid].append(linenum)

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
            for ranknum in sorted(ranknums.keys()):
                if len(collected) > THREASHOLD_NUM: break
                threadnums = invokemap[invokenum][ranknum]
                for threadnum in sorted(threadnums.keys()):
                    if len(collected) > THREASHOLD_NUM: break
                    fileids = invokemap[invokenum][ranknum][threadnum]
                    for fileid in sorted(fileids.keys()):
                        if len(collected) > THREASHOLD_NUM: break
                        linenums = invokemap[invokenum][ranknum][threadnum][fileid]
                        for linenum in linenums:
                            if len(collected) > THREASHOLD_NUM: break
                            if (fileid, linenum) not in collected:
                                collected.append((fileid, linenum))
                                if (ranknum, threadnum, invokenum) not in triples:
                                    triples[(ranknum, threadnum, invokenum)] = None

        print 'At least, %s of conditional blocks will be excuted by using following (MPI ranks, OpenMP Threads, Invokes) triples:'%'{:.1%}'.format(THREASHOLD)
        print ','.join([ ':'.join([ str(n) for n in t ]) for t in triples.keys()])
        print ''
        print 'Following (File id, line number) pairs are covered by above triples:'
        print str(collected)

    def set_indent(self, indent):
        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}

    def remove_multiblanklines(self, text):
        MAXBLANKLINES = 3
        lines = text.split('\n')
        newlines = []
        count = 0
        for line in lines:
            if len(line)>0:
                newlines.append(line)
                count = 0
            else:
                count += 1
                if count < MAXBLANKLINES:
                    newlines.append(line)

        return '\n'.join(newlines)


    def write(self, f, line, n=True, t=False):
        nl = ''
        tab = ''
        if n: nl = '\n'
        if t: tab = '\t'
        f.write(tab + line + nl)

    def gen_makefile(self):

        org_files = [ filepath for filepath, (sfile, mods_used, units_used) in Config.used_srcfiles.iteritems() if sfile.used4coverage ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['path'])

        with open('%s/Makefile'%(Config.path['coverage']), 'wb') as f:

            self.write(f, '# Makefile for KGEN-generated instrumentation')
            self.write(f, '')

            cwd = os.path.abspath(Config.cwd)

            prerun_clean_str = ''
            if Config.prerun['clean']:
                self.write(f, 'PRERUN_CLEAN := %s'%Config.prerun['clean'])
                prerun_clean_str = '${PRERUN_CLEAN}; '

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
                if Config.state_switch['cmds']>0:
                    self.write(f, Config.state_switch['cmds'], t=True)
                else:
                    self.write(f, 'echo "No information is provided to copy files. Please specify switch commands using \'state-switch\' command line option"; exit -1', t=True)
            self.write(f, '')

            self.write(f, 'recover:')
            for org_file in org_files:
                self.write(f, 'cp -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
            self.write(f, '')

            self.write(f, 'recover_from_locals:')
            for org_file in org_files:
                self.write(f, 'cp -f %s.kgen_org %s'%(os.path.basename(org_file), org_file), t=True)
            self.write(f, '')


            self.write(f, 'save:')
            for org_file in org_files:
                self.write(f, 'if [ ! -f %(f)s.kgen_org ]; then cp -f %(f)s %(f)s.kgen_org; fi'%{'f':org_file}, t=True)
                self.write(f, 'if [ ! -f %(g)s.kgen_org ]; then cp -f %(f)s %(g)s.kgen_org; fi'%{'f':org_file, 'g':os.path.basename(org_file)}, t=True)
            self.write(f, '')

            if Config.cmd_clean['cmds']>0:
                self.write(f, 'clean:')
                self.write(f, '%s%s'%(prerun_clean_str, Config.cmd_clean['cmds']), t=True)
            self.write(f, '')
