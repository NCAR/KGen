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

def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def update(d, u):
    for k, v in u.items():
        if isinstance(v, collections.Mapping):
            r = update(d.get(k, {}), v)
            d[k] = r
        else:
            if k in d:
                if isinstance( u[k], int ):
                    d[k] += u[k]
                else:
                    d[k] = u[k]
            else:
                d[k] = u[k]
    return d

def visit(path, invokes, usedfiles, usedlines, mpivisits, ompvisits, rank):

#    # collect data
#    usedfiles = [] # fid=fpath
#    usedlines = {} # fid=[linenum, ...]
#    mpivisits = {} # fileid:linenum:mpirank=visits
#    ompvisits = {} # fileid:linenum:omptid=visits
#    linevisits = {} # fileid:linenum=visits
#    invokes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

    for tid in os.listdir(path):
        if tid.isdigit() and os.path.isdir(os.path.join(path,tid)):
            if tid not in invokes[rank]: invokes[rank][tid] = {}
            omppath = os.path.join(path,tid)
            for dfile in sorted(glob.glob('%s/*'%omppath)):
                dfilename = os.path.basename(dfile)
                match = re.match(r'^(\d+)\.(\d+)$', dfilename)
                if match:
                    fid = match.group(1)
                    lid = match.group(2)

                    if fid not in usedfiles: usedfiles.append(fid)

                    if fid not in usedlines: usedlines[fid] = []
                    if lid not in usedlines[fid]: usedlines[fid].append(lid)
 
                    if fid not in mpivisits: mpivisits[fid] = {}
                    if lid not in mpivisits[fid]: mpivisits[fid][lid] = {}
                    if rank not in mpivisits[fid][lid]: mpivisits[fid][lid][rank] = 0

                    if fid not in ompvisits: ompvisits[fid] = {}
                    if lid not in ompvisits[fid]: ompvisits[fid][lid] = {}
                    if tid not in ompvisits[fid][lid]: ompvisits[fid][lid][tid] = 0

                    with open(dfile, 'r') as f:
                        for line in f:
                            invoke = line[:16].strip()
                            visit = int(line[16:].strip())
                            
                            if invoke not in invokes[rank][tid]: invokes[rank][tid][invoke] = []
                            invokes[rank][tid][invoke].append( (fid, lid, visit) )

                            mpivisits[fid][lid][rank] += visit 
                            ompvisits[fid][lid][tid] += visit 

def readdatafiles(inq, outq):

    # collect data
    usedfiles = [] # fid=fpath
    usedlines = {} # fid=[linenum, ...]
    mpivisits = {} # fileid:linenum:mpirank=visits
    ompvisits = {} # fileid:linenum:omptid=visits
    invokes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

    mpipaths = inq.get()

    for path, mpirank in mpipaths:
        try:
            if mpirank not in invokes: invokes[mpirank] = {}
            visit(os.path.join(path,mpirank), invokes, usedfiles, usedlines, mpivisits, ompvisits, mpirank)
        except Exception as e:
            kgutils.logger.info('ERROR at %s: %s'%(multiprocessing.current_process().name, str(e)))
        finally:
            pass

    outq.put((invokes, usedfiles, usedlines, mpivisits, ompvisits))

class Coverage(KGTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting KCover')

        coverage_abspath = os.path.abspath('%s/%s'%(Config.path['outdir'], Config.path['coverage']))

        # create coverage directory
        if not os.path.exists(coverage_abspath):
            os.makedirs(coverage_abspath)
        if not os.path.exists('%s/__data__'%coverage_abspath):
            os.makedirs('%s/__data__'%coverage_abspath)
        if not os.path.exists('%s/__data__/__resource__'%coverage_abspath):
            os.makedirs('%s/__data__/__resource__'%coverage_abspath)

        # build app with instrumntation
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.coveragefile)) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            code_coverage_path = '%s/__data__/%s'%(coverage_abspath, Config.coverage['types']['code']['id'])
            if os.path.exists(code_coverage_path) and len(glob.glob( '%s/*'%code_coverage_path )) > 1 and Config.coverage['reuse_rawdata']:
                kgutils.logger.info('Reusing coverage raw data.')
            else:
                kgutils.logger.info('Generating coverage raw data.')

                if os.path.exists(code_coverage_path):
                    shutil.rmtree(code_coverage_path)
                os.makedirs(code_coverage_path)

                if os.path.exists('%s/__data__/__resource__/%s'%(coverage_abspath, Config.coverage['types']['code']['id'])):
                    shutil.rmtree('%s/__data__/__resource__/%s'%(coverage_abspath, Config.coverage['types']['code']['id']))
                os.makedirs('%s/__data__/__resource__/%s'%(coverage_abspath, Config.coverage['types']['code']['id']))

                # generate wrapper nodes
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

            if os.path.exists(code_coverage_path) and len(glob.glob( '%s/*'%code_coverage_path )) > 1 and Config.coverage['reuse_rawdata']:

                kgutils.logger.info('Generating coverage file: %s/%s'%(Config.path['outdir'], Config.coveragefile))

                files = None
                with open('%s/files'%code_coverage_path, 'r') as f:
                    files = json.load(f)

                lines = None
                with open('%s/lines'%code_coverage_path, 'r') as f:
                    lines = json.load(f)

                numranks = None
                with open('%s/mpi'%code_coverage_path, 'r') as f:
                    for idx, line in enumerate(f.read().split('\n')):
                        if idx == 0: numranks = int(line)

                numthreads = None
                with open('%s/openmp'%code_coverage_path, 'r') as f:
                    for idx, line in enumerate(f.read().split('\n')):
                        if idx == 0: numthreads = int(line)


                # collect data
                usedfiles = [] # fid
                usedlines = {} # fid=[linenum, ...]
                mpivisits = {} # fileid:linenum:mpirank=visits
                ompvisits = {} # fileid:linenum:omptid=visits
                invokes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

                mpipaths = []
                for item in os.listdir(code_coverage_path):
                    if item.isdigit() and os.path.isdir(os.path.join(code_coverage_path,item)):
                        mpipaths.append((code_coverage_path,item))

                nprocs = min( len(mpipaths), multiprocessing.cpu_count()*1)

                if nprocs == 0:
                    kgutils.logger.warn('No coverage data files are found.')
                else:
                    workload = [ chunk for chunk in chunks(mpipaths, int(math.ceil(len(mpipaths)/nprocs))) ]
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

                    for inq, chunk in zip(inqs,workload):
                        inq.put(chunk)

                    for outq in outqs:
                        invoke, usedfile, usedline, mpivisit, ompvisit = outq.get()
                        update(invokes, invoke)
                        for f in usedfile:
                            if f not in usedfiles:
                                usedfiles.append(f)
                        update(usedlines, usedline)
                        update(mpivisits, mpivisit)
                        update(ompvisits, ompvisit)

                    for idx in range(nprocs):
                        procs[idx].join()

                if len(invokes) == 0:
                    if not _DEBUG:
                        shutil.rmtree(code_coverage_path)
                    kgutils.logger.warn('Code coverage data is not collected.')
                else:
                    with open('%s/%s'%(Config.path['outdir'], Config.coveragefile), 'w') as fd:
                        # summary section
                        fd.write('[summary]\n')
                        fd.write('number_of_files_having_condblocks = %d\n'%len(files))
                        fd.write('number_of_files_invoked = %d\n'%len(usedfiles))
                        fd.write('number_of_condblocks_exist = %d\n'%sum( [ len(lmap) for fid, lmap in lines.items() ] ))
                        fd.write('number_of_condblocks_invoked = %d\n'%sum( [ len(lids) for fid, lids in usedlines.items() ] ))
                        fd.write('\n')

                        # file section
                        fd.write('[file]\n')
                        fd.write('; <file number> = <path to file>\n')
                        #for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                        for fileid, filepath in files.items():
                            fd.write('%s = %s/%s.kgen\n'%(fileid, coverage_abspath, os.path.basename(filepath)))
                        fd.write('used_files = %s\n'%', '.join([ fid for fid in usedfiles ]))
                        fd.write('\n')

                        # block section
                        fd.write('[block]\n')
                        fd.write('; <file number> =  <line number> ...\n')
                        #for path, (pathnum, lines) in Config.plugindb['coverage_paths'].items():
                        for fileid, lmap in lines.items():
                            fd.write('%s = %s\n'%(fileid, ', '.join([ lnum for lid,lnum in lmap.items() ])))

                        used_line_pairs = []
                        for fid, lids in usedlines.items():
                            for lid in lids:
                                used_line_pairs.append( (f,lid) )
                        fd.write('used_lines = %s\n'%', '.join([ '%s:%s'%(fid,lines[fid][lid]) for fid,lid in used_line_pairs ]))
                        fd.write('\n')


                        # invoke section
                        fd.write('[invoke]\n')
                        fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number><num of invocations> ...\n')

                        for ranknum, threadnums in invokes.items():
                            for threadnum, invokenums in threadnums.items():
                                for invokenum, triples in invokenums.items():
                                    fd.write('%s %s %s = %s\n'%(ranknum, threadnum, invokenum, \
                                        ', '.join( [ '%s:%s:%d'%(fid, lines[fid][lid], nivks) for fid, lid, nivks in triples ] )))

                    kgutils.logger.info('    ***** Within "%s" kernel *****:'%Config.kernel['name'])
                    kgutils.logger.info('    * %d original source files have conditional blocks.'%len(files))
                    kgutils.logger.info('    * %d original source files are invoked at least once.'%len(usedfiles))
                    kgutils.logger.info('    * %d conditional blocks exist in the original source files.'%\
                        sum( [ len(lmap) for fid, lmap in lines.items() ] ))
                    kgutils.logger.info('    * %d conditional blocks are executed at least once among all the conditional blocks.'%\
                        sum( [ len(lids) for fid, lids in usedlines.items() ] ))

                    for fid in usedfiles:
                        basefile = '%s/%s.kgen'%(coverage_abspath, os.path.basename(files[fid]))
                        with open(basefile, 'r') as fsrc:
                            srclines = fsrc.readlines()

                        filesummary = [ \
                            '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', \
                            '!! %d conditional blocks exist in this file'%len(lines[fid]), \
                            '!! %d conditional blokcs are executed at least once among all the conditional blocks.'%len(usedlines[fid]), \
                            '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' \
                        ]
                        srclines[0] = '%s\n%s\n'%('\n'.join(filesummary), srclines[0])

                        for lid in usedlines[fid]:
                            linevisit = [ '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' ]
                            linevisit.append('!! Total number of visits: %d'% sum([visits for rank, visits in mpivisits[fid][lid].items() ]))
                            if Config.mpi['enabled']:
                                linevisit.append('!! MPI rank(visits)      : %s' % ' '.join( \
                                    ['%s(%d)'%(r,i) for r,i in mpivisits[fid][lid].items()]))
                            if Config.openmp['enabled']:
                                linevisit.append('!! OpenMP thread(visits) : %s' % ' '.join( \
                                    ['%s(%d)'%(t,i) for t,i in ompvisits[fid][lid].items()]))
                            linevisit.append( '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )

                            srclines[int(lines[fid][lid])-1] = '%s%s\n'%(srclines[int(lines[fid][lid])-1], '\n'.join(linevisit))

                        coveragefile = '%s/%s.coverage'%(coverage_abspath, os.path.basename(filepath))
                        with open(coveragefile, 'w') as fdst:
                            fdst.write(''.join(srclines))

            else:
                if not _DEBUG:
                    if os.path.exists('%s/%s'%(Config.path['outdir'], Config.coveragefile)):
                        os.remove('%s/%s'%(Config.path['outdir'], Config.coveragefile))
                    shutil.rmtree(code_coverage_path)
                kgutils.logger.info('failed to generate coverage information: %s'%err)

            out, err, retcode = kgutils.run_shcmd('make recover', cwd=coverage_abspath)

            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])
        else:
            kgutils.logger.info('Reusing KGen coverage file: %s/%s'%(Config.path['outdir'], Config.coveragefile))

        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.coveragefile)):
            kgutils.logger.warn('No coverage file is found.')
        else:
            # read ini file
            kgutils.logger.info('Reading %s/%s'%(Config.path['outdir'], Config.coveragefile))

            cfg = configparser.ConfigParser()
            cfg.optionxform = str
            cfg.read('%s/%s'%(Config.path['outdir'], Config.coveragefile))

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

