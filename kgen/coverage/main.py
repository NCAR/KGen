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

from kgtool import KGModelingTool
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

class Coverage(KGModelingTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting KCover')

        model_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['model']))
        coverage_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['coverage']))

        if not os.path.exists(coverage_realpath):
            os.makedirs(coverage_realpath)

        # clear shared resources
        Config.used_srcfiles.clear()

        if not self.hasmodel('coverage') or 'all' in Config.rebuild or 'coverage' in Config.rebuild:
        #if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            data_coverage_path = '%s/__data__/%s'%(model_realpath, Config.model['types']['code']['id'])
            if os.path.exists(data_coverage_path) and len(glob.glob( '%s/*'%data_coverage_path )) > 1 and Config.model['reuse_rawdata']:
                kgutils.logger.info('Reusing coverage raw data.')
            else:
                kgutils.logger.info('Generating coverage raw data.')

                if os.path.exists(data_coverage_path):
                    shutil.rmtree(data_coverage_path)
                os.makedirs(data_coverage_path)

                if os.path.exists('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['code']['id'])):
                    shutil.rmtree('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['code']['id']))
                os.makedirs('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['code']['id']))

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
                            with open('%s/%s'%(coverage_realpath, filename), 'wb') as fd:
                                fd.write(slines)
                            with open('%s/%s.kgen'%(coverage_realpath, filename), 'wb') as ft:
                                ft.write('\n'.join(sfile.kgen_stmt.prep))

                self.gen_makefile()

                kgutils.logger.info('Instrumentation for coverage is generated at %s.'%coverage_realpath)

                # TODO: wait until coverage data generation is completed
                # use -K option fir bsub to wait for job completion

                # clean app
                if Config.cmd_clean['cmds']:
                    kgutils.run_shcmd(Config.cmd_clean['cmds'])
                if Config.state_switch['clean']:
                    kgutils.run_shcmd(Config.state_switch['clean'])

                # TEMP
                out, err, retcode = kgutils.run_shcmd('make', cwd=coverage_realpath)
                if retcode != 0:
                    kgutils.logger.warn('Coverage raw data is not correctly generated.: %s'%err)

            if os.path.exists(data_coverage_path) and len(glob.glob( '%s/*'%data_coverage_path )) > 1 and Config.model['reuse_rawdata']:

                kgutils.logger.info('Generating model file: %s/%s'%(Config.path['outdir'], Config.modelfile))

                files = None
                with open('%s/files'%data_coverage_path, 'r') as f:
                    files = json.load(f)

                lines = None
                with open('%s/lines'%data_coverage_path, 'r') as f:
                    lines = json.load(f)

                if not os.path.exists('%s/mpi'%data_coverage_path) or not os.path.exists('%s/openmp'%data_coverage_path):
                    kgutils.logger.error('Coverage raw data is not correct. Please rerun KGen after generating coverage raw data correctly.')
                else:
                    numranks = None
                    with open('%s/mpi'%data_coverage_path, 'r') as f:
                        for idx, line in enumerate(f.read().split('\n')):
                            if idx == 0: numranks = int(line)

                    numthreads = None
                    # NOTE: numthreads could be smaller than actual number of omp threads as it depends on code regions.
                    with open('%s/openmp'%data_coverage_path, 'r') as f:
                        for idx, line in enumerate(f.read().split('\n')):
                            if idx == 0: numthreads = int(line)


                    # collect data
                    usedfiles = [] # fid
                    usedlines = {} # fid=[linenum, ...]
                    mpivisits = {} # fileid:linenum:mpirank=visits
                    ompvisits = {} # fileid:linenum:omptid=visits
                    invokes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

                    mpipaths = []
                    for item in os.listdir(data_coverage_path):
                        if item.isdigit() and os.path.isdir(os.path.join(data_coverage_path,item)):
                            mpipaths.append((data_coverage_path,item))

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
                            shutil.rmtree(data_coverage_path)
                        kgutils.logger.warn('Code coverage data is not collected.')
                    else:
                        try:
                            coverage_sections = [ 'summary', 'file', 'block', 'invoke']

                            self.addmodel('coverage', coverage_sections)

                            summary = []
                            summary.append( ( 'number_of_files_having_condblocks', str(len(files))) )
                            summary.append( ( 'number_of_files_invoked', str(len(usedfiles)) ) )
                            summary.append( ( 'number_of_condblocks_exist', str(sum( [ len(lmap) for fid, lmap in lines.items() ] )) ) )
                            summary.append( ( 'number_of_condblocks_invoked', str(sum( [ len(lids) for fid, lids in usedlines.items() ] )) ) )
                            self.addsection('coverage', 'summary', summary)

                            # file section
                            file = []
                            #    fd.write('; <file number> = <path to file>\n')
                            for fileid, filepath in files.items():
                                file.append( ( str(fileid), '%s/%s.kgen\n'%(coverage_realpath, os.path.basename(filepath)) ) )
                            file.append( ( 'used_files', ', '.join([ fid for fid in usedfiles ]) ) )
                            self.addsection('coverage', 'file', file)

                            # block section
                            block = []
                            #fd.write('; <file number> =  <line number> ...\n')
                            for fileid, lmap in lines.items():
                                block.append( ( str(fileid), ', '.join([ lnum for lid,lnum in lmap.items() ]) ) )

                            used_line_pairs = []
                            for fid, lids in usedlines.items():
                                for lid in lids:
                                    used_line_pairs.append( (fid,lid) )
                            block.append( ( 'used_lines', ', '.join([ '%s:%s'%(fid,lines[fid][lid]) for fid,lid in used_line_pairs ]) ) )
                            self.addsection('coverage', 'block', block)

                            # invoke section
                            invoke = []
                            #fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number><num of invocations> ...\n')

                            for ranknum, threadnums in invokes.items():
                                for threadnum, invokenums in threadnums.items():
                                    for invokenum, triples in invokenums.items():
                                        invoke.append( ( '%s %s %s'%(ranknum, threadnum, invokenum), \
                                            ', '.join( [ '%s:%s:%d'%(fid, lines[fid][lid], nivks) for fid, lid, nivks in triples ] ) ) )
                            self.addsection('coverage', 'invoke', invoke)

                            kgutils.logger.info('    ***** Within "%s" kernel *****:'%Config.kernel['name'])
                            kgutils.logger.info('    * %d original source files have conditional blocks.'%len(files))
                            kgutils.logger.info('    * %d original source files are invoked at least once.'%len(usedfiles))
                            kgutils.logger.info('    * %d conditional blocks exist in the original source files.'%\
                                sum( [ len(lmap) for fid, lmap in lines.items() ] ))
                            kgutils.logger.info('    * %d conditional blocks are executed at least once among all the conditional blocks.'%\
                                sum( [ len(lids) for fid, lids in usedlines.items() ] ))

                            for fid in usedfiles:
                                basefile = '%s/%s.kgen'%(coverage_realpath, os.path.basename(files[fid]))
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
                                            ['%s(%d)'%(r,mpivisits[fid][lid][r]) for r in sorted(mpivisits[fid][lid])]))
                                            #['%s(%d)'%(r,i) for r,i in mpivisits[fid][lid].items()]))
                                    if Config.openmp['enabled']:
                                        linevisit.append('!! OpenMP thread(visits) : %s' % ' '.join( \
                                            ['%s(%d)'%(t,ompvisits[fid][lid][t]) for t in sorted(ompvisits[fid][lid])]))
                                            #['%s(%d)'%(t,i) for t,i in ompvisits[fid][lid].items()]))
                                    linevisit.append( '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )

                                    srclines[int(lines[fid][lid])-1] = '%s%s\n'%(srclines[int(lines[fid][lid])-1], '\n'.join(linevisit))

                                coveragefile = '%s/%s.coverage'%(coverage_realpath, os.path.basename(files[fid]))
                                with open(coveragefile, 'w') as fdst:
                                    fdst.write(''.join(srclines))
                        except Exception as e:
                            kgutils.logger.error(str(e))
            else:
                if not _DEBUG:
                    shutil.rmtree(data_coverage_path)
                kgutils.logger.info('failed to generate coverage information')

            out, err, retcode = kgutils.run_shcmd('make recover', cwd=coverage_realpath)

            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])
        else: # check if coverage should be invoked
            kgutils.logger.info('Reusing KGen coverage file: %s/%s'%(Config.path['outdir'], Config.modelfile))

        # check if coverage data exists in model file
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)):
            kgutils.logger.warn('No coverage file is found.')
        else:
            # read ini file
            kgutils.logger.info('Reading %s/%s'%(Config.path['outdir'], Config.modelfile))

            cfg = configparser.ConfigParser()
            cfg.optionxform = str
            cfg.read('%s/%s'%(Config.path['outdir'], Config.modelfile))

            number_of_files_having_condblocks = int(cfg.get('coverage.summary', 'number_of_files_having_condblocks'))
            number_of_files_invoked = int(cfg.get('coverage.summary', 'number_of_files_invoked'))
            number_of_condblocks_exist = int(cfg.get('coverage.summary', 'number_of_condblocks_exist'))
            number_of_condblocks_invoked = int(cfg.get('coverage.summary', 'number_of_condblocks_invoked'))

            try:
                filemap = {}
                for opt in cfg.options('coverage.file'):
                    if opt.isdigit():
                        filemap[opt] = cfg.get('coverage.file', opt) 

                blockmap = {}
                for opt in cfg.options('coverage.block'):
                    if opt.isdigit():
                        blockmap[opt] =  tuple( linenum for linenum in cfg.get('coverage.block', opt).split() )

                # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number>:<num invokes> ... 
                invokemap = {}
                idx = 0
                for opt in cfg.options('coverage.invoke'):
                    idx += 1
                    ranknum, threadnum, invokenum = tuple( num for num in opt.split() )
                    optval = cfg.get('coverage.invoke', opt).split(',')
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

            THREASHOLD = Config.model['types']['code']['percentage'] / 100.0
            THREASHOLD_NUM = int(math.ceil(number_of_condblocks_invoked*THREASHOLD))
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
            #print ''
            #print 'Following (File id, line number) pairs are covered by above triples:'
            #print str(collected)

            for ranknum, threadnum, invokenum in triples.keys():
                Config.invocation['triples'].append( ( (str(ranknum), str(ranknum)), (str(threadnum), str(threadnum)), \
                    (str(invokenum), str(invokenum)) ) )

    def write(self, f, line, n=True, t=False):
        nl = ''
        tab = ''
        if n: nl = '\n'
        if t: tab = '\t'
        f.write(tab + line + nl)

    def gen_makefile(self):

        coverage_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['coverage']))

        org_files = [ filepath for filepath, (sfile, mods_used, units_used) in Config.used_srcfiles.iteritems() if sfile.used4coverage ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['filepath'])

        with open('%s/Makefile'%coverage_realpath, 'wb') as f:

            self.write(f, '# Makefile for KGEN-generated instrumentation')
            self.write(f, '')

            cwd = os.path.realpath(Config.cwd)

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
                    self.write(f, 'rm -f %(f2)s'%{'f2':org_file}, t=True)
                    self.write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
            elif Config.state_switch['type']=='copy':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'rm -f %s/%s'%(Config.state_switch['directory'], basename), t=True)
                    self.write(f, 'cp -f %s/%s %s'%(coverage_realpath, basename, Config.state_switch['directory']), t=True)
            self.write(f, '')

            self.write(f, 'recover:')
            for org_file in org_files:
                self.write(f, 'rm -f %s'%org_file, t=True)
                self.write(f, 'cp -f %s.kgen_org %s'%(os.path.basename(org_file), org_file), t=True)
            self.write(f, '')

            self.write(f, 'recover_from_srcdir:')
            for org_file in org_files:
                self.write(f, 'rm -f %s'%org_file, t=True)
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

