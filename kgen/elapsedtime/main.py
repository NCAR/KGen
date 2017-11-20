'''KGen elapsedtime detector
'''

import os
import re
import glob
import json
import math
import shutil
import datetime
import time
import random
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

def visit(path, etimes, eminmax):

#    # collect data
#    etimes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

    for tid in os.listdir(path):
        if tid.isdigit() and os.path.isdir(os.path.join(path,tid)):
            if tid not in etimes[rank]: etimes[rank][tid] = {}
            omppath = os.path.join(path,tid)
            for dfile in sorted(glob.glob('%s/*'%omppath)):
                dfilename = os.path.basename(dfile)
                match = re.match(r'^(\d+)\.(\d+)$', dfilename)
                if match:
                    fid = match.group(1)
                    lid = match.group(2)

                    with open(dfile, 'r') as f:
                        for line in f:
                            invoke = line[:16].strip()
                            etime = float(line[16:].strip())
                            etimes[rank][tid][invoke] = etime
                            if etime < eminmax[0]:
                                eminmax[0] = etime
                            if etime > eminmax[1]:
                                eminmax[1] = etime

def readdatafiles(inq, outq):

    # collect data
    etimes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]
    emeta = [ 1.0E100, 0.0, 0, 0.0, 0, 0 ] #  min, max, number, resolution, under limit, over limit

    etimepaths = inq.get()

    etimemin_limit = Config.model['types']['etime']['minval']
    etimemax_limit = Config.model['types']['etime']['maxval']

    for etimeroot, mpirank, ompthread in etimepaths:
        try:
            if mpirank not in etimes: etimes[mpirank] = {}
            if ompthread not in etimes[mpirank]: etimes[mpirank][ompthread] = {}

            with open(os.path.join(etimeroot, '%s.%s'%(mpirank, ompthread)), 'r') as f:
                for line in f:
                    invoke, start, stop, resolution = line.split()
                    estart = float(start)
                    estop = float(stop)
                    ediff = estop - estart
                    if etimemin_limit is not None and ediff < etimemin_limit:
                        emeta[4] += 1
                    elif etimemax_limit is not None and ediff > etimemax_limit:
                        emeta[5] += 1
                    else:
                        etimes[mpirank][ompthread][invoke] = ( start, stop )
                        if ediff < emeta[0]:
                            emeta[0] = ediff
                        if ediff > emeta[1]:
                            emeta[1] = ediff
                        emeta[2] += 1
                        eresol = float(resolution)
                        if eresol > emeta[3]:
                            emeta[3] = eresol

        except Exception as e:
            kgutils.logger.info('ERROR at %s: %s'%(multiprocessing.current_process().name, str(e)))
        finally:
            pass

    outq.put((etimes, emeta))

class ElapsedTime(KGModelingTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting ETime')

        model_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['model']))
        etime_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['etime']))

        if not os.path.exists(etime_realpath):
            os.makedirs(etime_realpath)

        # clear shared resources
        Config.used_srcfiles.clear()

        if not self.hasmodel(Config.path['etime']) or 'all' in Config.rebuild or 'etime' in Config.rebuild:
        #if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            data_etime_path = '%s/__data__/%s'%(model_realpath, Config.model['types']['etime']['id'])
            if os.path.exists(data_etime_path) and len(glob.glob( '%s/*'%data_etime_path )) > 0 and Config.model['reuse_rawdata']:
                kgutils.logger.info('Reusing elapsedtime raw data.')
            else:
                kgutils.logger.info('Generating elapsedtime raw data.')

                if os.path.exists(data_etime_path):
                    shutil.rmtree(data_etime_path)
                if os.path.exists('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['etime']['id'])):
                    shutil.rmtree('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['etime']['id']))

                time.sleep(1)

                os.makedirs(data_etime_path)
                os.makedirs('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['etime']['id']))

                # generate wrapper nodes
                for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():

                    if os.path.realpath(filepath) == os.path.realpath(Config.callsite['filepath']):
                        sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                        sfile.used4etime = True
                        self.genfiles.append((sfile, filepath))
                        Config.used_srcfiles[filepath] = (sfile, mods_used, units_used)

#                    sfile = None
#                    if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
#                        sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
#                    elif os.path.realpath(filepath) == os.path.realpath(Config.callsite['filepath']):
#                        sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
#
#                    
#                    if sfile is not None:
#                        if os.path.realpath(filepath) == os.path.realpath(Config.callsite['filepath']):
#                            sfile.used4etime = True
#                        else:
#                            sfile.used4etime = False
#
#                        self.genfiles.append((sfile, filepath))
#                        Config.used_srcfiles[filepath] = (sfile, mods_used, units_used)
#
#                    if sfile is None:
#                        import pdb; pdb.set_trace()
#                        raise kgutils.ProgramException('Kernel source file is not generated for %s.'%filepath)

                # process each nodes in the tree
                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('etime'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.created([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('etime'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.process([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('etime'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.finalize([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('etime'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.flatten(KERNEL_ID_0, [plugin_name])

                # generate source files from each node of the tree
                etime_files = []
                #import pdb; pdb.set_trace()
                for sfile, filepath in self.genfiles:
                    #import pdb; pdb.set_trace()
                    filename = os.path.basename(filepath)
                    if sfile.used4etime:
                        set_indent('')
                        slines = sfile.tostring()
                        if slines is not None:
                            slines = kgutils.remove_multiblanklines(slines)
                            etime_files.append(filename)
                            with open('%s/%s'%(etime_realpath, filename), 'wb') as fd:
                                fd.write(slines)
                            with open('%s/%s.kgen'%(etime_realpath, filename), 'wb') as ft:
                                ft.write('\n'.join(sfile.kgen_stmt.prep))

                self.gen_makefile()

                kgutils.logger.info('Instrumentation for elapsedtime is generated at %s.'%etime_realpath)

                # TODO: wait until coverage data generation is completed
                # use -K option fir bsub to wait for job completion

                # clean app
                if Config.cmd_clean['cmds']:
                    kgutils.run_shcmd(Config.cmd_clean['cmds'])
                if Config.state_switch['clean']:
                    kgutils.run_shcmd(Config.state_switch['clean'])

                # TEMP
                out, err, retcode = kgutils.run_shcmd('make', cwd=etime_realpath)
                if retcode != 0:
                    kgutils.logger.warn('Elapsedtime raw data is not correctly generated.: %s'%err)

            if os.path.exists(data_etime_path) and len(glob.glob( '%s/*'%data_etime_path )) > 0 and Config.model['reuse_rawdata']:

                kgutils.logger.info('Generating model file: %s/%s'%(Config.path['outdir'], Config.modelfile))

                # collect data
                etimes = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]
                etimemin = 1.0E100
                etimemax = 0.0
                netimes = 0
                etimeresol = 0.0
                nexcluded_under = 0
                nexcluded_over = 0
 
                mpipaths = []
                for item in os.listdir(data_etime_path):
                    try:
                        mpirank, ompthread = item.split('.')
                        if mpirank.isdigit() and ompthread.isdigit():
                            mpipaths.append((data_etime_path, mpirank, ompthread))
                    except:
                        pass

                #import pdb; pdb.set_trace()

                nprocs = min( len(mpipaths), multiprocessing.cpu_count()*1)

                if nprocs == 0:
                    kgutils.logger.warn('No elapsedtime data files are found.')
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
                        etime, emeta = outq.get()
                        update(etimes, etime)
                        etimemin = min(etimemin, emeta[0])
                        etimemax = max(etimemax, emeta[1])
                        netimes += emeta[2]
                        etimeresol = max(etimeresol, emeta[3])
                        nexcluded_under += emeta[4]
                        nexcluded_over += emeta[5]

                    for idx in range(nprocs):
                        procs[idx].join()

                    kgutils.logger.info('# of excluded samples: under limit = %d, over limit = %d'%(nexcluded_under, nexcluded_over))

                #import pdb; pdb.set_trace()

                if len(etimes) == 0:
                    if not _DEBUG:
                        shutil.rmtree(data_etime_path)
                    kgutils.logger.warn('Elapsedtime data is not collected.')
                else:
                    try:
                        etime_sections = [ Config.path['etime'], 'summary']

                        self.addmodel(Config.path['etime'], etime_sections)

                        # elapsedtime section
                        etime = []
                        #fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number><num of invocations> ...\n')

                        for ranknum, threadnums in etimes.items():
                            for threadnum, invokenums in threadnums.items():
                                for invokenum, evalues  in invokenums.items():
                                    etime.append( ( '%s %s %s'%(ranknum, threadnum, invokenum), ', '.join(evalues) ) )
                        self.addsection(Config.path['etime'], Config.path['etime'], etime)

                        summary = []
                        summary.append( ('minimum_elapsedtime', str(etimemin)) )
                        summary.append( ('maximum_elapsedtime', str(etimemax)) )
                        summary.append( ('number_elapsedtimes', str(netimes)) )
                        summary.append( ('resolution_elapsedtime', str(etimeresol)) )
                        self.addsection(Config.path['etime'], 'summary', summary )

                    except Exception as e:
                        kgutils.logger.error(str(e))
            else:
                if not _DEBUG:
                    shutil.rmtree(data_etime_path)
                kgutils.logger.info('failed to generate elapsedtime information')

            out, err, retcode = kgutils.run_shcmd('make recover', cwd=etime_realpath)

            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])
        else: # check if coverage should be invoked
            kgutils.logger.info('Reusing Elapsedtime file: %s/%s'%(Config.path['outdir'], Config.modelfile))

        # check if elapsedtime data exists in model file
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)):
            kgutils.logger.warn('No elapsedtime file is found.')
        else:
            # read ini file
            kgutils.logger.info('Reading %s/%s'%(Config.path['outdir'], Config.modelfile))

            cfg = configparser.ConfigParser()
            cfg.optionxform = str
            cfg.read('%s/%s'%(Config.path['outdir'], Config.modelfile))

            try:

                etimemin = float(cfg.get('elapsedtime.summary', 'minimum_elapsedtime').strip())
                etimemax = float(cfg.get('elapsedtime.summary', 'maximum_elapsedtime').strip())
                netimes = int(cfg.get('elapsedtime.summary', 'number_elapsedtimes').strip())
                etimediff = etimemax - etimemin
                etimeres = float(cfg.get('elapsedtime.summary', 'resolution_elapsedtime').strip())

                # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number>:<num etimes> ... 
                if etimediff == 0:
                    nbins = 1
                else:
                    nbins = max(min(Config.model['types']['etime']['nbins'], netimes), 2)

                kgutils.logger.info('nbins = %d'%nbins)
                kgutils.logger.info('etimemin = %f'%etimemin)
                kgutils.logger.info('etimemax = %f'%etimemax)
                kgutils.logger.info('etimediff = %f'%etimediff)
                kgutils.logger.info('netimes = %d'%netimes)
                
                if nbins > 1:
                    etimebins = [ {} for _ in range(nbins) ]
                    etimecounts = [ 0 for _ in range(nbins) ]
                else:
                    etimebins = [ {} ]
                    etimecounts = [ 0 ]

                idx = 0
                for opt in cfg.options('elapsedtime.elapsedtime'):
                    ranknum, threadnum, invokenum = tuple( num for num in opt.split() )
                    start, stop = cfg.get('elapsedtime.elapsedtime', opt).split(',')
                    estart = float(start)
                    eend = float(stop)
                    etimeval = eend - estart
                    if nbins > 1:
                        binnum = int(math.floor((etimeval - etimemin) / etimediff * (nbins - 1)))
                    else:
                        binnum = 0
                    etimecounts[binnum] += 1

                    invokenum = int(invokenum)
                    if invokenum not in etimebins[binnum]:
                        etimebins[binnum][invokenum] = {}
                    if ranknum not in etimebins[binnum][invokenum]:
                        etimebins[binnum][invokenum][ranknum] = {}
                    if threadnum not in etimebins[binnum][invokenum][ranknum]:
                        etimebins[binnum][invokenum][ranknum][threadnum] = float(etimeval)
                    else:
                        raise Exception('Dupulicated data: (%s, %s, %s, %s)'%(invokenum, ranknum, threadnum, etimeval))

                    idx += 1

                    if idx % 100000 == 0:
                        print 'Processed %d items: %s'%(idx, datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y"))
            except Exception as e:
                raise Exception('Please check the format of elapsedtime file: %s'%str(e))

            # types of representation
            # average, median, min/max, n-stratified, distribution
            # bins with histogram

            totalcount = sum(etimecounts)
            countdist = [ float(count) / float(totalcount) for count in etimecounts ]
            ndata = Config.model['types']['etime']['ndata']
            datacollect = [ int(round(dist * ndata)) for dist in countdist ]

            triples = []
            for binnum, etimebin in enumerate(etimebins):
                bin_triples = []
                range_begin = binnum*(etimemax-etimemin)/nbins + etimemin if binnum > 0  else etimemin
                range_end = (binnum+1)*(etimemax-etimemin)/nbins + etimemin if binnum < (nbins-1)  else None

                bunit = 'sec'
                if range_begin < 1.E-6:
                    bunit = 'usec'
                    range_begin *= 1.E6

                if range_end is None:
                    print 'From bin # %d [ %f (%s) ~ ] %f %% of %d'%(binnum, \
                        range_begin, bunit, countdist[binnum] * 100, totalcount)
                else:
                    eunit = 'sec'
                    if range_end < 1.E-6:
                        eunit = 'usec'
                        range_end *= 1.E6

                    print 'From bin # %d [ %f (%s) ~ %f (%s) ] %f %% of %d'%(binnum, \
                        range_begin, bunit, range_end, eunit, countdist[binnum] * 100, totalcount)

                for invokenum in sorted(etimebin.keys()):
                    if len(bin_triples) >= datacollect[binnum]: break
                    # select datacollect[binum] under this data tree, rank/thread/invoke
                    bininvokes = etimebin[invokenum].keys()
                    random.shuffle(bininvokes)
                    for ranknum in bininvokes:
                        if len(bin_triples) >= datacollect[binnum]: break
                        binranks = etimebin[invokenum][ranknum].keys()
                        random.shuffle(binranks)
                        for threadnum in binranks:
                            bin_triples.append( (ranknum, threadnum, invokenum) )
                            print '        invocation triple: %s:%s:%s'%(ranknum, threadnum, invokenum)
                triples.extend(bin_triples)

            print 'Number of bins: %d'%nbins
            print 'Minimun elapsed time: %f'%etimemin
            print 'Maximum elapsed time: %f'%etimemax
            #print 'Selected invocation triples:'
            #print ','.join([ ':'.join([ str(n) for n in t ]) for t in triples])

            for ranknum, threadnum, invokenum in triples:
                Config.invocation['triples'].append( ( (str(ranknum), str(ranknum)), (str(threadnum), str(threadnum)), \
                    (str(invokenum), str(invokenum)) ) )

    def write(self, f, line, n=True, t=False):
        nl = ''
        tab = ''
        if n: nl = '\n'
        if t: tab = '\t'
        f.write(tab + line + nl)

    def gen_makefile(self):

        etime_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['etime']))

        org_files = [ filepath for filepath, (sfile, mods_used, units_used) in Config.used_srcfiles.iteritems() if sfile.used4etime ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['filepath'])

        with open('%s/Makefile'%etime_realpath, 'wb') as f:

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
                    self.write(f, 'rm -f %s'%org_file, t=True)
                    self.write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
            elif Config.state_switch['type']=='copy':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'rm -f %s/%s'%(Config.state_switch['directory'], basename), t=True)
                    self.write(f, 'cp -f %s/%s %s'%(etime_realpath, basename, Config.state_switch['directory']), t=True)
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

