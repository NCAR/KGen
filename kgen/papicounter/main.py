'''KGen papi detector
'''

import os
import re
import glob
import json
import math
import shutil
import datetime
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

def visit(path, papis, eminmax):

#    # collect data
#    papis = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]

    for tid in os.listdir(path):
        if tid.isdigit() and os.path.isdir(os.path.join(path,tid)):
            if tid not in papis[rank]: papis[rank][tid] = {}
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
                            papi = float(line[16:].strip())
                            papis[rank][tid][invoke] = papi
                            if papi < eminmax[0]:
                                eminmax[0] = papi
                            if papi > eminmax[1]:
                                eminmax[1] = papi

def readdatafiles(inq, outq):

    # collect data
    papis = {} 
    pmeta = [ 1E100, 0, 0, 0, 0 ] #  min, max, number, resolution, under limit, over limit

    papipaths = inq.get()

    papimin_limit = Config.model['types']['papi']['minval']
    papimax_limit = Config.model['types']['papi']['maxval']

    for papiroot, mpirank, ompthread in papipaths:
        try:
            if mpirank not in papis: papis[mpirank] = {}
            if ompthread not in papis[mpirank]: papis[mpirank][ompthread] = {}

            with open(os.path.join(papiroot, '%s.%s'%(mpirank, ompthread)), 'r') as f:
                for line in f:
                    invoke, count = line.split()
                    count = int(count)
                    if papimin_limit is not None and count < papimin_limit:
                        pmeta[3] += 1
                    elif papimax_limit is not None and count > papimax_limit:
                        pmeta[4] += 1
                    else:
                        papis[mpirank][ompthread][invoke] = count
                        if count < pmeta[0]:
                            pmeta[0] = count
                        if count > pmeta[1]:
                            pmeta[1] = count
                        pmeta[2] += 1

        except Exception as e:
            kgutils.logger.info('ERROR at %s: %s'%(multiprocessing.current_process().name, str(e)))
        finally:
            pass

    outq.put((papis, pmeta))

class PapiCounter(KGModelingTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting PAPI')

        model_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['model']))
        papi_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['papi']))

        if not os.path.exists(papi_realpath):
            os.makedirs(papi_realpath)

        # clear shared resources
        Config.used_srcfiles.clear()

        if not self.hasmodel('papi') or 'all' in Config.rebuild or 'papi' in Config.rebuild:
        #if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            data_papi_path = '%s/__data__/%s'%(model_realpath, Config.model['types']['papi']['id'])
            if os.path.exists(data_papi_path) and len(glob.glob( '%s/*'%data_papi_path )) > 0 and Config.model['reuse_rawdata']:
                kgutils.logger.info('Reusing papi raw data.')
            else:
                kgutils.logger.info('Generating papi counter raw data.')

                if os.path.exists(data_papi_path):
                    shutil.rmtree(data_papi_path)
                os.makedirs(data_papi_path)

                if os.path.exists('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['papi']['id'])):
                    shutil.rmtree('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['papi']['id']))
                os.makedirs('%s/__data__/__resource__/%s'%(model_realpath, Config.model['types']['papi']['id']))

                # generate wrapper nodes
                for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():
                    if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                        sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                        if filepath == Config.callsite['filepath']:
                            sfile.used4papi = True
                        else:
                            sfile.used4papi = False
                        if sfile is None:
                            raise kgutils.ProgramException('Kernel source file is not generated for %s.'%filepath)
                        self.genfiles.append((sfile, filepath))
                        Config.used_srcfiles[filepath] = (sfile, mods_used, units_used)

                # process each nodes in the tree
                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('papi'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.created([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('papi'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.process([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('papi'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.finalize([plugin_name])

                for plugin_name in event_register.keys():
                    if not plugin_name.startswith('papi'): continue

                    for sfile, filepath in self.genfiles:
                        sfile.flatten(KERNEL_ID_0, [plugin_name])

                # generate source files from each node of the tree
                papi_files = []
                for sfile, filepath in self.genfiles:
                    filename = os.path.basename(filepath)
                    if sfile.used4papi:
                        set_indent('')
                        slines = sfile.tostring()
                        if slines is not None:
                            slines = kgutils.remove_multiblanklines(slines)
                            papi_files.append(filename)
                            with open('%s/%s'%(papi_realpath, filename), 'wb') as fd:
                                fd.write(slines)
                            with open('%s/%s.kgen'%(papi_realpath, filename), 'wb') as ft:
                                ft.write('\n'.join(sfile.kgen_stmt.prep))

                self.gen_makefile()

                kgutils.logger.info('Instrumentation for papi is generated at %s.'%papi_realpath)

                # TODO: wait until coverage data generation is completed
                # use -K option fir bsub to wait for job completion

                # clean app
                if Config.cmd_clean['cmds']:
                    kgutils.run_shcmd(Config.cmd_clean['cmds'])
                if Config.state_switch['clean']:
                    kgutils.run_shcmd(Config.state_switch['clean'])

                # TEMP
                out, err, retcode = kgutils.run_shcmd('make', cwd=papi_realpath)
                if retcode != 0:
                    kgutils.logger.warn('Papi counter raw data is not correctly generated.: %s'%err)

            if os.path.exists(data_papi_path) and len(glob.glob( '%s/*'%data_papi_path )) > 0 and Config.model['reuse_rawdata']:

                kgutils.logger.info('Generating model file: %s/%s'%(Config.path['outdir'], Config.modelfile))

                # collect data
                papis = {} # mpirank:omptid:invoke=[(fileid, linenum, numvisits), ... ]
                papimin = 1E100
                papimax = 0
                npapis = 0
                nexcluded_under = 0
                nexcluded_over = 0
 
                mpipaths = []
                for item in os.listdir(data_papi_path):
                    try:
                        mpirank, ompthread = item.split('.')
                        if mpirank.isdigit() and ompthread.isdigit():
                            mpipaths.append((data_papi_path, mpirank, ompthread))
                    except:
                        pass

                nprocs = min( len(mpipaths), multiprocessing.cpu_count()*1)

                if nprocs == 0:
                    kgutils.logger.warn('No papi data files are found.')
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
                        papi, pmeta = outq.get()
                        update(papis, papi)
                        papimin = min(papimin, pmeta[0])
                        papimax = max(papimax, pmeta[1])
                        npapis += pmeta[2]
                        nexcluded_under += pmeta[3]
                        nexcluded_over += pmeta[4]

                    for idx in range(nprocs):
                        procs[idx].join()

                    kgutils.logger.info('# of excluded samples: under limit = %d, over limit = %d'%(nexcluded_under, nexcluded_over))

                if len(papis) == 0:
                    if not _DEBUG:
                        shutil.rmtree(data_papi_path)
                    kgutils.logger.warn('Papi data collection is not right. Deleting corrupted data.')
                else:
                    try:
                        papi_sections = [ 'counters', 'summary']

                        self.addmodel('papi', papi_sections)

                        # papi section
                        papi = []
                        #fd.write('; <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number><num of invocations> ...\n')

                        for ranknum, threadnums in papis.items():
                            for threadnum, invokenums in threadnums.items():
                                for invokenum, pvalue  in invokenums.items():
                                    papi.append( ( '%s %s %s'%(ranknum, threadnum, invokenum), str(pvalue) ) )
                        self.addsection('papi', 'counters', papi)

                        summary = []
                        summary.append( ('minimum_papicounter', str(papimin)) )
                        summary.append( ('maximum_papicounter', str(papimax)) )
                        summary.append( ('number_papicounters', str(npapis)) )
                        self.addsection('papi', 'summary', summary )

                    except Exception as e:
                        kgutils.logger.error(str(e))
            else:
                if not _DEBUG:
                    shutil.rmtree(data_papi_path)
                kgutils.logger.info('failed to generate papi counter information')

            out, err, retcode = kgutils.run_shcmd('make recover', cwd=papi_realpath)

            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])
        else: # check if coverage should be invoked
            kgutils.logger.info('Reusing Papi counter file: %s/%s'%(Config.path['outdir'], Config.modelfile))

        # check if papi data exists in model file
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.modelfile)):
            kgutils.logger.warn('No papi counter file is found.')
        else:
            # read ini file
            kgutils.logger.info('Reading %s/%s'%(Config.path['outdir'], Config.modelfile))

            cfg = configparser.ConfigParser()
            cfg.optionxform = str
            cfg.read('%s/%s'%(Config.path['outdir'], Config.modelfile))

            try:

                papimin = float(cfg.get('papi.summary', 'minimum_papicounter').strip())
                papimax = float(cfg.get('papi.summary', 'maximum_papicounter').strip())
                npapis = int(cfg.get('papi.summary', 'number_papicounters').strip())
                papidiff = papimax - papimin

                # <MPI rank> < OpenMP Thread> <invocation order> =  <file number>:<line number>:<num papis> ... 
                if papidiff == 0:
                    nbins = 1
                else:
                    nbins = max(min(Config.model['types']['papi']['nbins'], npapis), 2)

                kgutils.logger.info('nbins = %d'%nbins)
                kgutils.logger.info('papimin = %f'%papimin)
                kgutils.logger.info('papimax = %f'%papimax)
                kgutils.logger.info('papidiff = %f'%papidiff)
                kgutils.logger.info('npapis = %d'%npapis)
                
                
                if nbins > 1:
                    papibins = [ {} for _ in range(nbins) ]
                    papicounts = [ 0 for _ in range(nbins) ]
                else:
                    papibins = [ {} ]
                    papicounts = [ 0 ]

                idx = 0
                # TODO: conver to counters
                for opt in cfg.options('papi.counters'):
                    ranknum, threadnum, invokenum = tuple( num for num in opt.split() )
                    count = cfg.getint('papi.counters', opt)

                    if nbins > 1:
                        binnum = int(math.floor((count - papimin) / papidiff * (nbins - 1)))
                    else:
                        binnum = 0

                    papicounts[binnum] += 1

                    invokenum = int(invokenum)
                    if invokenum not in papibins[binnum]:
                        papibins[binnum][invokenum] = {}
                    if ranknum not in papibins[binnum][invokenum]:
                        papibins[binnum][invokenum][ranknum] = {}
                    if threadnum not in papibins[binnum][invokenum][ranknum]:
                        papibins[binnum][invokenum][ranknum][threadnum] = count
                    else:
                        raise Exception('Dupulicated data: (%s, %s, %s, %d)'%(invokenum, ranknum, threadnum, count))

                    idx += 1

                    if idx % 100000 == 0:
                        print 'Processed %d items: %s'%(idx, datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y"))
            except Exception as e:
                raise Exception('Please check the format of papi counter file: %s'%str(e))

            # types of representation
            # average, median, min/max, n-stratified, distribution
            # bins with histogram

            totalcount = sum(papicounts)
            countdist = [ float(c) / float(totalcount) for c in papicounts ]
            ndata = Config.model['types']['papi']['ndata']
            datacollect = [ int(round(dist * ndata)) for dist in countdist ]

            # TODO: convert to counters
            triples = []
            for binnum, papibin in enumerate(papibins):
                bin_triples = []
                print 'From bin # %d [ %f (sec) ~ %f (sec) ] %f %% of %d'%(binnum, \
                    binnum*(papimax-papimin)/nbins + papimin if binnum > 0  else papimin, \
                    (binnum+1)*(papimax-papimin)/nbins + papimin if binnum < (nbins-1)  else float('inf'), \
                    countdist[binnum] * 100, totalcount)

                for invokenum in sorted(papibin.keys()):
                    if len(bin_triples) >= datacollect[binnum]: break
                    # select datacollect[binum] under this data tree, rank/thread/invoke
                    bininvokes = papibin[invokenum].keys()
                    random.shuffle(bininvokes)
                    for ranknum in bininvokes:
                        if len(bin_triples) >= datacollect[binnum]: break
                        binranks = papibin[invokenum][ranknum].keys()
                        random.shuffle(binranks)
                        for threadnum in binranks:
                            bin_triples.append( (ranknum, threadnum, invokenum) )
                            print '        invocation triple: %s:%s:%s'%(ranknum, threadnum, invokenum)
                triples.extend(bin_triples)

            print 'Number of bins: %d'%nbins
            print 'Minimun papi count: %f'%papimin
            print 'Maximum papi count: %f'%papimax
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

        papi_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['papi']))

        org_files = [ filepath for filepath, (sfile, mods_used, units_used) in Config.used_srcfiles.iteritems() if sfile.used4papi ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['filepath'])

        with open('%s/Makefile'%papi_realpath, 'wb') as f:

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
                    self.write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
            elif Config.state_switch['type']=='copy':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'cp -f %s/%s %s'%(papi_realpath, basename, Config.state_switch['directory']), t=True)
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

