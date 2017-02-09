'''KGen coverage detector
'''

import os
import re
import glob
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

            coverage_paths = Config.plugindb['coverage_paths']
            maxfiles = len(coverage_paths)
            maxlines = max( len(lineids) for fileid, lineids in coverage_paths.values() )
            
            totallines = { fileid: len(lineids) for fileid, lineids in coverage_paths.values() }

            with open(Config.coveragefile, 'w') as fd:
                fd.write('[file]\n')
                fd.write('; <file number> = <path to file>\n')
                for path, (pathnum, lines) in coverage_paths.items():
                    fd.write('%d = %s/%s.kgen\n'%(pathnum, os.path.abspath(Config.path['coverage']), os.path.basename(path)))
                fd.write('\n')
                fd.write('[data]\n')
                fd.write('; <item number> = <file number> <line number> <MPI rank> < OpenMP Thread> <number of invocations>\n')

            # clean app
            #if Config.cmd_clean['cmds']:
            #    kgutils.run_shcmd(Config.cmd_clean['cmds'])

            # TEMP
            #out, err, retcode = kgutils.run_shcmd('make', cwd=Config.path['coverage'])
            #if retcode != 0:
            #    #kgutils.logger.info('Failed to generate coverage information: %s : %s'%(out, err))
            #    kgutils.logger.info('Failed to generate coverage information: %s'%err)

            kgutils.logger.info('Application is built/run with coverage instrumentation.')

            # TODO: wait until coverage data generation is completed
            # use -K option fir bsub to wait for job completion

            def get_linenum(fileid, lineid):
                for path, (pathnum, lines) in coverage_paths.items():
                    if fileid != pathnum: continue
                    for linenum, lid in lines.items():
                        if lid == lineid: return linenum
                return -1

            count = 0
            blocks = {}
            with open(Config.coveragefile, 'a') as fd:
                for data in glob.glob('%s/coverage.data*'%Config.path['coverage']):
                    with open(data, 'r') as fc:
                        splitpath = data.split('.')
                        rank = splitpath[-2]
                        thread = splitpath[-1]
                        for fileid in range(maxfiles):
                            blocks[fileid] = {}
                            for lineid in range(maxlines):
                                invoke = fc.read(10).strip()
                                linenum = get_linenum(fileid, lineid)
                                if linenum >= 0:
                                    if invoke != '0':
                                        fd.write('%d = %d %d %s %s %s\n'%(count, fileid, get_linenum(fileid, lineid), rank, thread, invoke))
                                        count += 1
                                        if linenum not in blocks[fileid]:
                                            blocks[fileid][linenum] = 1
                                        else:
                                            blocks[fileid][linenum] += 1
                                elif invoke != '0':
                                    raise Exception('Coverage data file check failure: %d = %d %d %s %s %s'%\
                                        (count, fileid, get_linenum(fileid, lineid), rank, thread, invoke))

            kgutils.logger.info('KGen coverage file is generated: %s'%Config.coveragefile)
            kgutils.logger.info('    ***** In this kernel of "%s" *****:'%Config.kernel['name'])
            kgutils.logger.info('    * %d original source files are used.'%maxfiles)
            kgutils.logger.info('    * %d conditional blocks exist in the original source files.'%sum(totallines.values()))
            kgutils.logger.info('    * %d conditional blocks are invoked at least once among all the conditional blocks.'%\
                sum( len(linenums) for  linenums in blocks.values()) )

        else:
            kgutils.logger.info('Reusing KGen coverage file: %s'%Config.coveragefile)

        # generate skeleton source file

        # read ini file
        #configparser.ConfigParser
        cfg = configparser.ConfigParser()
        cfg.optionxform = str
        cfg.read(Config.coveragefile)

        filemap = {}
        for opt in cfg.options('file'):
            filemap[int(opt)] = cfg.get('file', opt) 

        invokes = {} # fileid, lineno, rank, thread, no. of invokes
        for opt in cfg.options('data'):

            fileid, lineno, rank, thread, num_invokes = tuple( int(num) for num in cfg.get('data', opt).split() )
            lineno -= 1

            if fileid not in invokes:
                invokes[fileid] = {}
            if lineno not in invokes[fileid]:
                invokes[fileid][lineno] = {}
            if rank not in invokes[fileid][lineno]:
                invokes[fileid][lineno][rank] = {}
            if thread not in invokes[fileid][lineno][rank]:
                invokes[fileid][lineno][rank][thread] = num_invokes
            else:
                raise Exception('Dupulicated invokes: %s'%cfg.get('data', opt))

        summary = {}
        for fileid, lines in invokes.items():
            summary[fileid] = {}
            for lineid, ranks in lines.items():
               
                rankinvokes = {}
                threadinvokes = {}
                for rank, threads in ranks.items(): 
                    rankinvokes[rank] = sum(threads.values())
                    for thread, invokes in threads.items():
                        if thread in threadinvokes:
                            threadinvokes[thread] += invokes
                        else:
                            threadinvokes[thread] = invokes
                totalinvokes = sum(rankinvokes.values())

                summary[fileid][lineid] = [ \
                ';; Total number of invokes: %d'%totalinvokes, \
                ';; MPI rank(invokes) : %s' % ' '.join(['%d(%d)'%(r,i) for r,i in rankinvokes.items()]), \
                ';; OpenMP thread(invokes) : %s' % ' '.join(['%d(%d)'%(t,i) for t,i in threadinvokes.items()]) ]
 
        #import pdb; pdb.set_trace()

        #open src file
        #add lines
        for fileid, filepath in filemap.items():
            if fileid not in summary: continue

            with open(filepath, 'r') as fsrc:
                srclines = fsrc.readlines()

            if 'totallines' in locals():
                filesummary = [ \
                    ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;', \
                    '; %d conditional blocks exist in this file'%totallines[fileid], \
                    '; %d conditional blokcs are invoked at least once among all the conditional blocks.'%len(blocks[fileid]), \
                    ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' \
                ]
                srclines[0] = '%s\n%s\n'%('\n'.join(filesummary), srclines[0])

            for lineno, invokelines in summary[fileid].items():
                srclines[lineno] = '%s\n%s\n'%(srclines[lineno], '\n'.join(invokelines))

            with open('%s.coverage'%'.'.join(filepath.split('.')[:-1]), 'w') as fdst:
                fdst.write(''.join(srclines))


        # run app
        #if Config.cmd_run['cmds']:
        #    kgutils.run_shcmd(Config.cmd_run['cmds'])


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
