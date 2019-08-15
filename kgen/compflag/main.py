'''Compiler flag detection
'''

import os
import stat
import kgtool
import kgutils
import kgcompiler
from kgconfig import Config
try:
    import configparser
except:
    import ConfigParser as configparser

STR_EX = 'execve('
STR_EN = 'ENOENT'
STR_UF = '<unfinished'
STR_RE = 'resumed>'
#TEMP_SH = '#!/bin/bash\n%s\n%s\n%s\n'
#SH = '%s/_kgen_compflag_cmdwrapper.sh'

class CompFlag(kgtool.KGTool):

    def run(self):

        # build app.
        stracepath = '%s/%s'%(Config.path['outdir'], Config.stracefile)
        includepath = '%s/%s'%(Config.path['outdir'], Config.includefile)

        if not os.path.exists(stracepath) or 'all' in Config.rebuild or 'strace' in Config.rebuild:

            # clean app.
            if Config.cmd_clean['cmds']:
                kgutils.run_shcmd(Config.cmd_clean['cmds'])
            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])

            #with open(SH%Config.cwd, 'w') as f:
            #    f.write(TEMP_SH%(Config.cmd_clean['cmds'], Config.prerun['build'], Config.cmd_build['cmds']))
            #st = os.stat(SH%Config.cwd)
            #os.chmod(SH%Config.cwd, st.st_mode | stat.S_IEXEC)
            
            if Config.prerun['build']:
                cmdstr = '%s;%s'%(Config.prerun['build'], Config.cmd_build['cmds'])
            else:
                cmdstr = Config.cmd_build['cmds']

            bld_cmd = 'strace -o %s -f -q -s 100000 -e trace=execve -v -- /bin/sh -c "%s"'%(stracepath, cmdstr)

            kgutils.logger.info('Creating KGen strace logfile: %s'%stracepath)
            try:
                out, err, retcode = kgutils.run_shcmd(bld_cmd)
                if retcode != 0 and os.path.exists(stracepath):
                    os.remove(stracepath)
                    kgutils.logger.error('%s\n%s'%(err, out))
            except Exception as err:
                if os.path.exists(stracepath):
                    os.remove(stracepath)
                kgutils.logger.error('%s\n%s'%(err, out))
                raise
        else:
            kgutils.logger.info('Reusing KGen strace logfile: %s'%stracepath)

        # parse strace.log and generate include.ini
        if not os.path.exists(includepath) or 'all' in Config.rebuild or 'include' in Config.rebuild:
            if stracepath:
                self._geninclude(stracepath, includepath)
            else:
                kgutils.logger.error('strace logfile is not found at: %s'%stracepath)
                kgutils.kgenexit('Please retry KGen after generting strace logfile.')
        else:
            kgutils.logger.info('Reusing KGen include file: %s'%includepath)

    def _getpwd(self, env):
        for item in env:
            if item.startswith('PWD='):
                return item[4:]
        return None

    def _geninclude(self, stracepath, includepath):

        kgutils.logger.info('Creating KGen include file: %s'%includepath)

        cfg = configparser.RawConfigParser()
        cfg.optionxform = str

        if len(Config.include['path'])>0:
            cfg.add_section('include')
            for inc in Config.include['path']:
                for i in inc.split(':'):
                    cfg.set('include', i, '')

        if len(Config.include['macro'])>0:
            cfg.add_section('macro')
            for key, value in Config.include['macro'].items():
                cfg.set('macro', key, value)

        if len(Config.include['import'])>0:
            cfg.add_section('import')
            for key, value in Config.include['macro'].items():
                cfg.set('import', key, value)


        if not os.path.exists(stracepath):
            raise Exception('No strace file is found.')

        flags = {}
        with open(stracepath, 'r') as f:
            line = f.readline()
            while(line):
                pos_execve = line.find(STR_EX)
                if pos_execve >= 0:
                    pos_enoent = line.rfind(STR_EN)
                    if pos_enoent < 0:
                        pos_last = line.rfind(STR_UF)
                        if pos_last < 0:
                            pos_last = line.rfind(']')
                        else:
                            pos_last -= 1
                        if pos_last >= 0:
                            try:
                                exec('exepath, cmdlist, env = %s'%line[pos_execve+len(STR_EX):(pos_last+1)])
                                compid = cmdlist[0].split('/')[-1]
                                if exepath and cmdlist and compid==cmdlist[0].split('/')[-1]:
                                    compiler = kgcompiler.CompilerFactory.createCompiler(compid)
                                    if compiler:
                                        srcs, incs, macros, openmp, options = compiler.parse_option(cmdlist, self._getpwd(env))
                                        if len(srcs)>0:
                                            for src in srcs:
                                                if src in flags:
                                                    flags[src].append((exepath, incs, macros, openmp, options))
                                                else:
                                                    flags[src] = [ (exepath, incs, macros, openmp, options) ]
                            except Exception as err:
                                raise
                                pass
                line = f.readline()

        for fname, incitems in flags.items():
            if len(incitems)>0:
                # save the last compiler set
                compiler = incitems[-1][0]
                incs = incitems[-1][1]
                macros = incitems[-1][2]
                options = incitems[-1][4]

                if cfg.has_section(fname):
                    print 'Warning: %s section is dupulicated.' % fname
                else:
                    cfg.add_section(fname)
                    cfg.set(fname,'compiler', compiler)
                    cfg.set(fname,'compiler_options', ' '.join(options))
                    cfg.set(fname,'include',':'.join(incs))
                    for name, value in macros:
                        cfg.set(fname, name, value)

        if len(cfg.sections())>0:
            with open(includepath, 'w') as f:
                cfg.write(f)

