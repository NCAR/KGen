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
TEMP_SH = '#!/bin/bash\n%s\n%s\n%s\n%s\n'
SH = '%s/_kgen_compflag_cmdwrapper.sh'

class CompFlag(kgtool.KGTool):

    def run(self):

        # clean app.
        if 'clean' not in Config.skip and Config.cmd_clean['cmds']:
            kgutils.run_shcmd(Config.cmd_clean['cmds'])

        # build app.
        if not os.path.exists(Config.stracefile) or 'all' in Config.rebuild or 'strace' in Config.rebuild:


            with open(SH%Config.cwd, 'w') as f:
                f.write(TEMP_SH%(Config.prerun['clean'], Config.cmd_clean['cmds'], \
                    Config.prerun['build'], Config.cmd_build['cmds']))
            st = os.stat(SH%Config.cwd)
            os.chmod(SH%Config.cwd, st.st_mode | stat.S_IEXEC)

            bld_cmd = 'strace -o %s -f -q -s 100000 -e trace=execve -v -- %s/_kgen_compflag_cmdwrapper.sh'%(Config.stracefile, Config.cwd)
            kgutils.logger.info('Creating KGen strace logfile: %s'%Config.stracefile)
            try:
                out, err, retcode = kgutils.run_shcmd(bld_cmd)
                if retcode != 0 and os.path.exists(Config.stracefile):
                    os.remove(Config.stracefile)
                    kgutils.logger.error('%s\n%s'%(err, out))
            except:
                if os.path.exists(Config.stracefile):
                    os.remove(Config.stracefile)
                kgutils.logger.error('%s\n%s'%(err, out))
                raise
        else:
            kgutils.logger.info('Reusing KGen strace logfile: %s'%Config.stracefile)

        # parse strace.log and generate include.ini
        if not os.path.exists(Config.includefile) or 'all' in Config.rebuild or 'include' in Config.rebuild:
            self._geninclude()
        else:
            kgutils.logger.info('Reusing KGen include file: %s'%Config.includefile)

    def _getpwd(self, env):
        for item in env:
            if item.startswith('PWD='):
                return item[4:]
        return None

    def _geninclude(self):

        kgutils.logger.info('Creating KGen include file: %s'%Config.includefile)

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


        flags = {}
        with open(Config.stracefile, 'r') as f:
            line = f.readline()
            while(line):
                pos_execve = line.find(STR_EX)
                if pos_execve >= 0:
                    pos_enoent = line.rfind(STR_EN)
                    if pos_enoent < 0:
                        pos_last = line.rfind(STR_UF)
                        if pos_last < 0:
                            pos_last = line.rfind(')')
                        if pos_last >= 0:
                            try:
                                exec('exepath, cmdlist, env = %s'%line[pos_execve+len(STR_EX):pos_last])
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
                            except:
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
            with open(Config.includefile, 'w') as f:
                cfg.write(f)

