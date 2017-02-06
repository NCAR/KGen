'''Compiler flag detection
'''

import os
import kgtool
import kgutils
import kgcompiler
import ConfigParser
from kgconfig import Config

STR_EX = 'execve('
STR_EN = 'ENOENT'
STR_UF = '<unfinished'

class CompFlag(kgtool.KGTool):

    straceoutfile = 'strace.log'
    includeoutfile = 'include.ini'

    def run(self):

        # clean app.
        if Config.cmd_clean['cmds']:
            kgutils.run_shcmd(Config.cmd_clean['cmds'])

        # build app.
        if not os.path.exists(self.straceoutfile) or 'all' in Config.rebuild or 'strace' in Config.rebuild:
            bld_cmd = 'strace -o strace.log -f -q -s 100000 -e trace=execve -v -- %s/_kgen_compflag_cmdwrapper.sh'%Config.cwd
            kgutils.logger.info('Creating KGen strace logfile: %s'%self.straceoutfile)
            kgutils.run_shcmd(bld_cmd)
        else:
            kgutils.logger.info('Reusing KGen strace logfile: %s'%self.straceoutfile)

        # parse strace.log and generate include.ini
        if not os.path.exists(self.includeoutfile) or 'all' in Config.rebuild or 'include' in Config.rebuild:
            self._geninclude()
        else:
            kgutils.logger.info('Reusing KGen include file: %s'%self.includeoutfile)

        # save info to cfg
        Config.includefile = self.includeoutfile

    def _getpwd(self, env):
        for item in env:
            if item.startswith('PWD='):
                return item[4:]
        return None

    def _geninclude(self):

        kgutils.logger.info('Creating KGen include file: %s'%self.includeoutfile)

        Config = ConfigParser.RawConfigParser()
        Config.optionxform = str

        if len(Config.cmdarg['includepath'])>0:
            Config.add_section('include')
            for path in Config.cmdarg['includepath']:
                Config.set('include', path, '')

        if len(Config.cmdarg['macro'])>0:
            Config.add_section('macro')
            for key, value in Config.cmdarg['macro']:
                Config.set('macro', key, value)


        flags = {}
        with open(self.straceoutfile, 'r') as f:
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

                if Config.has_section(fname):
                    print 'Warning: %s section is dupulicated.' % fname
                else:
                    Config.add_section(fname)
                    Config.set(fname,'compiler', compiler)
                    Config.set(fname,'compiler_options', ' '.join(options))
                    Config.set(fname,'include',':'.join(incs))
                    for name, value in macros:
                        Config.set(fname, name, value)

        if len(Config.sections())>0:
            with open(self.includeoutfile, 'w') as f:
                Config.write(f)

