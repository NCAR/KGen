#!/usr/bin/python
#
# compflag.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )

import os
import sys
import stat

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)

KGEN_COMPFLAG = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%KGEN_COMPFLAG
KGEN_BASE = '%s/base'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_COMPFLAG)

from kgen_tool import KGenTool
from kgen_utils import run_shcmd, Logger, ProgramException
from kgen_compiler import CompilerFactory
from compflag_config import CompFlagConfig
import subprocess

STR_EX = 'execve('
STR_EN = 'ENOENT'
STR_UF = '<unfinished'
TEMP_SH = '#!/bin/bash\n%s\n%s\n%s\n%s\n'
SH = '%s/_kgen_compflag_cmdwrapper.sh'

def _getpwd(env):
    for item in env:
        if item.startswith('PWD='):
            return item[4:]
    return None

class CompFlagDetect(KGenTool):

    def init(self, argv=None):
        self.config= CompFlagConfig(argv=argv)

    def main(self):
        
        Logger.info('Starting KOption', stdout=True)

        cwd = self.config.build['cwd']

        slog = os.path.join(cwd, self.config.strace)
        if not os.path.exists(slog) or \
            'all' in self.config.rebuild or 'strace' in self.config.rebuild:

            Logger.info('Building application to collect strace log file: %s'%slog, stdout=True)

            with open(SH%cwd, 'w') as f:
                f.write(TEMP_SH%(self.config.prerun['clean'], self.config.build['clean'], \
                    self.config.prerun['build'], self.config.build['cmdline']))
            st = os.stat(SH%cwd)
            os.chmod(SH%cwd, st.st_mode | stat.S_IEXEC)

            shcmds = 'strace -o %s -f -q -s 100000 -e trace=execve -v -- %s'%\
                (self.config.strace, SH%cwd)

            out, err, retcode = run_shcmd(shcmds, cwd=cwd)

            #os.remove(SH%self.config.build['cwd'])

            if retcode!=0:
                #print 'CMDS: ', shcmds
                #print 'RETCODE: ', retcode
                #print 'STDOUT: ', out
                #print 'STDERR: ', err
                raise ProgramException('Application build command is failed: %s'%err)
        else:
            Logger.info('Reusing strace log file: %s'%self.config.strace, stdout=True)

    def fini(self):
        import ConfigParser

        cwd = self.config.build['cwd']
        incini = os.path.join(cwd, self.config.ini)
        if not os.path.exists(incini) or 'all' in self.config.rebuild or 'include' in self.config.rebuild:

            Logger.info('Creating KGen include file: %s'%incini, stdout=True)

            Config = ConfigParser.RawConfigParser()
            Config.optionxform = str

            if len(self.config.include)>0:
                Config.add_section('include')
                for inc in self.config.include.keys():
                    for i in inc.split(':'):
                        Config.set('include', i, '')

            if len(self.config.macro)>0:
                Config.add_section('macro')
                for key, value in self.config.macro.items():
                    Config.set('macro', key, value)

            if len(self.config.object)>0:
                Config.add_section('import')
                for key, value in self.config.macro.items():
                    Config.set('import', key, value)


            flags = {}
            with open(os.path.join(cwd, self.config.strace), 'r') as f:
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
                                        compiler = CompilerFactory.createCompiler(compid)
                                        if compiler:
                                            srcs, incs, macros, openmp, options = compiler.parse_option(cmdlist, _getpwd(env))
                                            if len(srcs)>0:
                                                for src in srcs:
                                                    if src in flags:
                                                        flags[src].append((exepath, incs, macros, openmp, options))
                                                    else:
                                                        flags[src] = [ (exepath, incs, macros, openmp, options) ]
                                except:
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
                with open(incini, 'w') as f:
                    Config.write(f)
        else:
            Logger.info('Reusing KGen include file: %s'%incini, stdout=True)

        Logger.info('KOption is finished.', stdout=True)

        return { 'incini': incini }

if __name__ == '__main__':
    compflag = CompFlagDetect()
    compflag.init()
    compflag.main()
    compflag.fini()

