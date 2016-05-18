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
from kgen_utils import run_shcmd
from kgen_compiler import GenericFortranCompiler
from compflag_config import CompFlagConfig
import subprocess

STR_EX = 'execve('
STR_EN = 'ENOENT'
STR_UF = '<unfinished'
TEMP_SH = '#!/bin/bash\n%s\n%s'
SH = '%s/_kgen_compflag_cmdwrapper.sh'

# Known compilers
#Oracle: f77, f95, f90
#PGI: pgf77, pgfortran, pghpf
#PathScale: pathf90, pathf95
#GNU: gfortran
#Intel: ifort
#Silverfrost: ftn77, ftn95
#NAG: nagfor
#IBM XL: xlf, xlf90, xlf95, xlf2003, and xlf2008

known_compilers = [ 'f77', 'f95', 'f90', 'pgf77', 'pgfortran', 'pghpf', 'pathf90', 'pathf95', 'gfortran', 'ifort', \
    'ftn77', 'ftn95', 'nagfor', 'xlf', 'xlf90', 'xlf95', 'xlf2003', 'xlf2008']
known_mpicompiler_wrappers = [ 'mpief90', 'mpif77', 'mpif90', 'mpifc', 'mpiifort', 'mpipf77', 'mpipf90', 'mpiphpf', 'mpifort', 'mpfort']
COMPILERS = known_compilers + known_mpicompiler_wrappers

def _getpwd(env):
    for item in env:
        if item.startswith('PWD='):
            return item[4:]
    return None

class CompFlagDetect(KGenTool):

    def init(self, argv=None):
        print 'Starting CompFlag'
        self.config= CompFlagConfig(argv=argv)

    def main(self):
        
        if self.config.strace['infile']:
            pass
        elif not os.path.exists(os.path.join(self.config.build['cwd'], self.config.strace['outfile'])) or \
            'all' in self.config.rebuild or 'strace' in self.config.rebuild:

            print 'Building application using command-line of "%s; %s"'%(self.config.build['initcmd'], self.config.build['cmdline'])

            with open(SH%self.config.build['cwd'], 'w') as f:
                f.write(TEMP_SH%(elf.config.build['initcmd'], self.config.build['cmdline']))
            st = os.stat(SH%self.config.build['cwd'])
            os.chmod(SH%self.config.build['cwd'], st.st_mode | stat.S_IEXEC)

            shcmds = 'strace -o %s -f -q -s 100000 -e trace=execve -v -- %s'%\
                (self.config.strace['outfile'], SH%self.config.build['cwd'])

            out, err, retcode = run_shcmd(shcmds, cwd=self.config.build['cwd'])

            os.remove(SH%self.config.build['cwd'])

            if retcode==0:
                self.config.strace['infile'] = self.config.strace['outfile']
            else:
                print 'ERROR: Application build command is failed.'

                print 'CMDS: ', shcmds
                print 'RETCODE: ', retcode
                print 'STDOUT: ', out
                print 'STDERR: ', err
                sys.exit(-1)

        if os.path.exists(os.path.join(self.config.build['cwd'], self.config.strace['outfile'])) and \
            self.config.strace['infile'] is None:
            self.config.strace['infile'] = self.config.strace['outfile']
             

    def fini(self):
        import ConfigParser

        incini = os.path.join(self.config.build['cwd'], self.config.ini['outfile'])
        if not os.path.exists(incini) or 'all' in self.config.rebuild or 'include' in self.config.rebuild:

            print 'Creating KGen include file'

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


            if self.config.strace['infile']:
                flags = {}
                with open(os.path.join(self.config.build['cwd'], self.config.strace['infile']), 'r') as f:
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
                                        if exepath and cmdlist and exepath.split('/')[-1]==cmdlist[0].split('/')[-1] and \
                                            cmdlist[0].split('/')[-1] in COMPILERS:
                                            srcs, incs, macros, openmp, options = GenericFortranCompiler.parse_option(cmdlist, _getpwd(env))
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

        return { 'incini': incini }

if __name__ == '__main__':
    compflag = CompFlagDetect()
    compflag.init()
    compflag.main()
    compflag.fini()

