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
TEMP_SH = '#!/bin/bash\n%s'
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

        self.config= CompFlagConfig(argv=argv)

    def main(self):
        
        if self.config.strace['infile']:
            stracefile = self.config.strace['infile']
        else:
            with open(SH%self.config.build['cwd'], 'w') as f:
                f.write(TEMP_SH%self.config.build['cmdline'])
            st = os.stat(SH%self.config.build['cwd'])
            os.chmod(SH%self.config.build['cwd'], st.st_mode | stat.S_IEXEC)

            shcmds = 'strace -o strace.log -f -q -s 100000 -e trace=execve -v -- %s'%(SH%self.config.build['cwd'])

            print 'Building application using command-line of "%s"'%self.config.build['cmdline']
            out, err, retcode = run_shcmd(shcmds, cwd=self.config.build['cwd'])

            os.remove(SH%self.config.build['cwd'])

            if retcode==0:
                stracefile = '%s/strace.log'%self.config.build['cwd']
            else:
                print 'ERROR: Application build command is failed.'

                print 'CMDS: ', shcmds
                print 'RETCODE: ', retcode
                print 'STDOUT: ', out
                print 'STDERR: ', err
                sys.exit(-1)

        self.flags = {}
        with open(stracefile, 'r') as f:
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
                                    srcs, incs, macros, openmp = GenericFortranCompiler.parse_option(cmdlist, _getpwd(env))
                                    if len(srcs)>0:
                                        for src in srcs:
                                            if src in self.flags:
                                                self.flags[src].append((exepath, incs, macros, openmp))
                                            else:
                                                self.flags[src] = [ (exepath, incs, macros, openmp) ]
                            except:
                                pass
                line = f.readline()

    def fini(self):
        import ConfigParser

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

        for fname, incitems in self.flags.items():
            if len(incitems)>0:
                # save the last compiler set
                compiler = incitems[-1][0]
                incs = incitems[-1][1]
                macros = incitems[-1][2]

                if Config.has_section(fname):
                    print 'Warning: %s section is dupulicated.' % fname
                else:
                    Config.add_section(fname)
                    Config.set(fname,'compiler', compiler)
                    Config.set(fname,'include',':'.join(incs))
                    for name, value in macros:
                        Config.set(fname, name, value)

        outfile = self.config.strace['outfile']
        if outfile is None: outfile = 'include.ini'

        incini = '%s/%s'%(self.config.build['cwd'], outfile)
        with open(incini,'w') as f:
            Config.write(f)

        return { 'incini': incini }

if __name__ == '__main__':
    compflag = CompFlagDetect()
    compflag.init()
    compflag.main()
    compflag.fini()

