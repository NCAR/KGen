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
TEMP_SH = '#!/bin/bash\n%s'
SH = '%s/_kgen_temp.sh'

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

#            exedir, exefile = os.path.split(os.path.realpath(exepath))
#            qargs = [ '"%s"'%arg for arg in args[1:] ]
#            shcmds = 'cd %s; strace -o strace.log -f -q -s 100000 -e trace=execve -v -- %s %s'%( exedir, exefile, ' '.join(qargs))

            print 'Building application using command-line of "%s"'%self.config.build['cmdline']
            out, err, retcode = run_shcmd(shcmds, cwd=self.config.build['cwd'])

            #os.remove(SH%self.config.build['cwd'])

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
                    pos_enoent = line.find(STR_EN)
                    if pos_enoent < 0:
                        pos_lastparen = line.rfind(')')
                        if pos_lastparen >=0:
                            execve_args = line[pos_execve+len(STR_EX):pos_lastparen]
                            try:
                                exec('exepath, cmdlist, env = %s'%execve_args)
                                # check compiler command line: vendor, version, sourcefile, -I, -D, openmp, mpi??
                                if exepath and cmdlist and exepath.split('/')[-1]==cmdlist[0].split('/')[-1] and \
                                    cmdlist[0].split('/')[-1] in [ 'ifort', 'gfortran', 'pgfortran' ]:
                                    srcs, incs, macros = GenericFortranCompiler.parse_option(cmdlist, _getpwd(env))
                                    if len(srcs)==1:
                                        #if len(incs)>0 or len(macros)>0:
                                        if srcs[0] in self.flags:
                                            self.flags[srcs[0]].append((incs, macros))
                                        else:
                                            self.flags[srcs[0]] = [ (incs, macros) ]
                                    elif len(srcs)>1:
                                        raise
                                    #import pdb; pdb.set_trace()
                            except:
                                pass
                line = f.readline()

    def fini(self):
        import ConfigParser

        if self.config.strace['outfile']:
            outfile = self.config.strace['outfile']
        else:
            outfile = 'include.ini'

        Config = ConfigParser.RawConfigParser()
        Config.optionxform = str

#        if options.include:
#            Config.add_section('include')
#            for inc in options.include:
#                Config.set('include', inc, '')
#
#        if options.macro:
#            Config.add_section('macro')
#            for macro in options.macro:
#                splitmacro = macro.split('=')
#                if len(splitmacro)==2:
#                    Config.set('macro', splitmacro[0], splitmacro[1])
#                elif len(splitmacro)==2:
#                    Config.set('macro', macro, '')
#                else: raise
#
#        if options.importobj:
#            Config.add_section('import')
#            for impobj in options.importobj:
#                splitmacro = impobj.split('=')
#                if len(splitmacro)==2:
#                    Config.set('import', splitmacro[0], splitmacro[1])
#                else: raise

        for fname, incitems in self.flags.items():
            if len(incitems)==1:
                incs = incitems[0][0]
                macros = incitems[0][1]
            else:
                # Use the first item found, for temporary
                incs = incitems[0][0]
                macros = incitems[0][1]

            if Config.has_section(fname):
                print 'Warning: %s section is dupulicated.' % fname
            else:
                Config.add_section(fname)
                Config.set(fname,'include',':'.join(incs))
                for name, value in macros:
                    Config.set(fname, name, value)

        incini = '%s/%s'%(self.config.build['cwd'], outfile)
        with open(incini,'w') as f:
            Config.write(f)

        return { 'incini': incini }

if __name__ == '__main__':
    compflag = CompFlagDetect()
    compflag.init()
    compflag.main()
    compflag.fini()

