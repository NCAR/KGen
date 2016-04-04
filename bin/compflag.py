#!/usr/bin/python
#
# compflag.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )

import os
import sys
import subprocess
import ConfigParser
import optparse

KGEN_BASE = '%s/../base'%os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, KGEN_BASE)

from kgen_compiler import GenericCompiler

def run_shcmd(cmd, input=None, **kwargs):
    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    out, err = proc.communicate(input=input)
    return out, err, proc.returncode

STR_EX = 'execve('
STR_EN = 'ENOENT'

def _getpwd(env):
    for item in env:
        if item.startswith('PWD='):
            return item[4:]
    return None

def main():
    parser = optparse.OptionParser()
    parser.add_option("-i", "--infile", dest="infile", action='store', type='string', help="strace file")
    parser.add_option("-o", "--outfile", dest="outfile", action='store', type='string', help="output file")
    options, args = parser.parse_args()

    flags = {}
    stracefile = None
    if options.infile:
        stracefile = options.infile
    else:
        exepath = args[0]
        exedir, exefile = os.path.split(os.path.realpath(exepath))
        qargs = [ '"%s"'%arg for arg in args[1:] ]
        shcmds = 'cd %s; strace -o strace.log -f -q -s 100000 -e trace=execve -v -- ./%s %s'%( exedir, exefile, ' '.join(qargs))

        print 'Building application using command-line of "%s" at "%s"'%(exefile, exedir)
        out, err, retcode = run_shcmd(shcmds)

        if retcode==0:
            stracefile = '%s/strace.log'%exedir
        else:
            print 'ERROR: Application build command is failed.'

            #print 'CMDS: ', shcmds
            #print 'RETCODE: ', retcode
            #print 'STDERR: ', err
            sys.exit(-1)

    #import pdb; pdb.set_trace()
    print 'Collecting compiler flags from "%s"'%stracefile
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
                            srcs, incs, macros = GenericCompiler.parse_option(cmdlist, _getpwd(env))
                            if len(srcs)==1:
                                if len(incs)>0 or len(macros)>0:
                                    if srcs[0] in flags:
                                        flags[srcs[0]].append((incs, macros))
                                    else:
                                        flags[srcs[0]] = (incs, macros)
                            elif len(srcs)>1:
                                raise
                            #import pdb; pdb.set_trace()
                        except:
                            pass
            line = f.readline()
 
    if options.outfile:
        outfile = options.outfile
    else:
        outfile = 'include.ini'
    print 'Generating "%s"'%outfile
    
    Config = ConfigParser.RawConfigParser()
    Config.optionxform = str

    for fname, (incs, macros) in flags.items():
        if Config.has_section(fname):
            print 'Warning: %s section is dupulicated.' % fname
        else:
            Config.add_section(fname)
            Config.set(fname,'include',':'.join(incs))
            for name, value in macros:
                Config.set(fname, name, value)

    with open(outfile,'w') as f:
        Config.write(f)

if __name__ == '__main__':
    main()

