#!/usr/bin/python
#
# compflag.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )

import os
import sys
import optparse
from collections import OrderedDict

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)


class CompFlagConfig(object):
    def __init__(self, argv=None):

        self.attrs = OrderedDict()

        self.attrs['compflag'] = OrderedDict()
        self.attrs['compflag']['version'] = [ 0, 1, '0' ]

        self.attrs['strace'] = OrderedDict()
        self.attrs['strace']['infile'] = None
        self.attrs['strace']['outfile'] = None

        self.attrs['build'] = OrderedDict()
        self.attrs['build']['cmdline'] = ''
        self.attrs['build']['cwd'] = '.'

        self.attrs['ini'] = OrderedDict()
        self.attrs['ini']['outpath'] = '.'

        parser = optparse.OptionParser()

        parser.add_option("-s", "--strace", dest="strace", action='append', type='string', default=None, help="strace options")
        parser.add_option("-b", "--build", dest="build", action='append', type='string', default=None, help="build options")
        parser.add_option("-i", "--ini", dest="ini", action='append', type='string', default=None, help="INI options")

        opts, args = parser.parse_args(args=argv)
#        
#        if len(args)<1:
#            print 'ERROR: At least one argument is required.'
#            sys.exit(-1)
#
#        new_args = []
#        for arg in args:
#            temp_arg = arg.strip('"')
#            new_args.append(temp_arg.strip("'"))
        self.attrs['build']['cmdline'] = ' '.join(args)

        if opts.strace:
            self._save_opt(opts.strace, self.attrs['strace'])

        if opts.build:
            self._save_opt(opts.build, self.attrs['build'])

        if opts.ini:
            self._save_opt(opts.ini, self.attrs['ini'])

    def _save_opt(self, opt, attr):
        if isinstance(opt, str):
            opt = [ opt ]

        if isinstance(opt, ( list, tuple ) ):
            for o in opt:
                if isinstance(o, str):
                    subopts = o.split(',')
                    for subopt in subopts:
                        if subopt:
                            if subopt.find('=')>0:
                                key, value = subopt.split('=', 1)
                                attr[key] = value 
                            else:
                                attr[subopt] = True 
                else:
                    print 'UNKNOWN TYPE: %s'%o.__class__
                    sys.exit(-1)
        else:
            print 'UNKNOWN TYPE: %s'%opt.__class__
            sys.exit(-1)

    def __getattr__(self, name):
        return self.attrs[name]

#
#
#
#
#
#
#
#
#
#
#
#
#KGEN_COMPFLAG = os.path.dirname(os.path.realpath(__file__))
#KGEN_HOME = '%s/../..'%KGEN_COMPFLAG
#KGEN_BASE = '%s/base'%KGEN_HOME
#
#sys.path.insert(0, KGEN_BASE)
#sys.path.insert(0, KGEN_COMPFLAG)
#
#from kgen_tool import KGenTool
#from kgen_compiler import GenericFortranCompiler
#import subprocess
#
#
#def run_shcmd(cmd, input=None, **kwargs):
#    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
#        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
#    out, err = proc.communicate(input=input)
#    return out, err, proc.returncode
#
#STR_EX = 'execve('
#STR_EN = 'ENOENT'
#
#def _getpwd(env):
#    for item in env:
#        if item.startswith('PWD='):
#            return item[4:]
#    return None
#
#class KCompFlagDetect(KGenTool):
#
#    def init(self, standalone=True):
#
#
#    def main():
#        pass
#
#    def fini():
#        pass
#
#class CompflagDetector(object):
#    def __init__(self, argv1=None):
#        parser = optparse.OptionParser()
#        parser.add_option("-P", "--import", dest="importobj", action='append', type='string', help="import objects")
#        parser.add_option("-I", "--include", dest="include", action='append', type='string', help="include paths")
#        parser.add_option("-D", "--macro", dest="macro", action='append', type='string', help="macro definitions")
#        parser.add_option("-i", "--infile", dest="infile", action='store', type='string', help="strace file")
#        parser.add_option("-o", "--outfile", dest="outfile", action='store', type='string', help="output file")
#        options, args = parser.parse_args(args=sys.argv1)
#
#        flags = {}
#        stracefile = None
#        if options.infile:
#            stracefile = options.infile
#        else:
#            exepath = args[0]
#            exedir, exefile = os.path.split(os.path.realpath(exepath))
#            qargs = [ '"%s"'%arg for arg in args[1:] ]
#            shcmds = 'cd %s; strace -o strace.log -f -q -s 100000 -e trace=execve -v -- %s %s'%( exedir, exefile, ' '.join(qargs))
#
#            print 'Building application using command-line of "%s" at "%s"'%(exefile, exedir)
#            out, err, retcode = run_shcmd(shcmds)
#
#            if retcode==0:
#                stracefile = '%s/strace.log'%exedir
#            else:
#                print 'ERROR: Application build command is failed.'
#
#                print 'CMDS: ', shcmds
#                print 'RETCODE: ', retcode
#                print 'STDERR: ', err
#                sys.exit(-1)
#
#        #import pdb; pdb.set_trace()
#        print 'Collecting compiler flags from "%s"'%stracefile
#        with open(stracefile, 'r') as f:
#            line = f.readline()
#            while(line):
#                pos_execve = line.find(STR_EX)
#                if pos_execve >= 0:
#                    pos_enoent = line.find(STR_EN) 
#                    if pos_enoent < 0:
#                        pos_lastparen = line.rfind(')')
#                        if pos_lastparen >=0:
#                            execve_args = line[pos_execve+len(STR_EX):pos_lastparen]
#                            try:
#                                exec('exepath, cmdlist, env = %s'%execve_args)
#                                # check compiler command line: vendor, version, sourcefile, -I, -D, openmp, mpi??
#                                srcs, incs, macros = GenericFortranCompiler.parse_option(cmdlist, _getpwd(env))
#                                if len(srcs)==1:
#                                    #if len(incs)>0 or len(macros)>0:
#                                    if srcs[0] in flags:
#                                        flags[srcs[0]].append((incs, macros))
#                                    else:
#                                        flags[srcs[0]] = [ (incs, macros) ]
#                                elif len(srcs)>1:
#                                    raise
#                                #import pdb; pdb.set_trace()
#                            except:
#                                pass
#                line = f.readline()
#     
#        if options.outfile:
#            outfile = options.outfile
#        else:
#            outfile = 'include.ini'
#        print 'Generating "%s"'%outfile
#        
#        Config = ConfigParser.RawConfigParser()
#        Config.optionxform = str
#
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
#
#        for fname, incitems in flags.items():
#            if len(incitems)==1:
#                incs = incitems[0][0]
#                macros = incitems[0][1]
#            else:
#                # Use the first item found, for temporary
#                incs = incitems[0][0]
#                macros = incitems[0][1]
#
#            if Config.has_section(fname):
#                print 'Warning: %s section is dupulicated.' % fname
#            else:
#                Config.add_section(fname)
#                Config.set(fname,'include',':'.join(incs))
#                for name, value in macros:
#                    Config.set(fname, name, value)
#
#        with open(outfile,'w') as f:
#            Config.write(f)
