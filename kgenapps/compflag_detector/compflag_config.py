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
