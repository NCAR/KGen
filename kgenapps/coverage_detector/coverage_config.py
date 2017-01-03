#!/usr/bin/python
#
# coverage.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )

import os
import sys
import optparse
from collections import OrderedDict
from kgen_utils import strip_quote

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)


class CoverageConfig(object):
    def __init__(self, argv=None):

        self.attrs = OrderedDict()

        self.attrs['coverage'] = OrderedDict()
        self.attrs['coverage']['version'] = [ 0, 1, '0' ]
        self.attrs['coverage']['cwd'] = '.'
        self.attrs['coverage']['blocks'] = []

        self.attrs['file'] = OrderedDict()


#        self.attrs['build'] = OrderedDict()
#        self.attrs['build']['clean'] = ''
#        self.attrs['build']['cmdline'] = ''
#        self.attrs['build']['cwd'] = '.'
#
#        self.attrs['ini'] = 'include.ini'
#
#        self.attrs['rebuild'] = OrderedDict()
#
#        self.attrs['prerun'] = OrderedDict()
#        self.attrs['prerun']['clean'] = ''
#        self.attrs['prerun']['build'] = ''
#
#        self.attrs['macro'] = OrderedDict()
#
#        self.attrs['include'] = OrderedDict()
#
#        self.attrs['object'] = OrderedDict()
#
#        self.attrs['debug'] = OrderedDict()
#
        parser = optparse.OptionParser(version='Coverage version %d.%d.%s'%tuple(self.attrs['coverage']['version']))

        parser.add_option("--outdir", dest="outdir", action='store', type='string', default=None, help="path to create outputs")
        parser.add_option("--rebuild",  dest="rebuild", action='append', type='string', default=None, help="List of reusable files")
        parser.add_option("--prerun",  dest="prerun", action='append', type='string', default=None, help="List of prerun commands")
        parser.add_option("--debug",  dest="debug", action='append', type='string', default=None, help=optparse.SUPPRESS_HELP)
        parser.add_option("-c", "--cmd-clean", dest="cmd_clean", action='store', type='string', default=None, help="Linux command(s) to clean a target application build")
        parser.add_option("-b", "--cmd-build", dest="cmd_build", action='store', type='string', default=None, help="Linux command(s) to build a target application")
        parser.add_option("-r", "--cmd-run", dest="cmd_run", action='store', type='string', default=None, help="Linux command(s) to run a target application")

        opts, args = parser.parse_args(args=argv)
##        
##        if len(args)<1:
##            print 'ERROR: At least one argument is required.'
##            sys.exit(-1)
##

        if len(args)>0:
            self.attrs['file']['target'] = args[0]
#
#        if opts.strace:
#            self._save_opt(opts.strace, self.attrs['strace'])
#
        if opts.outdir:
            self.attrs['coverage']['cwd'] = '%s/coverage'%opts.outdir

#        if opts.build:
#            self._save_opt(opts.build, self.attrs['build'])
#
#        if opts.include_ini:
#            self.attrs['ini'] = opts.include_ini
#
#        if opts.macro:
#            self._save_opt(opts.macro, self.attrs['macro'], append=True)
#
#        if opts.include:
#            self._save_opt(opts.include, self.attrs['include'], append=True)
#
#        if opts.object:
#            self._save_opt(opts.object, self.attrs['object'], append=True)
#
#        if opts.rebuild:
#            self._save_opt(opts.rebuild, self.attrs['rebuild'], append=True)
#
#        if opts.prerun:
#            self._save_opt(opts.prerun, self.attrs['prerun'])
#
#        if opts.debug:
#            self._save_opt(opts.debug, self.attrs['debug'], append=True)

#    def _save_opt(self, opt, attr, append=False):
#        if isinstance(opt, str):
#            opt = [ opt ]
#
#        if isinstance(opt, ( list, tuple ) ):
#            for o in opt:
#                if isinstance(o, str):
#                    subopts = o.split(',')
#                    for subopt in subopts:
#                        if subopt:
#                            if subopt.find('=')>0:
#                                key, value = subopt.split('=', 1)
#                                if append:
#                                    if attr.has_key(key):
#                                        attr[key].append(strip_quote(value))
#                                    else:
#                                        attr[key] = [ strip_quote(value) ]
#                                else:
#                                    attr[key] = strip_quote(value)
#                            else:
#                                if append:
#                                    if attr.has_key(subopt):
#                                        attr[subopt].append(True)
#                                    else:
#                                        attr[subopt] = [ True ]
#                                else:
#                                    attr[subopt] = True
#                else:
#                    print 'UNKNOWN TYPE: %s'%o.__class__
#                    sys.exit(-1)
#        else:
#            print 'UNKNOWN TYPE: %s'%opt.__class__
#            sys.exit(-1)

    def __getattr__(self, name):
        print 'BBBB', name
        return self.attrs[name]
