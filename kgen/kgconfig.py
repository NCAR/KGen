'''config.py
KGen configuration
'''

import optparse

class Config(object):

    def __init__(self, cwd):
        self.cwd = cwd
        self._attrs = {}

        ##############################################
        # KGen configurations
        ##############################################

        # command arguments
        self._attrs['cmdarg'] = cmdarg = {}

        ######## mandatory arguments ########
        cmdarg['cmd_clean'] = None
        cmdarg['cmd_build'] = None
        cmdarg['cmd_run']   = None

        ######## path arguments ########
        cmdarg['outdir']   = self.cwd

        ######## Fortran parser arguments ########
        cmdarg['callsite']   = None
        cmdarg['includefile']   = None
        cmdarg['includepath']   = []
        cmdarg['macro']   = []

        ######## code generation arguments ########
        cmdarg['prerun']   = {}


        ######## misc. arguments ########
        cmdarg['rebuild']   = []

        ##############################################
        # KGen command argument parser
        ##############################################

        # create parser
        parser = optparse.OptionParser()

        ######## mandatory arguments ########
        parser.add_option("--cmd-clean", dest="cmd_clean", action='store', type='string', default=None, help="clean command for application")
        parser.add_option("--cmd-build", dest="cmd_build", action='store', type='string', default=None, help="build command for application")
        parser.add_option("--cmd-run",   dest="cmd_run",   action='store', type='string', default=None, help="run command for application")

        ######## path arguments ########
        parser.add_option("--outdir",   dest="outdir",  action='store', type='string', default=None, help="output directory")

        ######## code generation arguments ########
        parser.add_option("--prerun",   dest="prerun",  action='append', type='string', default=None, help="shell commands to run before several KGen stages")

        ######## Fortran parser arguments ########
        parser.add_option("-I", dest="incpath",  action='append', type='string', default=None, help="Include paths")
        parser.add_option("-D", dest="macro",  action='append', type='string', default=None, help="Macro definitions")

        ######## misc. arguments ########
        parser.add_option("--rebuild",  dest="rebuild", action='store', type='string', default=None, help="KGen stages ignoring cache")

        # run parser
        opts, args = parser.parse_args()

        ##############################################
        # KGen command argument processing
        ##############################################

        ######## mandatory arguments ########

        if len(args)==1:
            self.cmdarg['callsite'] = args[0]
        else:
            kgutils.exit('ERROR: Wrong number of callsite information.')

        if opts.cmd_clean:
            self.cmdarg['cmd_clean'] = opts.cmd_clean
        else:
            kgutils.exit('ERROR: "--cmd-clean" is not provided.')

        if opts.cmd_build:
            self.cmdarg['cmd_build'] = opts.cmd_build
        else:
            kgutils.exit('ERROR: "--cmd-build" is not provided.')

        if opts.cmd_run:
            self.cmdarg['cmd_run'] = opts.cmd_run
        else:
            kgutils.exit('ERROR: "--cmd-run" is not provided.')

        ######## path arguments ########
        if opts.outdir:
            self.cmdarg['outdir'] = opts.outdir

        ######## Fortran parser arguments ########
        if opts.incpath:
            self.cmdarg['includepath'] = self.parse_subargs(opts.incpath) 

        if opts.macro:
            self.cmdarg['macro'] = self.parse_subargs(opts.macro) 

        ######## code generation arguments ########
        if opts.prerun:
            self.cmdarg['prerun'] = self.parse_subargs(opts.prerun) 

        ######## misc. arguments ########
        if opts.rebuild:
            self.cmdarg['rebuild'] = self.parse_subargs(opts.rebuild) 

    def parse_subargs(self, arg):

        arglist = arg
        if isinstance(arg, str):
            arglilst = [ arg ]

        retval = []

        for argitem in arglist:
            comma = argitem.split(',')
            for comma_item in comma:
                equal = comma_item.split('=', 1)
                if len(equal) == 1:
                    colon = equal[0].split(':')
                    if len(colon) == 1:
                        retval.append(colon[0])
                    else:
                        retval.append(colon)
                elif len(equal) == 2:
                    colon = equal[1].split(':')
                    if len(colon) == 1:
                        retval.append((equal[0], equal[1]))
                    else:
                        retval.append((equal[0], colon))
                else:
                    kgutils.exit('Wrong format of sub arguments: %s'%argitem)

        if len(retval) == 1:
            return retval[0]
        else:
            return retval

    def __getattr__(self, name):
        return self._attrs[name]
