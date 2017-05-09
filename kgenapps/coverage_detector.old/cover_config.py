# cover_config.py

from collections import OrderedDict

class CoverConfig(object):

    def __init__(self, homedir):
        self.home = homedir
        self.attrs = OrderedDict()
        self.options = []

        # kgen parameters
        self.attrs['cover'] = OrderedDict()
        self.attrs['cover']['version'] = [ 0, 1, '0' ]

        # mpi parameters
        self.attrs['mpi'] = OrderedDict()
        self.attrs['mpi']['enabled'] = False
        self.attrs['mpi']['comm'] = None
        self.attrs['mpi']['logical'] = None
        self.attrs['mpi']['status_size'] = None
        self.attrs['mpi']['source'] = None
        self.attrs['mpi']['any_source'] = None
        self.attrs['mpi']['ranks'] = [ 'all' ]
        self.attrs['mpi']['header'] = 'mpif.h'
        self.attrs['mpi']['use_stmts'] = []

        # make state parameters
        self.attrs['state_build'] = OrderedDict()
        self.attrs['state_build']['cmds'] = ''
        self.attrs['state_run'] = OrderedDict()
        self.attrs['state_run']['cmds'] = ''
        self.attrs['state_switch'] = OrderedDict()
        self.attrs['state_switch']['type'] = 'replace'
        self.attrs['state_switch']['cmds'] = ''

        # set plugin parameters
        self.attrs['plugin'] = OrderedDict()
        self.attrs['plugin']['priority'] = OrderedDict()

        self.attrs['plugin']['priority']['cov.core'] = '%s/plugins/core'%self.home

        self.options.append( (self.opt_mpi, ["--mpi"], {'dest':"mpi", 'action':'append', 'type':'string', 'default':None, 'help':"MPI information for data collection"}) )
        self.options.append( (self.opt_state_switch, ["--state-switch"], {'dest':"state_switch", 'action':'append', 'type':'string', 'help':"Specifying how to switch orignal sources with instrumented ones."}) )
        self.options.append( (self.opt_state_build, ["--state-build"], {'dest':"state_build", 'action':'append', 'type':'string', 'help':"Build information to generate makefile"}) )
        self.options.append( (self.opt_state_run, ["--state-run"], {'dest':"state_run", 'action':'append', 'type':'string', 'help':"Run information to generate makefile"}) )

        # parsing arguments
        self.usage = "usage: %prog [options] call-site"
        self.version='KGEN Coverage Detector version %d.%d.%s'%tuple(self.attrs['cover']['version'])

    # parsing MPI parameters
    def opt_mpi(self, opt):
        self.attrs['mpi']['enabled'] = True
        for line in opt:
            for mpi in line.split(','):
                key, value = mpi.split('=')
                if key=='comm':
                    self.attrs['mpi'][key] = value
                elif key=='use':
                    mod_name, identifier = value.split(':')
                    self.attrs['mpi']['use_stmts'].append((mod_name, [identifier]))
                elif key=='ranks':
                    ranks = value.split(':')
                    if 'all' not in ranks:
                        self.attrs['mpi'][key] = ranks
                elif key=='header':
                    self.attrs['mpi'][key] = value
                else:
                    raise UserException('Unknown MPI option: %s' % mpi)

    def opt_state_build(self, opt):
        for line in opt:
            for build in line.split(','):
                key, value = build.split('=')
                if key in [ 'cmds' ] :
                    self.attrs['state_build'][key] = value
                else:
                    raise UserException('Unknown state-build option: %s' % build)

    def opt_state_run(self, opt):
        for line in opt:
            for run in line.split(','):
                key, value = run.split('=')
                if key in [ 'cmds' ] :
                    self.attrs['state_run'][key] = value
                else:
                    raise UserException('Unknown state-run option: %s' % run)

    def opt_state_switch(self, opt):
        for line in opt:
            for run in line.split(','):
                key, value = run.split('=')
                if key in [ 'cmds', 'type' ] :
                    self.attrs['state_switch'][key] = value
                else:
                    raise UserException('Unknown state-switch option: %s' % run)
