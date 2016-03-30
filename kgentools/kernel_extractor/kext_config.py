# kext_config.py


from ordereddict import OrderedDict

class KExtConfig(object):
    def __init__(self, homedir):
        self.home = homedir
        self.attrs = OrderedDict()
        self.options = []

        # kgen parameters
        self.attrs['kgen'] = OrderedDict()
        self.attrs['kgen']['version'] = [ 0, 6, '3' ]

        # mpi parameters
        self.attrs['mpi'] = OrderedDict()
        self.attrs['mpi']['enabled'] = False
        self.attrs['mpi']['ranks'] = [ '0' ]
        self.attrs['mpi']['size'] = len(self.attrs['mpi']['ranks'])
        self.attrs['mpi']['comm'] = None
        self.attrs['mpi']['header'] = 'mpif.h'
        self.attrs['mpi']['use_stmts'] = []

        # invocation parameters
        self.attrs['invocation'] = OrderedDict()
        self.attrs['invocation']['numbers'] = [ '1' ]
        self.attrs['invocation']['size'] = len(self.attrs['invocation']['numbers'])


        # timing parameters
        self.attrs['timing'] = OrderedDict()
        self.attrs['timing']['repeat'] = '10'

        # verification parameters
        self.attrs['verify'] = OrderedDict()
        self.attrs['verify']['tolerance'] = '1.E-14'
        self.attrs['verify']['verboselevel'] = '1'

        # make kernel parameters
        self.attrs['kernel_compile'] = OrderedDict()
        self.attrs['kernel_compile']['FC'] = 'ifort'
        self.attrs['kernel_compile']['FC_FLAGS'] = ''

        # make state parameters
        self.attrs['state_build'] = OrderedDict()
        self.attrs['state_build']['cmds'] = ''
        self.attrs['state_run'] = OrderedDict()
        self.attrs['state_run']['cmds'] = ''
        self.attrs['state_switch'] = OrderedDict()
        self.attrs['state_switch']['type'] = 'replace'
        self.attrs['state_switch']['cmds'] = ''

        # kernel correctness check parameters
        self.attrs['check'] = OrderedDict()
        #self.attrs['check']['pert_invar'] = ['*']
        self.attrs['check']['pert_invar'] = []
        self.attrs['check']['pert_lim'] = '1.0E-15'


        # set plugin parameters
        self.attrs['plugin'] = OrderedDict()
        self.attrs['plugin']['priority'] = OrderedDict()

        self.attrs['plugin']['priority']['ext.gencore'] = '%s/plugins/gencore'%self.home
        self.attrs['plugin']['priority']['ext.verification'] = '%s/plugins/verification'%self.home
        self.attrs['plugin']['priority']['ext.simple_timing'] = '%s/plugins/simple_timing'%self.home
        self.attrs['plugin']['priority']['ext.perturb'] = '%s/plugins/perturb'%self.home

        self.options.append( (self.opt_invocation, ["--invocation"], {'dest':"invocation", 'action':'store', 'type':'string', 'default':None, 'help':"Nth invocation of kernel for data collection"}) )
        self.options.append( (self.opt_mpi, ["--mpi"], {'dest':"mpi", 'action':'append', 'type':'string', 'default':None, 'help':"MPI information for data collection"}) )
        self.options.append( (self.opt_timing, ["--timing"], {'dest':"timing", 'action':'store', 'type':'string', 'default':None, 'help':"Timing measurement information"}) )
        self.options.append( (self.opt_kernel_compile, ["--kernel-compile"], {'dest':"kernel_compile", 'action':'append', 'type':'string', 'help':"Compile information to generate kernel makefile"}) )
        self.options.append( (self.opt_state_switch, ["--state-switch"], {'dest':"state_switch", 'action':'append', 'type':'string', 'help':"Specifying how to switch orignal sources with instrumented ones."}) )
        self.options.append( (self.opt_state_build, ["--state-build"], {'dest':"state_build", 'action':'append', 'type':'string', 'help':"Build information to generate makefile"}) )
        self.options.append( (self.opt_state_run, ["--state-run"], {'dest':"state_run", 'action':'append', 'type':'string', 'help':"Run information to generate makefile"}) )
        self.options.append( (self.opt_check, ["--check"], {'dest':"check", 'action':'append', 'type':'string', 'help':"Kernel correctness check information"}) )
        self.options.append( (self.opt_verbose_level, ["--verbose"], {'dest':"verbose_level", 'action':'store', 'type':'int', 'help':'Set the verbose level for verification output'}) )

        # parsing arguments
        self.usage = "usage: %prog [options] call-site"
        self.version='KGEN version %d.%d.%s'%tuple(self.attrs['kgen']['version'])

    # parsing invocation parameters
    def opt_invocation(self, opt):
        self.attrs['invocation']['numbers'] = []
        if opt.find(',')>0:
            print 'ERROR: Please use colon to separate invocation numbers of invocation flag instead of comma.'
            sys.exit(-1)

        for ord in opt.split(':'):
            if ord.isdigit():
                self.attrs['invocation']['numbers'].append(ord)
        self.attrs['invocation']['numbers'].sort()
        self.attrs['invocation']['size'] = len(self.attrs['invocation']['numbers'])

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
                    self.attrs['mpi'][key] = value.split(':')
                    self.attrs['mpi']['size'] = len(self.attrs['mpi'][key])
                elif key=='header':
                    self.attrs['mpi'][key] = value
                else:
                    raise UserException('Unknown MPI option: %s' % mpi)

    # parsing kernel makefile parameters
    def opt_kernel_compile(self, opt):
        for line in opt:
            for comp in line.split(','):
                key, value = comp.split('=')
                if key in [ 'FC', 'FC_FLAGS' ] :
                    self.attrs['kernel_compile'][key] = value
                else:
                    raise UserException('Unknown kernel compile option: %s' % comp)

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

    def opt_timing(self, opt):
        for time in opt.split(','):
            key, value = time.split('=')
            if key in [ 'repeat' ] :
                try:
                    self.attrs['timing'][key] = value
                except:
                    raise UserException('repeat sub-flag should be integer value: %s'%value)
            else:
                raise UserException('Unknown timing option: %s' % time)

    # kernel correctness checks 
    def opt_check(self, opt):
        for line in opt:
            for checkparams in line.split(','):
                key, value = checkparams.split('=')
                key = key.lower()
                value = value.lower()
                if key=='pert_invar':
                    self.attrs['check'][key] = value.split(':')
                elif key=='pert_lim':
                    self.attrs['check'][key] = value
                else:
                    print 'WARNING: %s is not supported check parameter'%key

    # parsing logging options
    def opt_verbose_level(self, opt):
        self.attrs['verify']['verboselevel'] = str(opt)

