
import os
import re
import sys
import glob
import shutil
import collections
import copy
import optparse
from kgutils import UserException, run_shcmd, INTERNAL_NAMELEVEL_SEPERATOR, traverse, match_namepath, dequote
try:
    import configparser
except:
    import ConfigParser as configparser

SRCROOT = os.path.dirname(os.path.realpath(__file__))
KGEN_MACHINE = '%s/../machines'%SRCROOT
KGEN_EXT = '%s/extractor'%SRCROOT
KGEN_COVER = '%s/coverage'%SRCROOT
KGEN_ETIME = '%s/elapsedtime'%SRCROOT
KGEN_PAPI = '%s/papicounter'%SRCROOT

#############################################################################
## CONFIG
#############################################################################

class KgenConfigParser(configparser.RawConfigParser):
    def __init__(self, *args, **kwargs):
        configparser.RawConfigParser.__init__(self, *args, **kwargs)
        self.optionxform = str

    def _optname_colon_to_dot(self, line):
        newline = line.strip()

        if len(newline)>0:
            if newline[0]==';': # comment
                return line
            elif newline[0]=='[' and newline[-1]==']': # filepath
                return line.replace(':', INTERNAL_NAMELEVEL_SEPERATOR)
            else: # else
                pos = line.find('=')
                if pos>0:
                    return line[:pos].replace(':', INTERNAL_NAMELEVEL_SEPERATOR) + line[pos:]
                else:
                    raise UserException('KGEN requires an equal symbol at each option line')
        else:
            return line

    def read(self, filenames):
        from StringIO import StringIO

        if isinstance(filenames, basestring):
            filenames = [filenames]
        for filename in filenames:
            try:
                fp = open(filename)
            except IOError:
                continue

            lines = []
            for line in fp.readlines():
                lines.append(self._optname_colon_to_dot(line))
            fp.close()

            buf = StringIO(''.join(lines))
            self._read(buf, filename)

def get_MPI_PARAM(node, bag, depth):
    import parser
    from parser.Fortran2003 import Specification_Part, Type_Declaration_Stmt, Entity_Decl, Parameter_Stmt, Named_Constant_Def, \
        NoMatchError, Module_Stmt, Program_Stmt, Named_Constant_Def_List

    if isinstance(node, Type_Declaration_Stmt):
        if isinstance(node.items[2], Entity_Decl) and node.items[2].items[0].string.upper()==bag['key']:
            pass
    elif isinstance(node, Parameter_Stmt):
        if isinstance(node.items[1], Named_Constant_Def) and node.items[1].items[0].string.upper()==bag['key']:
            bag[bag['key']].append(str(node.items[1].items[1]).replace(' ', ''))
        elif isinstance(node.items[1], Named_Constant_Def_List):
            for item in node.items[1].items:
                if isinstance(item, Named_Constant_Def) and item.items[0].string.upper()==bag['key']:
                    bag[bag['key']].append(str(item.items[1]).replace(' ', ''))


def handle_include(mpifdir, lines):

    insert_lines = []
    for i, line in enumerate(lines):
        match = re.match(r'^\s*include\s*("[^"]+"|\'[^\']+\')\s*\Z', line, re.I)
        if match:
            include_dirs = [mpifdir]+Config.include['path']
            filename = match.group(1)[1:-1].strip()
            path = filename
            for incl_dir in include_dirs:
                path = os.path.join(incl_dir, filename)
                if os.path.exists(path):
                    break
            if os.path.isfile(path):
                with open(path, 'r') as f:
                    included_lines = f.read()
                    insert_lines.extend(handle_include(mpifdir, included_lines.split('\n')))
            else:
                raise UserException('Can not find %s in include paths.'%path)
        else:
            insert_lines.append(line)

    return insert_lines

def process_exclude_option(exclude_option, excattrs):

    # collect exclude configuration information
    Exc = KgenConfigParser(allow_no_value=True)
    #Exc.optionxform = str
    Exc.read(exclude_option)
    if len(Exc.sections())>0:
        for section in Exc.sections():
            lsection = section.lower().strip()
            if lsection=='common':
                print 'ERROR: a section of "common" is discarded in INI file for exclusion. Please use "namepath" section instead'
                sys.exit(-1)

            excattrs[lsection] = collections.OrderedDict()
            for option in Exc.options(section):
                loption = option.lower().strip()
                excattrs[lsection][loption] = Exc.get(section, option).strip().split('=')
    else:
        UserException('Can not find exclude file: %s'%exclude_option)

def singleton(cls):
    """ singleton generator """

    instances = collections.OrderedDict()
    def get_instance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return get_instance()

@singleton
class Config(object):
    """ KGEN configuration parameter holder """

    def __init__(self):

        # command line option parser
        self.parser = optparse.OptionParser()

        # setup config parameters
        self._attrs = collections.OrderedDict()


        ###############################################################
        # Common
        ###############################################################
    
        # KGEN version
        self._attrs['kgen'] = collections.OrderedDict()
        self._attrs['kgen']['version'] = [ 0, 7, '3' ]

        # KGEN operation mode
        self._attrs['check_mode'] = False

        # machine parameters
        self._attrs['machine'] = collections.OrderedDict()
        self._attrs['machine']['inc'] = None
        self._attrs['machine']['general'] = collections.OrderedDict()
        self._attrs['machine']['general']['name'] = 'Generic Machine'
        self._attrs['machine']['general']['id'] = 'generic'
        self._attrs['machine']['compiler'] = collections.OrderedDict()
        self._attrs['machine']['compiler']['gnu'] = ''
        self._attrs['machine']['compiler']['intel'] = ''
        self._attrs['machine']['compiler']['pgi'] = ''
        self._attrs['machine']['variable'] = collections.OrderedDict()
        self._attrs['machine']['variable']['work_directory'] = os.path.expandvars('${HOME}/kgen_workspace')

        # Fortran parameters
        self._attrs['fort'] = collections.OrderedDict()
        self._attrs['fort']['maxlinelen'] = 132

        # logging parameters
        self._attrs['logging'] = collections.OrderedDict()
        self._attrs['logging']['select'] = collections.OrderedDict()

        # external tool parameters
        self._attrs['bin'] = collections.OrderedDict()
        self._attrs['bin']['pp'] = 'cpp'
        self._attrs['bin']['cpp_flags'] = '-w -traditional -P'
        #self._attrs['bin']['fpp_flags'] = '-w'

        # search parameters
        self._attrs['search'] = collections.OrderedDict()
        self._attrs['search']['skip_intrinsic'] = True
        self._attrs['search']['except'] = []
        self._attrs['search']['promote_exception'] = False

        # path parameters
        self._attrs['cwd'] = os.getcwd()
        self._attrs['path'] = collections.OrderedDict()
        self._attrs['path']['outdir'] = '.'
        self._attrs['path']['state'] = 'state'
        self._attrs['path']['kernel'] = 'kernel'
        self._attrs['path']['model'] = 'model'
        self._attrs['path']['coverage'] = 'coverage'
        self._attrs['path']['etime'] = 'elapsedtime'
        self._attrs['path']['papi'] = 'papi'

        # source file parameters
        self._attrs['source'] = collections.OrderedDict()
        self._attrs['source']['isfree'] = None
        self._attrs['source']['isstrict'] = None
        self._attrs['source']['alias'] = collections.OrderedDict()
        self._attrs['source']['file'] = collections.OrderedDict()
        self._attrs['source']['state'] = []

        # include parameters
        self._attrs['includefile'] = 'include.ini'
        self._attrs['include'] = collections.OrderedDict()
        self._attrs['include']['macro'] = collections.OrderedDict()
        self._attrs['include']['path'] = []
        self._attrs['include']['type'] = collections.OrderedDict()
        self._attrs['include']['compiler'] = collections.OrderedDict()
        self._attrs['include']['import'] = collections.OrderedDict()
        self._attrs['include']['file'] = collections.OrderedDict()
        self._attrs['include']['opt'] = None

        # exclude parameters
        self._attrs['exclude'] = collections.OrderedDict()

        # program units
        self._attrs['program_units'] = {}

        # debugging parameters
        self._attrs['debug'] = collections.OrderedDict()
        self._attrs['debug']['printvar'] = []

        # plugin parameters
        self._attrs['plugin'] = collections.OrderedDict()
        self._attrs['plugin']['priority'] = collections.OrderedDict()

        self._attrs['plugindb'] = collections.OrderedDict()

        ###############################################################
        # compiler flag information
        ###############################################################

        # strace parameters
        self._attrs['stracefile'] = 'strace.log'
        self._attrs['strace'] = collections.OrderedDict()


        ###############################################################
        # Kernel Extraction
        ###############################################################
        
        # openmp parameters
        self._attrs['openmp'] = collections.OrderedDict()
        self._attrs['openmp']['enabled'] = False
        self._attrs['openmp']['critical'] = True
        self._attrs['openmp']['maxnum_threads'] = 102

        # mpi parameters
        self._attrs['mpi'] = collections.OrderedDict()
        self._attrs['mpi']['enabled'] = False
        self._attrs['mpi']['comm'] = None
        self._attrs['mpi']['logical'] = None
        self._attrs['mpi']['status_size'] = None
        self._attrs['mpi']['source'] = None
        self._attrs['mpi']['any_source'] = None
        self._attrs['mpi']['header'] = 'mpif.h'
        self._attrs['mpi']['use_stmts'] = []

        # invocation parameters
        self._attrs['invocation'] = collections.OrderedDict()
        self._attrs['invocation']['triples'] = []

        # data parameters
        self._attrs['data'] = collections.OrderedDict()
        self._attrs['data']['condition'] = []
        self._attrs['data']['maxnuminvokes'] = None

        # add mpi frame code in kernel driver
        self._attrs['add_mpi_frame'] = collections.OrderedDict()
        self._attrs['add_mpi_frame']['enabled'] = False
        self._attrs['add_mpi_frame']['np'] = '2'
        self._attrs['add_mpi_frame']['mpifc'] = 'mpif90'
        self._attrs['add_mpi_frame']['mpirun'] = 'mpirun'

        # add mpi frame code in kernel driver
        self._attrs['add_cache_pollution'] = collections.OrderedDict()
        self._attrs['add_cache_pollution']['enabled'] = False
        self._attrs['add_cache_pollution']['size'] = 0

        # timing parameters
        #self._attrs['timing'] = collections.OrderedDict()
        #self._attrs['timing']['repeat'] = '2'

        # verification parameters
        self._attrs['verify'] = collections.OrderedDict()
        self._attrs['verify']['tolerance'] = '1.D-14'
        self._attrs['verify']['verboselevel'] = '1'

        # make kernel parameters
        self._attrs['kernel_option'] = collections.OrderedDict()
        self._attrs['kernel_option']['FC'] =  ''
        self._attrs['kernel_option']['FC_FLAGS'] = ''
        self._attrs['kernel_option']['compiler'] = collections.OrderedDict()
        self._attrs['kernel_option']['compiler']['add'] = []
        self._attrs['kernel_option']['compiler']['remove'] = []
        self._attrs['kernel_option']['linker'] = collections.OrderedDict()
        self._attrs['kernel_option']['linker']['add'] = []

        # make prerun parameters
        self._attrs['prerun'] = collections.OrderedDict()
        self._attrs['prerun']['kernel_build'] = ''
        self._attrs['prerun']['kernel_run'] = ''
        self._attrs['prerun']['build'] = ''
        self._attrs['prerun']['run'] = ''

        # make rebuild parameters
        self._attrs['rebuild'] = collections.OrderedDict()

        # make cmd parameters
        self._attrs['cmd_clean'] = collections.OrderedDict()
        self._attrs['cmd_clean']['cmds'] = ''
        self._attrs['cmd_build'] = collections.OrderedDict()
        self._attrs['cmd_build']['cmds'] = ''
        self._attrs['cmd_run'] = collections.OrderedDict()
        self._attrs['cmd_run']['cmds'] = ''
        self._attrs['state_switch'] = collections.OrderedDict()
        self._attrs['state_switch']['type'] = 'replace'
        self._attrs['state_switch']['directory'] = ''
        self._attrs['state_switch']['clean'] = ''

        # kernel correctness check parameters
        self._attrs['check'] = collections.OrderedDict()
        #self._attrs['check']['pert_invar'] = ['*']
        self._attrs['check']['pert_invar'] = []
        self._attrs['check']['pert_lim'] = '1.0E-15'

        # set plugin parameters
        self._attrs['plugin']['priority']['ext.gencore'] = '%s/plugins/gencore'%KGEN_EXT
        self._attrs['plugin']['priority']['ext.verification'] = '%s/plugins/verification'%KGEN_EXT
        self._attrs['plugin']['priority']['ext.simple_timing'] = '%s/plugins/simple_timing'%KGEN_EXT
        self._attrs['plugin']['priority']['ext.perturb'] = '%s/plugins/perturb'%KGEN_EXT
        #self._attrs['plugin']['priority']['ext.coverage'] = '%s/plugins/coverage'%KGEN_EXT

        ###############################################################
        # model information
        ###############################################################

        # model parameters
        self._attrs['modelfile'] = 'model.ini'
        self._attrs['model'] = collections.OrderedDict()
        self._attrs['model']['reuse_rawdata'] = True
        self._attrs['model']['types'] = collections.OrderedDict()
        self._attrs['model']['types']['code'] = collections.OrderedDict()
        self._attrs['model']['types']['code']['id'] = '0'
        self._attrs['model']['types']['code']['name'] = 'code'
        self._attrs['model']['types']['code']['percentage'] = 99.9
        self._attrs['model']['types']['code']['filter'] = None
        self._attrs['model']['types']['code']['ndata'] = 20
        self._attrs['model']['types']['code']['enabled'] = False
        self._attrs['model']['types']['etime'] = collections.OrderedDict()
        self._attrs['model']['types']['etime']['id'] = '1'
        self._attrs['model']['types']['etime']['name'] = 'etime'
        self._attrs['model']['types']['etime']['nbins'] = 5
        self._attrs['model']['types']['etime']['ndata'] = 20
        self._attrs['model']['types']['etime']['minval'] = None
        self._attrs['model']['types']['etime']['maxval'] = None
        self._attrs['model']['types']['etime']['timer'] = None
        self._attrs['model']['types']['etime']['enabled'] = True
        self._attrs['model']['types']['papi'] = collections.OrderedDict()
        self._attrs['model']['types']['papi']['id'] = '2'
        self._attrs['model']['types']['papi']['name'] = 'papi'
        self._attrs['model']['types']['papi']['nbins'] = 5
        self._attrs['model']['types']['papi']['ndata'] = 20
        self._attrs['model']['types']['papi']['minval'] = None
        self._attrs['model']['types']['papi']['maxval'] = None
        self._attrs['model']['types']['papi']['header'] = None
        self._attrs['model']['types']['papi']['event'] = 'PAPI_TOT_INS'
        self._attrs['model']['types']['papi']['static'] = None
        self._attrs['model']['types']['papi']['dynamic'] = None
        self._attrs['model']['types']['papi']['enabled'] = False

        # set plugin parameters
        self._attrs['plugin']['priority']['cover.gencore'] = '%s/plugins/gencore'%KGEN_COVER
        self._attrs['plugin']['priority']['etime.gencore'] = '%s/plugins/gencore'%KGEN_ETIME
        self._attrs['plugin']['priority']['papi.gencore'] = '%s/plugins/gencore'%KGEN_PAPI

        ###############################################################
        # state information
        ###############################################################

        self._attrs['modules'] = collections.OrderedDict()
        self._attrs['srcfiles'] = collections.OrderedDict()
        self._attrs['kernel'] = collections.OrderedDict()
        self._attrs['kernel']['name'] = None
        self._attrs['callsite'] = collections.OrderedDict()
        self._attrs['callsite']['stmts'] = []
        self._attrs['callsite']['filepath'] = ''
        self._attrs['callsite']['span'] = (-1, -1)
        self._attrs['callsite']['namepath'] = ''
#        self._attrs['callsite']['lineafter'] = -1
        self._attrs['parentblock'] = collections.OrderedDict()
        self._attrs['parentblock']['stmt'] = None
        self._attrs['topblock'] = collections.OrderedDict()
        self._attrs['topblock']['stmt'] = None
        self._attrs['topblock']['filepath'] = ''
        self._attrs['used_srcfiles'] = collections.OrderedDict()
        self._attrs['kernel_driver'] = collections.OrderedDict()
        self._attrs['kernel_driver']['name'] = 'kernel_driver'
        self._attrs['kernel_driver']['callsite_args'] = ['kgen_unit', 'kgen_measure', 'kgen_isverified', 'kgen_filepath']

        ###############################################################
        # Add common options
        ###############################################################
        self.parser.add_option("-s", "--syntax-check", dest="syntax_check", action='store_true', default=False, help="KGEN Syntax Check Mode")
        self.parser.add_option("-i", "--include-ini", dest="include_ini", action='store', type='string', default=None, help="information used for analysis")
        self.parser.add_option("-e", "--exclude-ini", dest="exclude_ini", action='store', type='string', default=None, help="information excluded for analysis")
        self.parser.add_option("-I", dest="include", action='append', type='string', default=None, help="include path information used for analysis")
        self.parser.add_option("-D", dest="macro", action='append', type='string', default=None, help="macro information used for analysis")
        self.parser.add_option("--outdir", dest="outdir", action='store', type='string', default=None, help="path to create outputs")
        self.parser.add_option("--source", dest="source", action='append', type='string', default=None, help="Setting source file related properties")
        self.parser.add_option("--skip-intrinsic", dest="skip_intrinsic", action='store_true', default=False, help=optparse.SUPPRESS_HELP)
        self.parser.add_option("--noskip-intrinsic", dest="noskip_intrinsic", action='store_true', default=False, help=optparse.SUPPRESS_HELP)
        self.parser.add_option("--intrinsic", dest="intrinsic", action='append', type='string', default=None, help="Specifying resolution for intrinsic procedures during searching")
        self.parser.add_option("--machinefile", dest="machinefile", action='store', type='string', default=None, help="Specifying machinefile")
        self.parser.add_option("--debug", dest="debug", action='append', type='string', help=optparse.SUPPRESS_HELP)
        self.parser.add_option("--logging", dest="logging", action='append', type='string', help=optparse.SUPPRESS_HELP)

        ###############################################################
        # Add extraction options
        ###############################################################

        self.parser.add_option("--invocation", dest="invocation", action='append', type='string', default=None, help="(process, thread, invocation) pairs of kernel for data collection")
        self.parser.add_option("--data", dest="data", action='append', type='string', default=None, help="Control state data generation")
        self.parser.add_option("--openmp", dest="openmp", action='append', type='string', default=None, help="Specifying OpenMP options")
        self.parser.add_option("--mpi", dest="mpi", action='append', type='string', default=None, help="MPI information for data collection")
        self.parser.add_option("--timing", dest="timing", action='store', type='string', default=None, help="Timing measurement information")
        self.parser.add_option("--prerun", dest="prerun", action='append', type='string', default=None, help="prerun commands")
        self.parser.add_option("--rebuild", dest="rebuild", action='append', type='string', default=None, help="rebuild controls")
        self.parser.add_option("--state-switch", dest="state_switch", action='append', type='string', default=None, help="Specifying how to switch orignal sources with instrumented ones.")
        self.parser.add_option("--cmd-clean", dest="cmd_clean", action='store', type='string', default=None, help="Clean information to generate makefile")
        self.parser.add_option("--cmd-build", dest="cmd_build", action='store', type='string', default=None, help="Build information to generate makefile")
        self.parser.add_option("--cmd-run", dest="cmd_run", action='store', type='string', default=None, help="Run information to generate makefile")
        self.parser.add_option("--kernel-option", dest="kernel_option", action='append', type='string', default=None, help="Specifying kernel compiler and linker options")
        self.parser.add_option("--check", dest="check", action='append', type='string', default=None, help="Kernel correctness check information")
        self.parser.add_option("--verbose", dest="verbose_level", action='store', type='int', default=None, help="Set the verbose level for verification output")
        self.parser.add_option("--add-mpi-frame", dest="add_mpi_frame", action='store', type='string', default=None, help="Add MPI frame codes in kernel_driver")
        self.parser.add_option("--add-cache-pollution", dest="add_cache_pollution", action='store', type='string', help="Add cache pollution frame codes in kernel_driver")

        self.parser.add_option("--noreuse-rawdata", dest="reuse_rawdata", action='store_false', default=True, help="Control raw data generation for modeling.")
        self.parser.add_option("--repr-etime", dest="repr_etime", action='append', type='string', default=None, help="Specifying elapsedtime representativeness feature flags")
        self.parser.add_option("--repr-papi", dest="repr_papi", action='append', type='string', default=None, help="Specifying papi counter representativeness feature flags")
        self.parser.add_option("--repr-code", dest="repr_code", action='append', type='string', default=None, help="Specifying code coverage representativeness feature flags")

        #self.parser.set_usage(cfg.usage)

    def parse(self, cfgargs=None):

        if cfgargs is None:
            cfgargs = sys.argv[1:]

        if "--mpi" in cfgargs and "--add-mpi-frame" not in cfgargs:
            cfgargs.extend(["--add-mpi-frame", "np=2"])

        opts, args = self.parser.parse_args(cfgargs)

        if len(args)<1:
            print 'ERROR: No call-site information is provided in command line.'
            sys.exit(-1)

        if opts.syntax_check:
            self._process_default_flags(opts)
            self._attrs['check_mode'] = args
            return

        # old options
        if opts.skip_intrinsic:
            print "skip-intrinsic flag is discarded. Please use --intrinsic skip instead"
            sys.exit(-1)
        if opts.noskip_intrinsic:
            print "noskip-intrinsic flag is discarded. Please use --intrinsic noskip instead"
            sys.exit(-1)

        callsite = args[0].split(':', 1)
        if not os.path.isfile(callsite[0]):
            print 'ERROR: callsite file, "%s" can not be found.' % callsite[0]
            sys.exit(-1)

        # set callsite filepath
        self.callsite['filepath'] = os.path.realpath(callsite[0])

        # set namepath if exists in command line argument
        if len(callsite)==2:
            self.callsite['namepath'] = callsite[1].lower()
        elif len(callsite)>2:
            print 'ERROR: Unrecognized call-site information(Syntax -> filepath[:subprogramname]): %s'%str(callsite)
            sys.exit(-1)

        # process default flags
        self._process_default_flags(opts)
                
        # process extraction flags
        self._process_extract_flags(opts)
                
        # process representativeness flags
        self._process_repr_flags(opts)

    def __getattr__(self, name):
        return self._attrs[name]

#    def __getstate__(self):
#        return 'TEST'
#
#    def __setstate__(self, state):
#        print 'STATE: ', state

    def collect_mpi_params(self):
        #from parser.api import parse, walk
        import parser

        if self._attrs['mpi']['enabled']:
            # get path of mpif.h
            mpifpath = ''
            if os.path.isabs(self._attrs['mpi']['header']):
                if os.path.exists(self._attrs['mpi']['header']):
                    mpifpath = self._attrs['mpi']['header']
                else:
                    raise UserException('Can not find %s'%self._attrs['.mpi']['header'])
            else:
                for p in self._attrs['include']['path']:
                    fp = os.path.join(p, self._attrs['mpi']['header'])
                    if os.path.exists(fp):
                        mpifpath = fp
                        break
                if not mpifpath:
                    for incpath, incdict in self._attrs['include']['file'].items():
                        for p in incdict['path']:
                            fp = os.path.join(p, self._attrs['mpi']['header'])
                            if os.path.exists(fp):
                                mpifpath = fp
                                break
                        if mpifpath: break

            # collect required information
            if mpifpath:
                try:
                    with open(mpifpath, 'r') as f:
                        filelines = f.read().split('\n')
                        lines = '\n'.join(handle_include(os.path.dirname(mpifpath), filelines))
                        #reader = FortranStringReader(lines)
                    tree = parser.api.parse(lines, ignore_comments=True, analyze=False, isfree=True, isstrict=False, include_dirs=None, source_only=None )
                    for stmt, depth in parser.api.walk(tree, -1):
                        stmt.parse_f2003()

                    #import pdb; pdb.set_trace()
                    #spec = Specification_Part(reader)
                    bag = {}
                    config_name_mapping = [
                        ('comm', 'MPI_COMM_WORLD'),
                        ('logical', 'MPI_LOGICAL'),
                        ('status_size', 'MPI_STATUS_SIZE'),
                        ('any_source', 'MPI_ANY_SOURCE'),
                        ('source', 'MPI_SOURCE'),
                        ]
                    for config_key, name in config_name_mapping:
                        if not self._attrs['mpi'].has_key(config_key) or self._attrs['mpi'][config_key] is None:
                            for stmt, depth in parser.api.walk(tree, -1):
                                bag['key'] = name
                                bag[name] = []
                                if hasattr(stmt, 'f2003'):
                                    traverse(stmt.f2003, get_MPI_PARAM, bag, subnode='content')
                                    if len(bag[name]) > 0:
                                        self._attrs['mpi'][config_key] = bag[name][-1]
                                        break

                    for config_key, name in config_name_mapping:
                        if not self._attrs['mpi'].has_key(config_key) or self._attrs['mpi'][config_key] is None:
                            raise UserException('Can not find {name} in mpif.h'.format(name=name))

                except UserException:
                    raise  # Reraise this exception rather than catching it below
                except Exception as e:
                    raise UserException('Error occurred during reading %s.'%mpifpath)
            else:
                raise UserException('Can not find mpif.h. Please provide a path to the file')

    def _process_default_flags(self, opts):

        # check if exists fpp or cpp
        output = ''
        try:
            out, err, retcode = run_shcmd('which cpp', show_error_msg=False)
            output = out.strip()
        except Exception as e: pass
        if output.endswith('cpp'):
            self.bin['pp'] = output
        else:
            print 'ERROR: cpp is not found.'
            sys.exit(-1)

#            output = ''
#            try:
#                out, err, retcode = run_shcmd('which fpp', show_error_msg=False)
#                output = out.strip()
#            except Exception as e: pass
#            if output.endswith('fpp'):
#                self.bin['pp'] = output
#            else:
#                print 'ERROR: neither cpp or fpp is found'
#                sys.exit(-1)

        # parsing intrinsic skip option
        if opts.intrinsic:
            subflags = []
            for line in opts.intrinsic:
                subflags.extend(line.split(','))

            for subf in subflags:
                if subf and subf.find('=')>0:
                    key, value = subf.split('=')
                    if key=='except':
                        self._attrs['search']['except'].extend(value.split(';'))
                    elif key=='add_intrinsic':
                        Intrinsic_Procedures.extend([name.lower() for name in value.split(';')])
                    else:
                        raise UserException('Unknown intrinsic sub option: %s' % subf)
                else:
                    if subf=='skip':
                        self._attrs['search']['skip_intrinsic'] = True
                    elif subf=='noskip':
                        self._attrs['search']['skip_intrinsic'] = False
                    else:
                        raise UserException('Unknown intrinsic option(s) in %s' % subf)
                       

        # parsing include parameters
        if opts.include:
            for inc in opts.include:
                inc_eq = inc.split('=')
                if len(inc_eq)==1:
                    for inc_colon in inc_eq[0].split(':'): 
                        self._attrs['include']['path'].append(inc_colon)
                if len(inc_eq)==1:
                    for inc_colon in inc_eq[0].split(':'): 
                        self._attrs['include']['path'].append(inc_colon)
                elif len(inc_eq)==2:
                    # TODO: support path for each file
                    pass
                else: raise UserException('Wrong format include: %s'%inc)

        if opts.include_ini:
            self._attrs['include']['opt'] = opts.include_ini

        if opts.exclude_ini:
            process_exclude_option(opts.exclude_ini, self._attrs['exclude'])

        # parsing macro parameters
        if opts.macro:
            for line in opts.macro:
                for macro in line.split(','): 
                    macro_eq = macro.split('=')
                    if len(macro_eq)==1:
                        self._attrs['include']['macro'][macro_eq[0]] = None
                    elif len(macro_eq)==2:
                        self._attrs['include']['macro'][macro_eq[0]] = macro_eq[1]
                    else: raise UserException('Wrong format include: %s'%inc)

        files = None
        if opts.source:

            isfree = None
            isstrict = None

            for line in opts.source:
                flags = collections.OrderedDict()
                for subflag in line.split(','):
                    if subflag.find('=')>0:
                        key, value = subflag.split('=')
                        if key=='format':
                            if value == 'free':
                                isfree = True
                            elif value == 'fixed':
                                isfree = False
                            self._attrs['source']['isfree'] = isfree
                        elif key=='strict':
                            if value == 'yes':
                                isstrict = True
                            elif value == 'no':
                                isstrict = False
                            self._attrs['source']['isstrict'] = isstrict

            for line in opts.source:
                flags = collections.OrderedDict()
                for subflag in line.split(','):
                    if subflag.find('=')>0:
                        key, value = subflag.split('=')
                        if key=='file':
                            flags[key] = value.split(':')
                        elif key=='alias':
                            p1, p2 = value.split(':')
                            if p1.endswith('/'): p1 = p1[:-1]
                            if p2.endswith('/'): p2 = p2[:-1]
                            self._attrs['source']['alias'][p1] = p2
                        elif key=='state':
                            for path in value.split(':'):
                                if os.path.exists(path):
                                    realpath = os.path.realpath(path)
                                    self._attrs['source'][key].append(realpath)
                                else:
                                    raise UserException('%s does not exist.'%os.path.realpath(path))
                        else:
                            flags[key] = value 
                    else:
                        flags[subflag] = None

                isfree = None
                isstrict = None

                if flags.has_key('file'):
                    subflags = collections.OrderedDict()
                    if isfree: subflags['isfree'] = isfree
                    if isstrict: subflags['isstrict'] = isstrict
                    for file in flags['file']:
                        realpath = os.path.realpath(file)
                        if files is None: files = []
                        files.append(realpath)
                        self._attrs['source']['file'][realpath] = subflags
                else:
                    if isfree: self._attrs['source']['isfree'] = isfree
                    if isstrict: self._attrs['source']['isstrict'] = isstrict

#        # dupulicate paths per each alias
#        if files is None:
#            newpath = set() 
#            for path in self._attrs['include']['path']:
#                newpath.add(path)
#                for p1, p2 in self._attrs['source']['alias'].iteritems():
#                    if path.startswith(p1):
#                        newpath.add(p2+path[len(p1):])
#                    elif path.startswith(p2):
#                        newpath.add(p1+path[len(p2):])
#            self._attrs['include']['path'] = list(newpath)
#
#        newfile =  collections.OrderedDict()
#        for path, value in self._attrs['include']['file'].iteritems():
#            newfile[path] = value
#            for p1, p2 in self._attrs['source']['alias'].iteritems():
#                if path.startswith(p1):
#                    newpath = p2+path[len(p1):]
#                    newfile[newpath] = copy.deepcopy(value) 
#                elif path.startswith(p2):
#                    newpath = p1+path[len(p2):]
#                    newfile[newpath] = copy.deepcopy(value) 
#        self._attrs['include']['file'] = newfile
#
#        for path, value in self._attrs['include']['file'].iteritems():
#            if value.has_key('path'):
#                newpath = set()
#                for path in value['path']:
#                    newpath.add(path)
#                    for p1, p2 in self._attrs['source']['alias'].iteritems():
#                        if path.startswith(p1):
#                            newpath.add(p2+path[len(p1):])
#                        elif path.startswith(p2):
#                            newpath.add(p1+path[len(p2):])
#                value['path'] = list(newpath)


        # parsing debugging options
        # syntax: a.b.c=d,e,f
        if opts.debug:
            for dbg in opts.debug:
                param_path, value = dbg.split('=')
                param_split = param_path.lower().split('.')
                value_split = value.lower().split(',')
                curdict = self._attrs['debug']
                for param in param_split[:-1]:
                    curdict = curdict[param] 
                exec('curdict[param_split[-1]] = value_split')

        # parsing logging options
        if opts.logging:
            for log in opts.logging:
                param_path, value = log.split('=')
                param_split = param_path.lower().split('.')
                value_split = value.lower().split(',')
                curdict = self._attrs['logging']
                for param in param_split[:-1]:
                    curdict = curdict[param] 
                exec('curdict[param_split[-1]] = value_split')

        if opts.outdir:
            self._attrs['path']['outdir'] = opts.outdir

        if opts.machinefile:
            if os.path.exists(opts.machinefile):
                self._attrs['machine']['inc'] = KgenConfigParser(allow_no_value=True)
                inc.read(opts.machinefile)
            else:
                print 'WARNING: "%s" machine file does not exist.'%opts.machinefile
        else:
            self._attrs['machine']['inc'] = self.find_machine()

        if self._attrs['machine']['inc']:
            self.read_machinefile(self._attrs['machine'], self._attrs['prerun'])

        if not os.path.exists(self._attrs['machine']['variable']['work_directory']):
            os.makedirs(self._attrs['machine']['variable']['work_directory'])

        # create state directories and change working directory
        if not os.path.exists(self._attrs['path']['outdir']):
            os.makedirs(self._attrs['path']['outdir'])
        os.chdir(self._attrs['path']['outdir'])

    def _process_extract_flags(self, opts):

        # parsing invocation parameters
        if opts.invocation:
            for line in opts.invocation:
                for invocation in line.split(','):
                    t = invocation.split(':')
                    if len(t) != 3:
                        raise UserException('Wrong invocation syntax: expected <mpi ranks>:<openmp numbers>:invocations but used %s'%invocation)

                    triple = []
                    for pair in t:
                        r = pair.split('-')
                        if len(r)==1:
                            triple.append((r[0],r[0]))
                        elif len(r)==2:
                            triple.append(r)
                        else:
                            raise UserException('Wrong invocation syntax: expected a single number or "number-number" format but used %s'%pair)
                    try:
                        int(triple[2][0])
                        int(triple[2][1])
                    except:
                        raise UserException('The last item in invocation triple should be number.')
                    self._attrs['invocation']['triples'].append(triple)

        # parsing data parameters
        if opts.data:
            for line in opts.data:
                for data in line.split(','):
                    key, value = data.split('=', 1)
                    if key == 'condition':
                        t = value.strip().split(':')
                        if len(t) == 1:
                            self._attrs['data']['condition'].append(('and', t[0]))
                        elif len(t) == 2:
                            #if t[0] in ('set', 'or', 'and'):
                            if t[0] in ('and'): # supports "and" only
                                self._attrs['data']['condition'].append((t[0], t[1]))
                            else:
                                raise UserException('Unknown condition action type: %s' % t[0])
                        else:
                            raise UserException('Wrong number of condition subvalues: %s' % value)
                    elif key == 'maxnuminvokes':
                        self._attrs['data']['maxnuminvokes'] = int(value)
                    else:
                        raise UserException('Unknown data option: %s' % key)

        # parsing OpenMP parameters
        if opts.openmp:
            self._attrs['openmp']['enabled'] = True
            for line in opts.openmp:
                for openmp in line.split(','):
                    if openmp=='enable':
                        pass
                    else:
                        key, value = openmp.split('=')
                        if key=='kernel-in-critical-region':
                            if value=='no':
                                self._attrs['openmp']['critical'] = False
                        elif key=='omp_num_threads':
                            if isinstance(value, str) and value.isdigit():
                                self._attrs['openmp']['maxnum_threads'] = int(value)
                        else:
                            raise UserException('Unknown OpenMP option: %s' % openmp)

        # parsing MPI parameters
        if opts.mpi:
            self._attrs['mpi']['enabled'] = True
            self._attrs['add_mpi_frame']['enabled'] = True
            for line in opts.mpi:
                for mpi in line.split(','):
                    if mpi=='enable':
                        pass
                    else:
                        key, value = mpi.split('=', 1)
                        if key=='comm':
                            self._attrs['mpi'][key] = value
                        elif key=='use':
                            mod_name, identifier = value.split(':')
                            self._attrs['mpi']['use_stmts'].append((mod_name, [identifier]))
                        elif key=='ranks':
                            print 'ranks subflag for mpi is not supported. Please use invocation flag instead'
                            sys.exit(-1)
                            #self._attrs['mpi'][key] = value.split(':')
                            #self._attrs['mpi']['size'] = len(self._attrs['mpi'][key])
                        elif key=='header':
                            self._attrs['mpi'][key] = value
                        else:
                            raise UserException('Unknown MPI option: %s' % mpi)

        # parsing kernel makefile parameters
        if opts.prerun:
            for line in opts.prerun:
                for comp in line.split(','):
                    key, value = comp.split('=', 1)
                    if key in [ 'build', 'run', 'kernel_build', 'kernel_run' ] :
                        self._attrs['prerun'][key] = dequote(value)
                    else:
                        raise UserException('Unknown prerun option: %s' % comp)

        if opts.rebuild:
            for line in opts.rebuild:
                for comp in line.split(','):
                    self._attrs['rebuild'][comp] = True

        if opts.cmd_clean:
            self._attrs['cmd_clean']['cmds'] = dequote(opts.cmd_clean)


        if opts.cmd_build:
            self._attrs['cmd_build']['cmds'] = dequote(opts.cmd_build)

        if opts.cmd_run:
            self._attrs['cmd_run']['cmds'] = dequote(opts.cmd_run)

        if opts.state_switch:
            for line in opts.state_switch:
                for run in line.split(','):
                    key, value = run.split('=', 1)
                    if key in [ 'directory', 'type', 'clean', 'script' ] :
                        self._attrs['state_switch'][key] = dequote(value)
                    else:
                        raise UserException('Unknown state-switch option: %s' % run)

        if opts.kernel_option:
            for line in opts.kernel_option:
                for kopt in line.split(','):
                    split_kopt = kopt.split('=', 1)
                    if len(split_kopt)==1:
                        self._attrs['kernel_option']['compiler']['add'].append(dequote(split_kopt[0]))
                    elif len(split_kopt)==2:
                        if split_kopt[0] in [ 'FC', 'FC_FLAGS' ]:
                            self._attrs['kernel_option'][split_kopt[0]] = dequote(split_kopt[1])
                        elif split_kopt[0] in ('add', 'remove'):
                            self._attrs['kernel_option']['compiler'][split_kopt[0]].extend(dequote(split_kopt[1]).split(':'))
                        elif split_kopt[0]=='link':
                            self._attrs['kernel_option']['linker']['add'].extend(dequote(split_kopt[1]).split(':'))
                        else:
                            raise UserException('Unknown state-switch option: %s' % kopt)

#        if opts.timing:
#            for time in opts.timing.split(','):
#                key, value = time.split('=', 1)
#                if key in [ 'repeat' ] :
#                    try:
#                        self._attrs['timing'][key] = value
#                    except:
#                        raise UserException('repeat sub-flag should be integer value: %s'%value)
#                else:
#                    raise UserException('Unknown timing option: %s' % time)

        # kernel correctness checks 
        if opts.check:
            for line in opts.check:
                for checkparams in line.split(','):
                    key, value = checkparams.split('=', 1)
                    key = key.lower()
                    value = value.lower()
                    if key=='pert_invar':
                        self._attrs['check'][key] = value.split(':')
                    elif key=='pert_lim':
                        self._attrs['check'][key] = value
                    elif key=='tolerance':
                        self._attrs['verify'][key] = value
                    else:
                        print 'WARNING: %s is not supported check parameter'%key

        # parsing logging options
        if opts.verbose_level:
            self._attrs['verify']['verboselevel'] = str(opts.verbose_level)

        # mpi frame code in kernel driver
        if opts.add_mpi_frame:
            for checkparams in opts.add_mpi_frame.split(','):
                sparam = checkparams.split('=')
                if len(sparam) == 2:
                    key, value = sparam
                    key = key.lower()
                    if key in ['np', 'mpirun', 'mpifc']:
                        self._attrs['add_mpi_frame']['enabled'] = True
                        self._attrs['add_mpi_frame'][key] = value
                    else:
                        print 'WARNING: %s is not supported add_mpi_frame parameter'%key
                elif len(sparam) == 1:
                    if sparam[0] == "enabled":
                        self._attrs['add_mpi_frame']['enabled'] = True
                    else:
                        print 'WARNING: %s is not supported add_mpi_frame parameter'%sparam[0]

        # cache pollution frame code in kernel driver
        if opts.add_cache_pollution:
            if opts.add_cache_pollution.isdigit():
                self._attrs['add_cache_pollution']['enabled'] = True
                self._attrs['add_cache_pollution']['size'] = int(opts.add_cache_pollution)
            else:
                print 'WARNING: %s is not supported add_cache_pollution parameter'%opts.add_cache_pollution

    def _process_repr_flags(self, opts):

        # generating model raw data
        self._attrs['model']['reuse_rawdata'] = opts.reuse_rawdata 

        if opts.repr_etime:
            for line in opts.repr_etime:
                for eopt in line.split(','):
                    split_eopt = eopt.split('=', 1)
                    if len(split_eopt)==1:
                        if split_eopt[0] == 'enable':
                            self._attrs['model']['types']['etime']['enabled'] = True
                        elif split_eopt[0] == 'disable':
                            self._attrs['model']['types']['etime']['enabled'] = False
                        else:
                            raise UserException('Unknown elapsed-time flag option: %s' % eopt)
                    elif len(split_eopt)==2:

                        self._attrs['model']['types']['etime']['enabled'] = True

                        if split_eopt[0] in [ 'minval', 'maxval' ]:
                            self._attrs['model']['types']['etime'][split_eopt[0]] = float(split_eopt[1])
                        elif split_eopt[0] in ('nbins', 'ndata'):
                            self._attrs['model']['types']['etime'][split_eopt[0]] = int(split_eopt[1])
                        elif split_eopt[0] in ('timer', ):
                            self._attrs['model']['types']['etime'][split_eopt[0]] = split_eopt[1]
                        else:
                            raise UserException('Unknown elapsed-time flag option: %s' % eopt)

        if opts.repr_papi:
            for line in opts.repr_papi:
                for popt in line.split(','):
                    split_popt = popt.split('=', 1)
                    if len(split_popt)==1:
                        if split_popt[0] == 'enable':
                            self._attrs['model']['types']['papi']['enabled'] = True
                        elif split_popt[0] == 'disable':
                            self._attrs['model']['types']['papi']['enabled'] = False
                        else:
                            raise UserException('Unknown papi-counter flag option: %s' % popt)
                    elif len(split_popt)==2:

                        self._attrs['model']['types']['papi']['enabled'] = True

                        if split_popt[0] in [ 'minval', 'maxval', 'header', 'static', 'dynamic', 'event' ]:
                            self._attrs['model']['types']['papi'][split_popt[0]] = split_popt[1]
                        elif split_popt[0] in ('nbins', 'ndata'):
                            self._attrs['model']['types']['papi'][split_popt[0]] = int(split_popt[1])
                        else:
                            raise UserException('Unknown papi-counter flag option: %s' % popt)

        if opts.repr_code:
            for line in opts.repr_code:
                for copt in line.split(','):
                    split_copt = copt.split('=', 1)
                    if len(split_copt)==1:
                        if split_copt[0] == 'enable':
                            self._attrs['model']['types']['code']['enabled'] = True
                        elif split_copt[0] == 'disable':
                            self._attrs['model']['types']['code']['enabled'] = False
                        else:
                            raise UserException('Unknown code-coverage flag option: %s' % copt)
                    elif len(split_copt)==2:

                        self._attrs['model']['types']['code']['enabled'] = True

                        if split_copt[0] in [ 'percentage' ]:
                            self._attrs['model']['types']['code'][split_copt[0]] = float(split_copt[1])
                        elif split_copt[0] in [ 'filter' ]:
                            self._attrs['model']['types']['code'][split_copt[0]] = split_copt[1].strip().split(':')
                        elif split_copt[0] in ( 'ndata' ):
                            self._attrs['model']['types']['code'][split_popt[0]] = int(split_copt[1])
                        else:
                            raise UserException('Unknown code-coverage flag option: %s' % copt)
        
            # enable coverage feature at extractor
            if self._attrs['model']['types']['code']['enabled']:
                self._attrs['plugin']['priority']['ext.coverage'] = '%s/plugins/coverage'%KGEN_EXT

    def get_exclude_actions(self, section_name, *args ):
        if section_name=='namepath':
            if len(args)<1: return []

            if section_name in self.exclude:
                options = self.exclude[section_name]
                for pattern, actions in options.iteritems():
                    if match_namepath(pattern, args[0]):
                        return actions
            return []
        else:
            UserException('Not supported section name in exclusion input file: %s'%section)


    def process_include_option(self):

        incattrs = self.include

        if incattrs['opt']:
            self.includefile = os.path.basename(incattrs['opt'])
            shutil.copy(incattrs['opt'], self.path['outdir'])

        # collect include configuration information
        Inc = KgenConfigParser(allow_no_value=True)
        #Inc.optionxform = str
        Inc.read('%s/%s'%(self.path['outdir'], self.includefile))
        for section in Inc.sections():
            lsection = section.lower().strip()
            #if lsection in [ 'type', 'rename', 'state', 'extern' ]:
            if lsection in [ 'type', 'macro' ]:
                for option in Inc.options(section):
                    incattrs[lsection][option] = Inc.get(section, option).strip()
            elif lsection=='import':
                for option in Inc.options(section):
                    incattrs[lsection][option] = Inc.get(section, option).strip()
    #                subflags = collections.OrderedDict()
    #                for subf in Inc.get(section, option).split(','):
    #                    subflags[subf.strip()] = None
    #                incattrs[lsection][option] = subflags
            elif lsection=='include':
                for option in Inc.options(section):
                    incattrs['path'].append(option.strip())
            elif lsection=='compiler':
                for option in Inc.options(section):
                    incattrs[lsection][option] = Inc.get(section, option).strip()
            elif os.path.isfile(section):
                realpath = os.path.realpath(section)
                if not incattrs['file'].has_key(realpath):
                    incattrs['file'][realpath] = collections.OrderedDict()
                    incattrs['file'][realpath]['path'] = ['.']
                    incattrs['file'][realpath]['compiler'] = None 
                    incattrs['file'][realpath]['compiler_options'] = None
                    incattrs['file'][realpath]['macro'] = collections.OrderedDict()
                for option in Inc.options(section):
                    if option=='include':
                        pathlist = Inc.get(section, option).split(':')
                        incattrs['file'][realpath]['path'].extend(pathlist)
                    elif option in [ 'compiler', 'compiler_options' ]:
                        incattrs['file'][realpath][option] = Inc.get(section, option)
                    else:
                        incattrs['file'][realpath]['macro'][option] = Inc.get(section, option)
            else:
                pass
                #print '%s is either not suppored keyword or can not be found. Ignored.' % section

        # dupulicate paths per each alias
        newpath = set() 
        for path in self._attrs['include']['path']:
            newpath.add(path)
            for p1, p2 in self._attrs['source']['alias'].iteritems():
                if path.startswith(p1):
                    newpath.add(p2+path[len(p1):])
                elif path.startswith(p2):
                    newpath.add(p1+path[len(p2):])
        self._attrs['include']['path'] = list(newpath)

        newfile =  collections.OrderedDict()
        for path, value in self._attrs['include']['file'].iteritems():
            newfile[path] = value
            for p1, p2 in self._attrs['source']['alias'].iteritems():
                if path.startswith(p1):
                    newpath = p2+path[len(p1):]
                    newfile[newpath] = copy.deepcopy(value) 
                elif path.startswith(p2):
                    newpath = p1+path[len(p2):]
                    newfile[newpath] = copy.deepcopy(value) 
        self._attrs['include']['file'] = newfile

        for path, value in self._attrs['include']['file'].iteritems():
            if value.has_key('path'):
                newpath = set()
                for path in value['path']:
                    newpath.add(path)
                    for p1, p2 in self._attrs['source']['alias'].iteritems():
                        if path.startswith(p1):
                            newpath.add(p2+path[len(p1):])
                        elif path.startswith(p2):
                            newpath.add(p1+path[len(p2):])
                value['path'] = list(newpath)



    def find_machine(self):

        # find machine files
        inc = None
        for path in glob.glob('%s/*'%KGEN_MACHINE):
            try:
                inc = KgenConfigParser(allow_no_value=True)
                inc.read(path)
                cmd = inc.get('shell', 'command')
                out, err, retcode = run_shcmd(cmd)
                if retcode == 0:
                    if inc.has_option('shell', 'startswith'):
                        if any( out.startswith(s.strip()) for s in inc.get('shell', 'startswith').split(',') ):
                            break
                    elif inc.has_option('shell', 'pattern'):
                        if any( re.match(p.strip(), out) for p in inc.get('shell', 'pattern').split(',') ):
                            break
                inc = None
            except:
                inc = None

        if inc is None:
            inc = KgenConfigParser(allow_no_value=True)
            inc.read('%s/generic_Linux'%KGEN_MACHINE)

        return inc

    def read_machinefile(self, machine, prerun):

        # populate contents of the machine file
        try:
            inc = machine['inc']
            if inc.has_section('general'):
                if inc.has_option('general', 'name') and inc.get('general', 'name'):
                    machine['general']['name'] = inc.get('general', 'name')
                if inc.has_option('general', 'id') and inc.get('general', 'id'):
                    machine['general']['id'] = inc.get('general', 'id')
            if inc.has_section('variable'):
                if inc.has_option('variable', 'prerun_build') and inc.get('variable', 'prerun_build'):
                    prerun['build'] = inc.get('variable', 'prerun_build')
                if inc.has_option('variable', 'prerun_run') and inc.get('variable', 'prerun_run'):
                    prerun['run'] = inc.get('variable', 'prerun_run')
                if inc.has_option('variable', 'prerun_kernel_build') and inc.get('variable', 'prerun_kernel_build'):
                    prerun['kernel_build'] = inc.get('variable', 'prerun_kernel_build')
                if inc.has_option('variable', 'prerun_kernel_run') and inc.get('variable', 'prerun_kernel_run'):
                    prerun['kernel_run'] = inc.get('variable', 'prerun_kernel_run')
                if inc.has_option('variable', 'work_directory') and inc.get('variable', 'work_directory'):
                    machine['variable']['work_directory'] = os.path.expandvars(inc.get('variable', 'work_directory'))
            if inc.has_section('compiler'):
                if inc.has_option('compiler', 'gnu') and inc.get('compiler', 'gnu'):
                    machine['compiler']['gnu'] = inc.get('compiler', 'gnu')
                if inc.has_option('compiler', 'intel') and inc.get('compiler', 'intel'):
                    machine['compiler']['intel'] = inc.get('compiler', 'intel')
                if inc.has_option('compiler', 'pgi') and inc.get('compiler', 'pgi'):
                    machine['compiler']['pgi'] = inc.get('compiler', 'pgi')
        except:
            pass

