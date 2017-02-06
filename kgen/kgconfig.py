
import os
import re
import sys
import collections
import copy
import optparse
from kgutils import UserException, run_shcmd, INTERNAL_NAMELEVEL_SEPERATOR, traverse
try:
    import configparser
except:
    import ConfigParser as configparser

KGEN_EXT = '%s/extractor'%os.path.dirname(os.path.realpath(__file__))
KGEN_COVER = '%s/coverage'%os.path.dirname(os.path.realpath(__file__))

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
        self._attrs['kgen']['version'] = [ 0, 7, '2' ]

        # KGEN operation mode
        self._attrs['check_mode'] = False

        # Fortran parameters
        self._attrs['fort'] = collections.OrderedDict()
        self._attrs['fort']['maxlinelen'] = 132

        # logging parameters
        self._attrs['logging'] = collections.OrderedDict()
        self._attrs['logging']['select'] = collections.OrderedDict()

        # callsite parameters
        self._attrs['callsite'] = collections.OrderedDict()
        self._attrs['callsite']['filepath'] = ''
        self._attrs['callsite']['span'] = (-1, -1)
        self._attrs['callsite']['namepath'] = ''
#        self._attrs['callsite']['lineafter'] = -1

        # external tool parameters
        self._attrs['bin'] = collections.OrderedDict()
        self._attrs['bin']['pp'] = 'cpp'
        self._attrs['bin']['cpp_flags'] = '-w -traditional'
        self._attrs['bin']['fpp_flags'] = '-w'

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
        self._attrs['path']['coverage'] = 'coverage'

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

        # exclude parameters
        self._attrs['exclude'] = collections.OrderedDict()

        # debugging parameters
        self._attrs['debug'] = collections.OrderedDict()
        self._attrs['debug']['printvar'] = []

        # plugin parameters
        self._attrs['plugin'] = collections.OrderedDict()
        self._attrs['plugin']['priority'] = collections.OrderedDict()

        ###############################################################
        # compiler flag information
        ###############################################################

        # coverage parameters
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
        #self._attrs['invocation']['triples'] = [ (('0','0'), ('0','0'), ('0','0')) ]
        self._attrs['invocation']['triples'] = None

        # add mpi frame code in kernel driver
        self._attrs['add_mpi_frame'] = collections.OrderedDict()
        self._attrs['add_mpi_frame']['enabled'] = False
        self._attrs['add_mpi_frame']['np'] = '2'
        self._attrs['add_mpi_frame']['mpiexec'] = 'mpiexec'

        # timing parameters
        self._attrs['timing'] = collections.OrderedDict()
        self._attrs['timing']['repeat'] = '10'

        # verification parameters
        self._attrs['verify'] = collections.OrderedDict()
        self._attrs['verify']['tolerance'] = '1.D-14'
        self._attrs['verify']['verboselevel'] = '1'

        # make kernel parameters
        self._attrs['kernel_option'] = collections.OrderedDict()
        self._attrs['kernel_option']['FC'] = None
        self._attrs['kernel_option']['FC_FLAGS'] = None
        self._attrs['kernel_option']['compiler'] = collections.OrderedDict()
        self._attrs['kernel_option']['compiler']['add'] = []
        self._attrs['kernel_option']['compiler']['remove'] = []
        self._attrs['kernel_option']['linker'] = collections.OrderedDict()
        self._attrs['kernel_option']['linker']['add'] = []

        # make prerun parameters
        self._attrs['prerun'] = collections.OrderedDict()
        self._attrs['prerun']['kernel_build'] = None
        self._attrs['prerun']['kernel_run'] = None
        self._attrs['prerun']['clean'] = None
        self._attrs['prerun']['build'] = None
        self._attrs['prerun']['run'] = None

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
        self._attrs['state_switch']['cmds'] = ''

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

        ###############################################################
        # coverage information
        ###############################################################

        # coverage parameters
        self._attrs['coveragefile'] = 'coverage.ini'
        self._attrs['coverage'] = collections.OrderedDict()

        # set plugin parameters
        self._attrs['plugin']['priority']['cover.gencore'] = '%s/plugins/gencore'%KGEN_COVER

        ###############################################################
        # state information
        ###############################################################

        self._attrs['modules'] = collections.OrderedDict()
        self._attrs['srcfiles'] = collections.OrderedDict()
        self._attrs['kernel'] = collections.OrderedDict()
        self._attrs['kernel']['name'] = None
        self._attrs['callsite'] = collections.OrderedDict()
        self._attrs['callsite']['stmts'] = []
        self._attrs['parentblock'] = collections.OrderedDict()
        self._attrs['parentblock']['stmt'] = None
        self._attrs['topblock'] = collections.OrderedDict()
        self._attrs['topblock']['stmt'] = None
        self._attrs['used_srcfiles'] = collections.OrderedDict()


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
        self.parser.add_option("--debug", dest="debug", action='append', type='string', help=optparse.SUPPRESS_HELP)
        self.parser.add_option("--logging", dest="logging", action='append', type='string', help=optparse.SUPPRESS_HELP)

        ###############################################################
        # Add extraction options
        ###############################################################

        self.parser.add_option("--invocation", dest="invocation", action='append', type='string', default=None, help="(process, thread, invocation) pairs of kernel for data collection")
        self.parser.add_option("--openmp", dest="openmp", action='append', type='string', default=None, help="Specifying OpenMP options")
        self.parser.add_option("--mpi", dest="mpi", action='append', type='string', default=None, help="MPI information for data collection")
        self.parser.add_option("--timing", dest="timing", action='store', type='string', default=None, help="Timing measurement information")
        self.parser.add_option("--prerun", dest="prerun", action='append', type='string', default=None, help="prerun commands")
        self.parser.add_option("--rebuild", dest="rebuild", action='append', type='string', default=None, help="rebuild controls")
        self.parser.add_option("--state-switch", dest="state_switch", action='append', type='string', default=None, help="Specifying how to switch orignal sources with instrumented ones.")
        self.parser.add_option("--cmd-clean", dest="cmd_clean", action='append', type='string', default=None, help="Clean information to generate makefile")
        self.parser.add_option("--cmd-build", dest="cmd_build", action='append', type='string', default=None, help="Build information to generate makefile")
        self.parser.add_option("--cmd-run", dest="cmd_run", action='append', type='string', default=None, help="Run information to generate makefile")
        self.parser.add_option("--kernel-option", dest="kernel_option", action='append', type='string', default=None, help="Specifying kernel compiler and linker options")
        self.parser.add_option("--check", dest="check", action='append', type='string', default=None, help="Kernel correctness check information")
        self.parser.add_option("--verbose", dest="verbose_level", action='store', type='int', default=None, help="Set the verbose level for verification output")
        self.parser.add_option("--add-mpi-frame", dest="add_mpi_frame", action='store', type='string', default=None, help="Add MPI frame codes in kernel_driver")

        #self.parser.set_usage(cfg.usage)

        opts, args = self.parser.parse_args()

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
            print 'ERROR: %s can not be found.' % callsite[0]
            sys.exit(-1)

        # set callsite filepath
        self.callsite['filepath'] = callsite[0]

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
                
        # collect mpi params
        #self.collect_mpi_params()

    def __getattr__(self, name):
        return self._attrs[name]

    def collect_mpi_params(self):
        #from parser.api import parse, walk
        import parser

        if self._attrs['mpi']['enabled']:
            # get path of mpif.h
            mpifpath = ''
            if os.path.isabs(self._attrs['mpi']['header']):
                if os.path.exists(self._attrs['mpi']['header']):
                    mpifpath = self.__attrs['.mpi']['header']
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
                    import pdb; pdb.set_trace()
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
            output = ''
            try:
                out, err, retcode = run_shcmd('which fpp', show_error_msg=False)
                output = out.strip()
            except Exception as e: pass
            if output.endswith('fpp'):
                self.bin['pp'] = output
            else:
                print 'ERROR: neither cpp or fpp is found'
                sys.exit(-1)

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

        if self.includefile:
            self.process_include_option()

        if opts.exclude_ini:
            process_exclude_option(opts.exclude_ini, self._attrs['exclude'])

        # parsing macro parameters
        if opts.macro:
            for line in opts.macro:
                for macro in line.split(','): 
                    macro_eq = macro.split('=')
                    if len(macro_eq)==1:
                        self._attrs['include']['macro'][macro_eq[0]] = '1'
                    elif len(macro_eq)==2:
                        self._attrs['include']['macro'][macro_eq[0]] = macro_eq[1]
                    else: raise UserException('Wrong format include: %s'%inc)

        files = None
        if opts.source:
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
                                    abspath = os.path.abspath(path)
                                    self._attrs['source'][key].append(abspath)
                                else:
                                    raise UserException('%s does not exist.'%os.path.abspath(path))
                        else:
                            flags[key] = value 
                    else:
                        flags[subflag] = None

                isfree = None
                isstrict = None
                if flags.has_key('format'):
                    if flags['format']=='free': isfree = True 
                    elif flags['format']=='fixed': isfree = False 
                    else: raise UserException('format subflag of source flag should be either free or fixed.')

                if flags.has_key('strict'):
                    if flags['strict']=='yes': isstrict = True 
                    elif flags['strict']=='no': isstrict = False 
                    else: raise UserException('strict subflag of source flag should be either yes or no.')

                if flags.has_key('file'):
                    subflags = collections.OrderedDict()
                    if isfree: subflags['isfree'] = isfree
                    if isstrict: subflags['isstrict'] = isstrict
                    for file in flags['file']:
                        abspath = os.path.abspath(file)
                        if files is None: files = []
                        files.append(abspath)
                        self._attrs['source']['file'][abspath] = subflags
                else:
                    if isfree: self._attrs['source']['isfree'] = isfree
                    if isstrict: self._attrs['source']['isstrict'] = isstrict

        # dupulicate paths per each alias
        if files is None:
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


        # parsing debugging options
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

        # create state directories and change working directory
        if not os.path.exists(self._attrs['path']['outdir']):
            os.makedirs(self._attrs['path']['outdir'])
        os.chdir(self._attrs['path']['outdir'])

    def _process_extract_flags(self, opts):

        # parsing invocation parameters
        if opts.invocation:
            self._attrs['invocation']['triples'] = []
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
            # remove for development of coverage
            #if not self._attrs['invocation']['triples']:
            #    self._attrs['invocation']['triples'] = [ (('0','0'), ('0','0'), ('0','0')) ]

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
                    if key in [ 'clean', 'build', 'run', 'kernel_build', 'kernel_run' ] :
                        self._attrs['prerun'][key] = value
                    else:
                        raise UserException('Unknown prerun option: %s' % comp)

        if opts.rebuild:
            for line in opts.rebuild:
                for comp in line.split(','):
                    self._attrs['rebuild'][comp] = True

        if opts.cmd_clean:
            for line in opts.cmd_clean:
                for clean in line.split(','):
                    kv = clean.split('=', 1)
                    if len(kv) == 1:
                        self._attrs['cmd_clean']['cmds'] = kv[0]
                    elif len(kv) ==2:
                        key, value = kv
                        if key in [ 'cmds' ] :
                            self._attrs['cmd_clean'][key] = value
                        else:
                            raise UserException('Unknown cmd-clean option: %s' % clean)
                    else:
                        raise UserException('Wrong syntax: %s' % clean)


        if opts.cmd_build:
            for line in opts.cmd_build:
                for build in line.split(','):
                    kv = build.split('=', 1)
                    if len(kv) == 1:
                        self._attrs['cmd_build']['cmds'] = kv[0]
                    elif len(kv) ==2:
                        key, value = kv
                        if key in [ 'cmds' ] :
                            self._attrs['cmd_build'][key] = value
                        else:
                            raise UserException('Unknown cmd-build option: %s' % build)
                    else:
                        raise UserException('Wrong syntax: %s' % build)

        if opts.cmd_run:
            for line in opts.cmd_run:
                for run in line.split(','):
                    kv = run.split('=', 1)
                    if len(kv) == 1:
                        self._attrs['cmd_run']['cmds'] = kv[0]
                    elif len(kv) ==2:
                        key, value = kv
                        if key in [ 'cmds' ] :
                            self._attrs['cmd_run'][key] = value
                        else:
                            raise UserException('Unknown cmd-run option: %s' % run)
                    else:
                        raise UserException('Wrong syntax: %s' % run)

        if opts.state_switch:
            for line in opts.state_switch:
                for run in line.split(','):
                    key, value = run.split('=', 1)
                    if key in [ 'cmds', 'type' ] :
                        self._attrs['state_switch'][key] = value
                    else:
                        raise UserException('Unknown state-switch option: %s' % run)

        if opts.kernel_option:
            for line in opts.kernel_option:
                for kopt in line.split(','):
                    split_kopt = kopt.split('=', 1)
                    if len(split_kopt)==1:
                        self._attrs['kernel_option']['compiler']['add'][split_kopt[0]] = None
                    elif len(split_kopt)==2:
                        if split_kopt[1] in [ 'FC', 'FC_FLAGS' ]:
                            self._attrs['kernel_option'][split_kopt[1]] = split_kopt[0]
                        elif split_kopt[1] in [ 'add', 'remove' ]:
                            self._attrs['kernel_option']['compiler'][split_kopt[1]].append(split_kopt[0])
                        elif split_kopt[1]=='link':
                            self._attrs['kernel_option']['linker']['add'].append(split_kopt[0])
                        else:
                            raise UserException('Unknown state-switch option: %s' % run)

        if opts.timing:
            for time in opts.timing.split(','):
                key, value = time.split('=', 1)
                if key in [ 'repeat' ] :
                    try:
                        self._attrs['timing'][key] = value
                    except:
                        raise UserException('repeat sub-flag should be integer value: %s'%value)
                else:
                    raise UserException('Unknown timing option: %s' % time)

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
            self._attrs['verify']['verboselevel'] = str(opt)

        # mpi frame code in kernel driver
        if opts.add_mpi_frame:
            self._attrs['add_mpi_frame']['enabled'] = True
            for checkparams in opts.add_mpi_frame.split(','):
                key, value = checkparams.split('=')
                key = key.lower()
                if key in ['np', 'mpiexec']:
                    self._attrs['add_mpi_frame'][key] = value
                else:
                    print 'WARNING: %s is not supported add_mpi_frame parameter'%key

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

        # collect include configuration information
        Inc = KgenConfigParser(allow_no_value=True)
        #Inc.optionxform = str
        Inc.read(self.includefile)
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
                abspath = os.path.abspath(section)
                if not incattrs['file'].has_key(abspath):
                    incattrs['file'][abspath] = collections.OrderedDict()
                    incattrs['file'][abspath]['path'] = ['.']
                    incattrs['file'][abspath]['compiler'] = None 
                    incattrs['file'][abspath]['compiler_options'] = None
                    incattrs['file'][abspath]['macro'] = collections.OrderedDict()
                for option in Inc.options(section):
                    if option=='include':
                        pathlist = Inc.get(section, option).split(':')
                        incattrs['file'][abspath]['path'].extend(pathlist)
                    elif option in [ 'compiler', 'compiler_options' ]:
                        incattrs['file'][abspath][option] = Inc.get(section, option)
                    else:
                        incattrs['file'][abspath]['macro'][option] = Inc.get(section, option)
            else:
                pass
                #print '%s is either not suppored keyword or can not be found. Ignored.' % section

