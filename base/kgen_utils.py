# kgen_utils.py
# KGEN utillities

import os
import re
import sys
import subprocess
from collections import OrderedDict
from copy import deepcopy
import optparse
from Fortran2003 import Name, Data_Ref
from ConfigParser import RawConfigParser

#############################################################################
## COMMON
#############################################################################

# Put src folder first in path
sys.path = sys.path + [ os.path.dirname(__file__) ]

EXTERNAL_NAMELEVEL_SEPERATOR = ':'
INTERNAL_NAMELEVEL_SEPERATOR = '__kgen__' # lower-case only

def encode_NS(namepath):
    return namepath.replace(EXTERNAL_NAMELEVEL_SEPERATOR, INTERNAL_NAMELEVEL_SEPERATOR)

def decode_NS(namepath):
    return namepath.replace(INTERNAL_NAMELEVEL_SEPERATOR, EXTERNAL_NAMELEVEL_SEPERATOR)

class KGName(object):
    def __init__(self, name, node=None, stmt=None):
        if not name: raise ProgramException('Name can not be none or blank')
        if name[0].isdigit(): raise ProgramException('Name can not have digit as its first character')

        self.namepath = encode_NS(name).strip().lower() # lower case
        self.namelist = self.namepath.split(INTERNAL_NAMELEVEL_SEPERATOR)
        self.dataref = Data_Ref(self.namelist[-1])
        self.node = node
        self.stmt = stmt
        #self.rename = []

    def path(self):
        return decode_NS(self.namepath)

    def list(self):
        return self.namelist

    def dataref(self):
        return self.dataref

    def last(self):
        return self.namelist[-1]

    def first(self):
        return self.namelist[0]

    def firstpartname(self):
        if isinstance(self.dataref, Name):
            return self.dataref.string
        else:
            return self.dataref.items[0].string

    def __eq__(self, other):
        return self.namepath==other.namepath

    def __str__(self):
        raise Exception('KGName')

def _get_namepath(stmt, external):
    if external:
        return EXTERNAL_NAMELEVEL_SEPERATOR.join([ a.name.lower() for a in stmt.ancestors() ])
    else:
        return INTERNAL_NAMELEVEL_SEPERATOR.join([ a.name.lower() for a in stmt.ancestors() ])

def _pack_namepath(stmt, lastname, external):
    if external:
        return '%s%s%s'%(_get_namepath(stmt, True), EXTERNAL_NAMELEVEL_SEPERATOR, lastname)
    else:
        return '%s%s%s'%(_get_namepath(stmt, False), INTERNAL_NAMELEVEL_SEPERATOR, lastname)

def pack_innamepath(stmt, name):
    return _pack_namepath(stmt, name, False)

def pack_exnamepath(stmt, name):
    return _pack_namepath(stmt, name, True)

def get_innamepath(stmt):
    return _get_namepath(stmt, False)

def get_exnamepath(stmt):
    return _get_namepath(stmt, True)

def match_namepath(pattern, namepath, internal=True):

#name -> name in the beginning and the end
#:name: -> name in any location
#name: -> name at the beginning
#:name -> name at the end
#name1:name2 -> two level name
#name1:name2: -> more than two level name starts with the two names
#:name1:name2 -> more than two level name ends with the two names
#:name1:name2: -> more than two level name that the two name locates in the middle
#eventually data slicing

    if not pattern or not namepath: return False

    if internal:
        split_pattern = pattern.split(INTERNAL_NAMELEVEL_SEPERATOR)
        split_namepath = namepath.split(INTERNAL_NAMELEVEL_SEPERATOR)
    else:
        split_pattern = pattern.split(EXTERNAL_NAMELEVEL_SEPERATOR)
        split_namepath = namepath.split(EXTERNAL_NAMELEVEL_SEPERATOR)

    p = list(split_pattern)

    leading_mark = False
    if len(p[0])==0:
        leading_mark = True
        p = p[1:]

    ending_mark = False
    if len(p[-1])==0:
        ending_mark = True
        p = p[:-1]
        if len(p)==0:
            raise UserException('Wrong namepath format: %s'%split_pattern)

    n = list(split_namepath)
    while len(p)>0 and len(n)>0:
        if p[0]==n[0]:
            p = p[1:]
            n = n[1:]
        elif leading_mark:
            n = n[1:]
        elif len(p[0])==0:
            leading_mark = True
            p = p[1:]
        else:
            return False

    if len(p)==0:
        if len(n)>0:
            if ending_mark: return True
            return False
        else:
            return True
    else:
        if len(n)>0:
            raise ProgramException('Incorrect namepath match: (%s, %s)'%(split_pattern, split_namepath))
        else:
            return False
        
def singleton(cls):
    """ singleton generator """

    instances = OrderedDict()
    def get_instance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return get_instance()

def run_shcmd(cmd, input=None, **kwargs):

    show_error_msg = None
    if kwargs.has_key('show_error_msg'):
        show_error_msg = kwargs['show_error_msg']
        del kwargs['show_error_msg']

    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    out, err = proc.communicate(input=input)

    if proc.returncode != 0 and show_error_msg:
        print '>> %s' % cmd
        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(out)+'\n ERROR: '+str(err)+'\n'

    return out, err, proc.returncode

def strip_quote(string):
    if not string: return string
    return string.strip('"\'')

#def exec_cmd(cmd, show_error_msg=True, input=None):
#    import subprocess
#
#    proc = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
#
#    out, err = proc.communicate(input=input)
#
#    ret_code = proc.wait()
#    if ret_code != 0 and show_error_msg:
#        print '>> %s' % cmd
#        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(out)+'\n ERROR: '+str(err)+'\n'
#
#    return out

# traverse f2003 nodes
# traverse and func will return None if to continue processing
# traverse and func will return return code if to stop processing
# The return code will be forwarded to initial caller
# func will collect anything in bag during processing
def traverse(node, func, bag, subnode='items', prerun=True, depth=0):
    ret = None

    if prerun and func is not None:
        ret = func(node, bag, depth)
        if ret is not None: return ret

    if node and hasattr(node, subnode) and getattr(node, subnode) is not None:
        for child in getattr(node, subnode):
            ret = traverse(child, func, bag, subnode=subnode, prerun=prerun, depth=depth+1)

    if not prerun and func is not None:
        ret = func(node, bag, depth)
        if ret is not None: return ret

    return ret

def get_subtree(obj, tree, prefix='top', depth=0):
    tab = '    '
    postfix = ''
    if isinstance(obj, str): postfix = ' => ' + obj
    elif isinstance(obj, type): postfix = ' => ' + str(obj)
    elif obj.__class__.__name__=='Name': postfix = ' => ' + obj.string

    #tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix, depth ) ]
    if hasattr(obj, 'parent'):
        pcls = str(obj.parent.__class__)
    else:
        pcls = 'None'
    tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix + ': parent => ' + pcls , depth ) ]
    if hasattr(obj, 'items'):
        for item in obj.items:
            get_subtree(item, tree, prefix='item', depth=depth+1)

    if hasattr(obj, 'content'):
        for elem in obj.content:
            get_subtree(elem, tree, prefix='content', depth=depth+1)

def show_obj(obj):
    print 'CLS: ', obj.__class__
    print 'STR: ', str(obj)
    print 'DIR: ', dir(obj)

def show_tree(node, prevent_print=False):
    tree = []
    get_subtree(node, tree)
    lines = []
    for elem, depth in tree:
        line = '    '*depth + elem
        if not prevent_print:
            print line
        lines.append(line+'\n')
    return lines

class KgenConfigParser(RawConfigParser):
    def __init__(self, *args, **kwargs):
        RawConfigParser.__init__(self, *args, **kwargs)
        self.optionxform = str

    def _optname_colon_to_dot(self, line):
        newline = line.strip()

        if len(newline)>0:
            if newline[0]==';':
                return line
            elif newline[0]=='[' and newline[-1]==']':
                return line.replace(':', INTERNAL_NAMELEVEL_SEPERATOR)
            else:
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
    from Fortran2003 import Specification_Part, Type_Declaration_Stmt, Entity_Decl, Parameter_Stmt, Named_Constant_Def, \
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
    import re
    import os

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

#############################################################################
## RESOLUTION TYPE
#############################################################################

class KGGenType(object):
    STATE_IN = 0x2
    STATE_OUT = 0x3

    @classmethod
    def is_state_in(cls, value):
        return cls.STATE_IN == value

    @classmethod
    def is_state_out(cls, value):
        return cls.STATE_OUT == value

    @classmethod
    def is_state(cls, value):
        return cls.is_state_in(value) or cls.is_state_out(value)

    @classmethod
    def has_state_in(cls, geninfo):
        return geninfo.has_key(cls.STATE_IN)

    @classmethod
    def has_state_out(cls, geninfo):
        return geninfo.has_key(cls.STATE_OUT)

    @classmethod
    def has_state(cls, geninfo):
        return cls.has_state_in(geninfo) or cls.has_state_out(geninfo)

    @classmethod
    def get_state_in(cls, geninfo):
        return geninfo.get(cls.STATE_IN, [])

    @classmethod
    def get_state_out(cls, geninfo):
        return geninfo.get(cls.STATE_OUT, [])

    @classmethod
    def get_state(cls, geninfo):
        state = cls.get_state_in(geninfo)
        #state = cls.get_state_in_inout(geninfo)
        for uname, req in cls.get_state_out(geninfo):
            if all(not uname==u for u, r in state):
                state.append((uname, req))
        return state

    @classmethod
    def get_request_in(cls, uname, geninfo):
        if cls.has_state_in(geninfo):
            for sin_uname, req in cls.get_state_in(geninfo):
                if uname==sin_uname: return req

    @classmethod
    def get_request_out(cls, uname, geninfo):
        if cls.has_state_out(geninfo):
            for sout_uname, req in cls.get_state_out(geninfo):
                if uname==sout_uname: return req

    @classmethod
    def get_request(cls, uname, geninfo):
        if cls.has_state(geninfo):
            for s_uname, req in cls.get_state(geninfo):
                if uname==s_uname: return req

    @classmethod
    def has_uname_in(cls, uname, geninfo):
        if cls.get_request_in(uname, geninfo): return True
        else: return False

    @classmethod
    def has_uname_out(cls, uname, geninfo):
        if cls.get_request_out(uname, geninfo): return True
        else: return False

    @classmethod
    def has_uname(cls, uname, geninfo):
        if cls.get_request(uname, geninfo): return True
        else: return False

#############################################################################
## EXCEPTION
#############################################################################

class KGException(Exception):
    pass

class UserException(KGException):
    pass

class ProgramException(KGException):
    pass

#############################################################################
## CONFIG
#############################################################################

def process_include_option(include_option, incattrs):

    # collect include configuration information
    Inc = KgenConfigParser(allow_no_value=True)
    #Inc.optionxform = str
    Inc.read(include_option)
    for section in Inc.sections():
        lsection = section.lower().strip()
        #if lsection in [ 'type', 'rename', 'state', 'extern' ]:
        if lsection in [ 'type', 'macro' ]:
            for option in Inc.options(section):
                incattrs[lsection][option] = Inc.get(section, option).strip()
        elif lsection=='import':
            for option in Inc.options(section):
                incattrs[lsection][option] = Inc.get(section, option).strip()
#                subflags = OrderedDict()
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
                incattrs['file'][abspath] = OrderedDict()
                incattrs['file'][abspath]['path'] = ['.']
                incattrs['file'][abspath]['compiler'] = None 
                incattrs['file'][abspath]['compiler_options'] = None
                incattrs['file'][abspath]['macro'] = OrderedDict()
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

            excattrs[lsection] = OrderedDict()
            for option in Exc.options(section):
                loption = option.lower().strip()
                excattrs[lsection][loption] = Exc.get(section, option).strip().split('=')
    else:
        UserException('Can not find exclude file: %s'%exclude_option)

def get_exclude_actions( section_name, *args ):
    if section_name=='namepath':
        if len(args)<1: return []

        if section_name in Config.exclude:
            options = Config.exclude[section_name]
            for pattern, actions in options.iteritems():
                if match_namepath(pattern, args[0]):
                    return actions
        return []
    else:
        UserException('Not supported section name in exclusion input file: %s'%section)

@singleton
class Config(object):
    """ KGEN configuration parameter holder """

    def __init__(self):

        # setup config parameters
        self._attrs = OrderedDict()
        self.opt_handlers = OrderedDict()

        # KGEN operation mode
        self._attrs['check_mode'] = False

        # Fortran parameters
        self._attrs['fort'] = OrderedDict()
        self._attrs['fort']['maxlinelen'] = 132

        # logging parameters
        self._attrs['logging'] = OrderedDict()
        self._attrs['logging']['select'] = OrderedDict()

        # callsite parameters
        self._attrs['callsite'] = OrderedDict()
        self._attrs['callsite']['filepath'] = ''
        self._attrs['callsite']['span'] = (-1, -1)
        self._attrs['callsite']['namepath'] = ''
#        self._attrs['callsite']['lineafter'] = -1

        # external tool parameters
        self._attrs['bin'] = OrderedDict()
        self._attrs['bin']['pp'] = 'cpp'
        self._attrs['bin']['cpp_flags'] = '-w -traditional'
        self._attrs['bin']['fpp_flags'] = '-w'

        # search parameters
        self._attrs['search'] = OrderedDict()
        self._attrs['search']['skip_intrinsic'] = True
        self._attrs['search']['except'] = []
        self._attrs['search']['promote_exception'] = False

        # path parameters
        self._attrs['path'] = OrderedDict()
        self._attrs['path']['outdir'] = '.'
        self._attrs['path']['state'] = 'state'
        self._attrs['path']['kernel'] = 'kernel'

        # source file parameters
        self._attrs['source'] = OrderedDict()
        self._attrs['source']['isfree'] = None
        self._attrs['source']['isstrict'] = None
        self._attrs['source']['alias'] = OrderedDict()
        self._attrs['source']['file'] = OrderedDict()
        self._attrs['source']['state'] = []

        # include parameters
        self._attrs['include'] = OrderedDict()
        self._attrs['include']['macro'] = OrderedDict()
        self._attrs['include']['path'] = ['.']
        self._attrs['include']['type'] = OrderedDict()
        self._attrs['include']['compiler'] = OrderedDict()
        self._attrs['include']['import'] = OrderedDict()
        self._attrs['include']['file'] = OrderedDict()

        # exclude parameters
        self._attrs['exclude'] = OrderedDict()

        # debugging parameters
        self._attrs['debug'] = OrderedDict()
        self._attrs['debug']['printvar'] = []

        # plugin parameters
        self._attrs['plugin'] = OrderedDict()
        self._attrs['plugin']['priority'] = OrderedDict()

        self.parser = optparse.OptionParser()

    def register(self, cfg):
    
        if not cfg:
            raise ProgramException('Custom configuration is not provided.')

        if hasattr(cfg, 'attrs'):
            self._attrs.update(cfg.attrs)

        # add custom options 
        if hasattr(cfg, 'options'):
            for opt_handler, args, kwargs in cfg.options:
                self.parser.add_option(*args, **kwargs)
                self.opt_handlers[kwargs['dest']] = opt_handler

    def apply(self, argv=None, cfg=None):
        if cfg and hasattr(cfg, 'attrs'):
            self._attrs.update(cfg.attrs)
        
        # parsing arguments

        # add default options
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

        #self.parser.set_usage(cfg.usage)

        # add custom options 
        if cfg and hasattr(cfg, 'options'):
            for opt_handler, args, kwargs in cfg.options:
                self.parser.add_option(*args, **kwargs)
                self.opt_handlers[kwargs['dest']] = opt_handler

        opts, args = self.parser.parse_args(args=argv)

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

        # process custom flags
        for optname, opt_handler in self.opt_handlers.items():
            opt = getattr(opts, optname, None)
            if opt and optname in self.opt_handlers:
                self.opt_handlers[optname](opt)
                
        # collect mpi params
        self._collect_mpi_params()

    def _collect_mpi_params(self):
        from api import parse, walk

        if Config.mpi['enabled']:
            # get path of mpif.h
            mpifpath = ''
            if os.path.isabs(Config.mpi['header']):
                if os.path.exists(Config.mpi['header']):
                    mpifpath = Config.mpi['header']
                else:
                    raise UserException('Can not find %s'%Config.mpi['header'])
            else:
                for p in Config.include['path']:
                    fp = os.path.join(p, Config.mpi['header'])
                    if os.path.exists(fp):
                        mpifpath = fp
                        break
                if not mpifpath:
                    for incpath, incdict in Config.include['file'].items():
                        for p in incdict['path']:
                            fp = os.path.join(p, Config.mpi['header'])
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
                    tree = parse(lines, ignore_comments=True, analyze=False, isfree=True, isstrict=False, include_dirs=None, source_only=None )
                    for stmt, depth in walk(tree, -1):
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
                        if not Config.mpi.has_key(config_key) or Config.mpi[config_key] is None:
                            for stmt, depth in walk(tree, -1):
                                bag['key'] = name
                                bag[name] = []
                                if hasattr(stmt, 'f2003'):
                                    traverse(stmt.f2003, get_MPI_PARAM, bag, subnode='content')
                                    if len(bag[name]) > 0:
                                        Config.mpi[config_key] = bag[name][-1]
                                        break

                    for config_key, name in config_name_mapping:
                        if not Config.mpi.has_key(config_key) or Config.mpi[config_key] is None:
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

        if opts.include_ini:
            process_include_option(opts.include_ini, self._attrs['include'])

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
                flags = OrderedDict()
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
                    subflags = OrderedDict()
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

        newfile =  OrderedDict()
        for path, value in self._attrs['include']['file'].iteritems():
            newfile[path] = value
            for p1, p2 in self._attrs['source']['alias'].iteritems():
                if path.startswith(p1):
                    newpath = p2+path[len(p1):]
                    newfile[newpath] = deepcopy(value) 
                elif path.startswith(p2):
                    newpath = p1+path[len(p2):]
                    newfile[newpath] = deepcopy(value) 
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

    def __getattr__(self, name):
        return self._attrs[name]

#############################################################################
## LOGGING
#############################################################################

def check_logging(func):
    """ logging decorator to check if to continue to log """

    def func_wrapper(obj, msg, **kwargs):
        exe_func = True
        if Config.logging['select'].has_key('name'):
            exe_func = False
            if kwargs.has_key('name'):
                for pattern in Config.logging['select']['name']:
                    if match_namepath(encode_NS(pattern), kwargs['name'].namepath):
                        exe_func = True
                        break

        if kwargs.has_key('stmt'):
            stmt = kwargs['stmt']
            msg += ' at %s in %s' % ( str(stmt.item.span), stmt.item.reader.id )

        # prerun
        if kwargs.has_key('stdout') and kwargs['stdout']:
            print msg

        # execute func
        if exe_func or func.__name__ in [ 'error', 'critical']:
            func(obj, msg)

        # postrun

    return func_wrapper

@singleton
class Logger(object):
    """ KGEN logger """

    def __init__(self):
        import logging.config
        logconfig_path = os.path.join(os.path.dirname(__file__),'log.config')
        logging.config.fileConfig(logconfig_path)
        self.logger = logging.getLogger('kgen')

    def _pack_msg(self, msg):
        import traceback
        import inspect

        exc_type, exc_value, exc_traceback = sys.exc_info()
        tb = traceback.format_tb(exc_traceback)
        if len(tb)>0:
            return str(msg) + '\n' + '\n'.join(tb)
        else:
            frame=inspect.currentframe()
            frame=frame.f_back.f_back.f_back
            code=frame.f_code
            return '%s:%d - %s'%(os.path.basename(code.co_filename), frame.f_lineno, str(msg))

    @check_logging
    def debug(self, msg, **kwargs):
        self.logger.debug(self._pack_msg(msg))

    @check_logging
    def info(self, msg, **kwargs):
        self.logger.info(self._pack_msg(msg))

    @check_logging
    def warn(self, msg, **kwargs):
        self.logger.warn(self._pack_msg(msg))

    @check_logging
    def error(self, msg, **kwargs):
        self.logger.error(self._pack_msg(msg))

    @check_logging
    def critical(self, msg, **kwargs):
        self.logger.critical(self._pack_msg(msg))

    def exception(self, msg, **kwargs):
        import traceback
        exc_type, exc_value, exc_traceback = sys.exc_info()
        output = [ msg+'\n', '\n' ]
        output += traceback.format_tb(exc_traceback)
        if kwargs.has_key('node') and kwargs['node']:
            output += [ '\n' ] + show_tree(kwargs['node'], prevent_print=True)
        self.logger.info(''.join(output))

import unittest
class Test_kgen_utils(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_exec_cmd(self):
        output, err, retcode = run_shcmd('echo "TestOK"')
        self.assertEqual( output, "TestOK\n")
 

if __name__ == "__main__":
    #unittest.main(argv=[sys.argv[0]], verbosity=2) # verbosity is available from Python 2.7
    unittest.main(argv=[sys.argv[0]])
