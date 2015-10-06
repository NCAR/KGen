# kgen_state.py
# data structure

from kgen_utils import Logger, Config, singleton
from api import parse, walk

@singleton
class State(object):
    # KGEN states
    CREATED, CALLSITE_LOCATED, KERNELINFO_COLLECTED, GENINFO_MARKED, \
        STATE_GENERATED, KERNEL_GENERATED, MAKEFILES_GENERATED = range(7)

    # TKDPAT types
    TB_EXTERN, PB_INPUT, PB_OUTPUT, KD_INPUT, DT_MODULE, DT_CALLMODULE, DT_CALLPARENT, MOD_EXTERN = range(8)

    # module number
    mod_num = 1

    def __init__(self):

        # attributes holder
        self._attrs = {}

        # KGEN state
        self._attrs['state'] = self.CREATED

        # callsite attributes
        self._attrs['callsite'] = {}
        self._attrs['callsite']['stmt'] = None
        self._attrs['callsite']['expr'] = None
        self._attrs['callsite']['actual_arg'] = {}
        self._attrs['callsite']['actual_arg']['names'] = None # ActualArgList object
        self._attrs['callsite']['actual_arg']['in_names'] = []
        self._attrs['callsite']['actual_arg']['out_names'] = []
        self._attrs['callsite']['actual_arg']['inout_names'] = []
        self._attrs['callsite']['actual_arg']['res_stmt'] = {}

        # parent subprogram of callsite stmt attributes
        self._attrs['parentblock'] = {}
        self._attrs['parentblock']['stmt'] = None
        self._attrs['parentblock']['expr'] = None
        self._attrs['parentblock']['dummy_arg'] = {}
        self._attrs['parentblock']['dummy_arg']['names'] = []
        self._attrs['parentblock']['dummy_arg']['in_names'] = []
        self._attrs['parentblock']['dummy_arg']['out_names'] = []
        self._attrs['parentblock']['dummy_arg']['inout_names'] = []
        self._attrs['parentblock']['dummy_arg']['res_stmt'] = {}
        self._attrs['parentblock']['input'] = {} # variables for callsite actual args and kernel externs
        self._attrs['parentblock']['input']['names'] = []
        self._attrs['parentblock']['input']['res_stmt'] = {}
        self._attrs['parentblock']['output'] = {} # variables for callsite outputs
        self._attrs['parentblock']['output']['names'] = []
        self._attrs['parentblock']['output']['res_stmt'] = {}
        self._attrs['parentblock']['output']['tkdpat'] = []
        self._attrs['parentblock']['inout'] = {}
        self._attrs['parentblock']['inout']['tkdpat'] = []
        self._attrs['parentblock']['writesubr'] = {}
        self._attrs['parentblock']['writesubr']['tkdpat'] = []
        self._attrs['parentblock']['mod_rw_var_depends'] = [] # dependency for call kgen_write_var
        self._attrs['parentblock']['dtype'] = [] # derived types

        # callsite topblock attributes
        self._attrs['topblock'] = {}
        self._attrs['topblock']['file'] = None # SrcFile object
        self._attrs['topblock']['path'] = '' # absolute path
        self._attrs['topblock']['stmt'] = None
        self._attrs['topblock']['expr'] = None
        self._attrs['topblock']['extern'] = {}
        self._attrs['topblock']['extern']['names'] = []
        self._attrs['topblock']['extern']['res_stmt'] = {}
        self._attrs['topblock']['extern']['tkdpat'] = []
        self._attrs['topblock']['mod_depends'] = [] # dependency for compile
        self._attrs['topblock']['mod_rw_var_depends'] = [] # dependency for call kgen_write_var
        self._attrs['topblock']['dtype'] = [] # derived types

        # kernel attributes
        self._attrs['kernel'] = {}
        self._attrs['kernel']['stmt'] = None
        self._attrs['kernel']['expr'] = None
        self._attrs['kernel']['parent'] = None
        self._attrs['kernel']['top'] = None
        self._attrs['kernel']['dummy_arg'] = {}
        self._attrs['kernel']['dummy_arg']['names'] = []
        self._attrs['kernel']['dummy_arg']['in_names'] = []
        self._attrs['kernel']['dummy_arg']['out_names'] = []
        self._attrs['kernel']['dummy_arg']['inout_names'] = []
        self._attrs['kernel']['dummy_arg']['res_stmt'] = {}

        # modules
        self._attrs['modules'] = {}

        # module files
        self._attrs['depfiles'] = {}

        # kernel_driver attributes
        self._attrs['kernel_driver'] = {}
        self._attrs['kernel_driver']['input'] = {}
        self._attrs['kernel_driver']['input']['names'] = []
        self._attrs['kernel_driver']['input']['res_stmt'] = {}
        self._attrs['kernel_driver']['input']['tkdpat'] = []
        self._attrs['kernel_driver']['mod_rw_var_depends'] = []

        # program units
        self._attrs['program_units'] = {}

    def __getattr__(self, name):
        return self._attrs[name]

class ResState(object):
    ( NOT_STARTED, RESOLVED ) = range(2)

    def __init__(self, uname, org, resolvers):
        self.state = self.NOT_STARTED
        self.uname = uname
        self.originator = org
        self.resolvers = resolvers
        #self.temp_uname = None
        self.res_stmt = None
        self.unamelist = [uname]

    def push_uname(self, uname):
        self.unamelist.append(uname)
        self.uname = uname

    def pop_uname(self, reset_uname=False):
        newname = self.unamelist.pop()
        self.uname = self.unamelist[-1]
        if self.res_stmt and reset_uname:
            newlist = []
            for resuname in self.res_stmt.geninfo.values()[0]:
                if resuname==newname:
                    newlist.append(self.uname)
                else:
                    newlist.append(resuname)
                    pass
            self.res_stmt.geninfo.values()[0] = newlist
            #import pdb; pdb.set_trace()
            

class SrcFile(object):
    def __init__(self, srcpath):
        import os.path
        from kgen_utils import exec_cmd
        from statements import Comment
        from block_statements import Module, Program

        # set default values
        self.prep = None
        self.tree = None
        self.srcpath = srcpath
        self.abspath = os.path.abspath(self.srcpath)
        self.used4genstate = False

        
        # set source file format
        isfree = True
        isstrict = False
        if self.abspath in Config.source['file'].keys():
            if Config.source['file'][self.abspath].has_key('isfree'):
                isfree = Config.source['file'][self.abspath]['isfree']
            if Config.source['file'][self.abspath].has_key('isstrict'):
                isstrict = Config.source['file'][self.abspath]['isstrict']
        else:
            if Config.source['isstrict']: isstrict = Config.source['isstrict']
            if Config.source['isfree']: isfree = Config.source['isfree']

        # prepare include paths and macro definitions
        path_src = []
        macros_src = ''
        if Config.include['file'].has_key(self.abspath):
            path_src = Config.include['file'][self.abspath]['path']
            macros_src = ' '.join([ '-D%s=%s'%(k,v) for k, v in Config.include['file'][self.abspath]['macro'].iteritems() ])
        includes = '-I'+' -I'.join(Config.include['path']+path_src)
        macros = ' '.join([ '-D%s=%s'%(k,v) for k, v in Config.include['macro'].iteritems() ]) + ' ' + macros_src

        # execute preprocessing
        Logger.info('Reading %s'%self.srcpath, stdout=True)
        prep = Config.bin['pp']
        if prep.endswith('fpp'):
            if isfree: srcfmt = ' -free'
            else: srcfmt = ' -fixed'
            flags = Config.bin['fpp_flags'] + srcfmt
        elif prep.endswith('cpp'):
            flags = Config.bin['cpp_flags']
        else: raise UserException('Preprocessor is not either fpp or cpp')
        output = exec_cmd('%s %s %s %s %s' % (prep, flags, includes, macros, self.abspath))

        # convert the preprocessed for fparser
        self.prep = map(lambda l: '!KGEN'+l if l.startswith('#') else l, output.split('\n'))

        # fparse
        self.tree = parse('\n'.join(self.prep), ignore_comments=False, analyze=True, isfree=isfree, \
            isstrict=isstrict, include_dirs=None, source_only=None )
        self.tree.srcfile = self

        # parse f2003
        lineno = 0
        linediff = 0
        for stmt, depth in walk(self.tree, -1):
            stmt.parse_f2003()

        # rename reader.id
        self.tree.reader.id = self.abspath

        # collect module information
        for mod_name, mod_stmt in self.tree.a.module.iteritems(): 
            if not State.modules.has_key(mod_name):
                State.modules[mod_name] = {}
                State.modules[mod_name]['num'] = State.mod_num
                State.mod_num += 1
                State.modules[mod_name]['stmt'] = mod_stmt
                State.modules[mod_name]['file'] = self
                State.modules[mod_name]['path'] = self.abspath
                State.modules[mod_name]['extern'] = {}
                State.modules[mod_name]['extern']['names'] = []
                State.modules[mod_name]['extern']['res_stmt'] = {}
                State.modules[mod_name]['extern']['tkdpat'] = []
                State.modules[mod_name]['mod_rw_var_depends'] = []
                State.modules[mod_name]['dtype'] = []
        
        # collect program unit information
        for item in self.tree.content:
            if item.__class__ not in [ Module, Comment, Program ]:
                if item.reader.id not in State.program_units.keys():
                    State.program_units[item.reader.id] = []
                State.program_units[item.reader.id].append(item)

    def stmt_by_name(self, name, cls=None, lineafter=-1):
        from statements import Comment

        _stmt = None
        _expr = None

        for stmt, depth in walk(self.tree, -1):
            if isinstance(cls, list):
                if not stmt.__class__ in cls: continue 

            if lineafter>0:
                if stmt.item.span[1]<=lineafter: continue
                if isinstance(stmt, Comment): continue
 
            expr = stmt.expr_by_name(name, stmt.f2003)
            if lineafter>0 or expr is not None:
                _stmt = stmt
                _expr = expr
                break

        return _stmt, _expr

import unittest
class Test_kgen_state(unittest.TestCase):

    def setUp(self):
        pass

    def test_true(self):
        pass

if __name__ == "__main__":
    import sys
    #unittest.main(argv=[sys.argv[0]], verbosity=2) # verbosity is available from Python 2.7
    unittest.main(argv=[sys.argv[0]])
