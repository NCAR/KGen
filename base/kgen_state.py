# kgen_state.py
# data structure

from kgen_utils import Logger, Config, singleton, UserException, traverse, match_namepath, pack_exnamepath, KGGenType
from api import parse, walk
from ordereddict import OrderedDict

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
        self._attrs = OrderedDict()

        # KGEN state
        self._attrs['state'] = self.CREATED

        # callsite attributes
        self._attrs['callsite'] = OrderedDict()
        self._attrs['callsite']['stmts'] = []

        # parent subprogram of callsite stmt attributes
        self._attrs['parentblock'] = OrderedDict()
        self._attrs['parentblock']['stmt'] = None

        # callsite topblock attributes
        self._attrs['topblock'] = OrderedDict()
        self._attrs['topblock']['stmt'] = None

        # kernel attributes
        self._attrs['kernel'] = OrderedDict()
        self._attrs['kernel']['name'] = None

        # modules
        self._attrs['modules'] = OrderedDict()

        # src files
        self._attrs['srcfiles'] = OrderedDict()

        # used src files
        self._attrs['used_srcfiles'] = OrderedDict()

        # imported files
        self._attrs['imported'] = OrderedDict()
        self._attrs['imported']['source'] = []


        # kernel_driver attributes
        self._attrs['kernel_driver'] = OrderedDict()
        self._attrs['kernel_driver']['name'] = 'kernel_driver'
        self._attrs['kernel_driver']['callsite_args'] = ['kgen_unit', 'kgen_elapsed_time', 'kgen_isverified']

        # program units
        self._attrs['program_units'] = OrderedDict()

        # plugin database
        self._attrs['plugindb'] = OrderedDict()

    def __getattr__(self, name):
        return self._attrs[name]

class ResState(object):
    ( NOT_STARTED, RESOLVED ) = range(2)

    def __init__(self, gentype, uname, org, resolvers):
        self.state = self.NOT_STARTED
        self.gentype = gentype
        self.uname = uname
        self.originator = org
        self.resolvers = resolvers
        self.res_stmts = []
        self.unamelist = [uname]

    def push_uname(self, uname):
        self.unamelist.append(uname)
        self.uname = uname

    def pop_uname(self, reset_uname=False):
        newname = self.unamelist.pop()
        self.uname = self.unamelist[-1]
        if len(self.res_stmts)>0 and reset_uname:
            newlist = []
            for (resuname, req) in self.res_stmts[-1].geninfo.values()[0]:
                if resuname==newname:
                    newlist.append((self.uname, req))
                else:
                    newlist.append((resuname, req))
                    pass
            self.res_stmts[-1].geninfo.values()[0] = newlist
            

class SrcFile(object):
    def handle_include(self, lines):
        import re
        import os

        insert_lines = []
        for i, line in enumerate(lines):
            match = re.match(r'^\s*include\s*("[^"]+"|\'[^\']+\')\s*\Z', line, re.I)
            #if not match:
            #    match = re.match(r'\s*#include\s*("[^"]+"|\<[^\']+\>)\s*\Z', line, re.I)
            if match:
                if Config.include['file'].has_key(self.abspath):
                    include_dirs = Config.include['file'][self.abspath]['path']+Config.include['path']
                else:
                    include_dirs = Config.include['path']
                filename = match.group(1)[1:-1].strip()
                path = filename
                for incl_dir in include_dirs+[os.path.dirname(self.abspath)]:
                    path = os.path.join(incl_dir, filename)
                    if os.path.exists(path):
                        break
                if os.path.isfile(path):
                    with open(path, 'r') as f:
                        included_lines = f.read()
                        insert_lines.extend(self.handle_include(included_lines.split('\n')))
                else:
                    raise UserException('Can not find %s in include paths of %s.'%(filename, self.abspath))
            else:
                insert_lines.append(line)

        return insert_lines

    def __init__(self, srcpath, preprocess=True):
        import os.path
        from kgen_utils import run_shcmd
        from statements import Comment
        from block_statements import Module, Program

        # set default values
        self.tree = None
        self.srcpath = srcpath
        self.abspath = os.path.abspath(self.srcpath)

        
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
        macros_src = []
        if Config.include['file'].has_key(self.abspath):
            path_src = Config.include['file'][self.abspath]['path']+[os.path.dirname(self.abspath)]
            path_src = [ path for path in path_src if len(path)>0 ]
            for k, v in Config.include['file'][self.abspath]['macro'].iteritems():
                if v:
                    macros_src.append('-D%s=%s'%(k,v))
                else:
                    macros_src.append('-D%s'%k)
        includes = '-I'+' -I'.join(Config.include['path']+path_src)
        macros_common = []
        for k, v in Config.include['macro'].iteritems():
            if v:
                macros_common.append('-D%s=%s'%(k,v))
            else:
                macros_common.append('-D%s'%k)
        macros = ' '.join(macros_common + macros_src)

        # execute preprocessing
        Logger.info('Reading %s'%self.srcpath, stdout=True)

        new_lines = []
        with open(self.abspath, 'r') as f:
            if preprocess:
                pp = Config.bin['pp']
                if pp.endswith('fpp'):
                    if isfree: srcfmt = ' -free'
                    else: srcfmt = ' -fixed'
                    flags = Config.bin['fpp_flags'] + srcfmt
                elif pp.endswith('cpp'):
                    flags = Config.bin['cpp_flags']
                else: raise UserException('Preprocessor is not either fpp or cpp')

                output, err, retcode = run_shcmd('%s %s %s %s' % (pp, flags, includes, macros), input=f.read())
                prep = map(lambda l: '!KGEN'+l if l.startswith('#') else l, output.split('\n'))
                new_lines = self.handle_include(prep)
            else:
                new_lines = f.read().split('\n')

        # add include paths
        if Config.include['file'].has_key(self.abspath) and Config.include['file'][self.abspath].has_key('path'):
            include_dirs = Config.include['file'][self.abspath]['path'] + [os.path.dirname(self.abspath)]
        else: include_dirs = None

        # fparse
        self.tree = parse('\n'.join(new_lines), ignore_comments=False, analyze=True, isfree=isfree, \
            isstrict=isstrict, include_dirs=include_dirs, source_only=None )
        self.tree.prep = new_lines
        self.tree.used4genstate = False

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
                State.modules[mod_name] = OrderedDict()
                State.modules[mod_name]['stmt'] = mod_stmt
                State.modules[mod_name]['file'] = self
                State.modules[mod_name]['path'] = self.abspath
        
        # collect program unit information
        for item in self.tree.content:
            if item.__class__ not in [ Module, Comment, Program ]:
                if item.reader.id not in State.program_units.keys():
                    State.program_units[item.reader.id] = []
                State.program_units[item.reader.id].append(item)

        # create a tuple for file dependency
        State.srcfiles[self.abspath] = ( self, [], [] )

        self.process_directive()

    def stmt_by_name(self, name, cls=None, lineafter=-1):
        from statements import Comment

        for stmt, depth in walk(self.tree, -1):
            if isinstance(cls, list):
                if not stmt.__class__ in cls: continue 

            if lineafter>0:
                if stmt.item.span[1]<=lineafter: continue
                if isinstance(stmt, Comment): continue
 
            expr = stmt.expr_by_name(name, stmt.f2003)
            if expr: return stmt, expr

        return None, None

    def process_directive(self):
        from statements import Comment
        from block_statements import executable_construct
        import re

        def get_next_non_comment(stmt):
            if not stmt: return
            if not hasattr(stmt, 'parent'): return

            started = False
            for s in stmt.parent.content:
                if s==stmt:
                    if not isinstance(s, Comment): return s
                    started = True
                elif started:
                    if not isinstance(s, Comment): return s

        def get_names(node, bag, depth):
            from Fortran2003 import Name
            if isinstance(node, Name) and not node.string in bag:
                bag.append(node.string)
           
        # collect directives
        directs = []
        for stmt, depth in walk(self.tree):
            if isinstance(stmt, Comment):
                line = stmt.item.comment.strip()
                match = re.match(r'^[c!*]\$kgen\s+(.+)$', line, re.IGNORECASE)
                if match:
                    dsplit = match.group(1).split(' ', 1)
                    dname = dsplit[0].strip()
                    if len(dsplit)>1: clause = dsplit[1].strip()
                    else: clause = None

                    if dname.startswith('begin_'):
                        sname = dname[6:]
                        directs.append(sname)
                        State.kernel['name'] = clause
                    elif dname.startswith('end_'):
                        ename = dname[4:]
                        if directs[-1]==ename:
                            directs.pop()
                            if ename=='callsite':
                                pass
                            else:
                                raise UserException('WARNING: Not supported KGEN directive: %s'%ename)
                        else:
                            raise UserException('Directive name mismatch: %s, %s'%(dname_stack[-1], ename))
                    elif dname=='callsite':
                        next_fort_stmt = get_next_non_comment(stmt)
                        if next_fort_stmt:
                            State.kernel['name'] = clause
                            State.callsite['stmts'].append(next_fort_stmt)
                        else:
                            raise UserException('WARNING: callsite is not found')
                    elif dname=='write':
                        if clause:
                            stmt.write_state = clause.split(',')
                    elif dname=='exclude':
                        next_fort_stmt = get_next_non_comment(stmt)
                        if next_fort_stmt:
                            next_fort_stmt.f2003.skip_search = True
                        else:
                            raise UserException('WARNING: exclude target is not found')

                elif 'callsite' in directs:
                    State.callsite['stmts'].append(stmt)
            elif 'callsite' in directs:
                State.callsite['stmts'].append(stmt)
            else:
                if Config.callsite['namepath'] and stmt.__class__ in executable_construct:
                    names = []
                    traverse(stmt.f2003, get_names, names)
                    for name in names:
                        if match_namepath(Config.callsite['namepath'], pack_exnamepath(stmt, name), internal=False):
                            State.kernel['name'] = name
                            for _s, _d in walk(stmt):
                                State.callsite['stmts'].append(_s)
                            return
                elif len(directs)>0 and directs[-1]=='callsite':
                    State.callsite['stmts'].append(stmt)

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
