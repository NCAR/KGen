'''KGen parser
'''
#import os.path
from kgutils import UserException
from kgconfig import Config
from statements import Comment
from block_statements import Module, Program
import os
import logging
import kgutils
import api
import collections

logger = logging.getLogger('kgen')

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
            match = re.match(r'^\s*include\s*("[^"]+"|\'[^\']+\')', line, re.I)
            #if not match:
            #    match = re.match(r'\s*#include\s*("[^"]+"|\<[^\']+\>)\s*\Z', line, re.I)
            if match:
                if Config.include['file'].has_key(self.realpath):
                    include_dirs = Config.include['file'][self.realpath]['path']+Config.include['path']
                else:
                    include_dirs = Config.include['path']

                if os.path.isfile(Config.mpi['header']):
                    include_dirs.insert(0, os.path.dirname(Config.mpi['header']))

                filename = match.group(1)[1:-1].strip()
                path = filename
                for incl_dir in include_dirs+[os.path.dirname(self.realpath)]:
                    path = os.path.join(incl_dir, filename)
                    if os.path.exists(path):
                        break
                if os.path.isfile(path):
                    with open(path, 'r') as f:
                        included_lines = f.read()
                        insert_lines.extend(self.handle_include(included_lines.split('\n')))
                else:
                    raise UserException('Can not find %s in include paths of %s.'%(filename, self.realpath))
            else:
                insert_lines.append(line)

        return insert_lines

    def __init__(self, srcpath, preprocess=True):

        # set default values
        self.tree = None
        self.srcpath = srcpath
        self.realpath = os.path.realpath(self.srcpath)

        
        # set source file format
        isfree = None
        isstrict = None
        if self.realpath in Config.source['file'].keys():
            if Config.source['file'][self.realpath].has_key('isfree'):
                isfree = Config.source['file'][self.realpath]['isfree']
            if Config.source['file'][self.realpath].has_key('isstrict'):
                isstrict = Config.source['file'][self.realpath]['isstrict']
        else:
            isstrict = Config.source['isstrict']
            isfree = Config.source['isfree']
        # prepare include paths and macro definitions
        path_src = []
        macros_src = []
        if Config.include['file'].has_key(self.realpath):
            path_src = Config.include['file'][self.realpath]['path']+[os.path.dirname(self.realpath)]
            path_src = [ path for path in path_src if len(path)>0 ]
            for k, v in Config.include['file'][self.realpath]['macro'].iteritems():
                if v:
                    macros_src.append('-D%s=%s'%(k,v))
                else:
                    macros_src.append('-D%s'%k)

        if os.path.isfile(Config.mpi['header']):
            includes = [ '-I %s'%incpath for incpath in [os.path.dirname(Config.mpi['header'])] + Config.include['path']+path_src ]
        else:
            includes = [ '-I %s'%incpath for incpath in Config.include['path']+path_src ]

        macros_common = []
        for k, v in Config.include['macro'].iteritems():
            if v:
                macros_common.append('-D%s=%s'%(k,v))
            else:
                macros_common.append('-D%s'%k)
        macros = ' '.join(macros_common + macros_src)

        # execute preprocessing
        logger.info('Reading %s'%self.srcpath)

        new_lines = []
        with open(self.realpath, 'r') as f:
            if preprocess:
                pp = Config.bin['pp']
                if pp.endswith('fpp'):
                    if isfree is None or isfree: srcfmt = ' -free'
                    else: srcfmt = ' -fixed'
                    flags = Config.bin['fpp_flags'] + srcfmt
                elif pp.endswith('cpp'):
                    flags = Config.bin['cpp_flags']
                else: raise UserException('Preprocessor is not either fpp or cpp')

                output, err, retcode = kgutils.run_shcmd('%s %s %s %s' % (pp, flags, ' '.join(includes), macros), input=f.read())
                prep = map(lambda l: '!KGEN'+l if l.startswith('#') else l, output.split('\n'))
                new_lines = self.handle_include(prep)
            else:
                new_lines = f.read().split('\n')

        # add include paths
        include_dirs = Config.include['path'][:]
        if Config.include['file'].has_key(self.realpath) and Config.include['file'][self.realpath].has_key('path'):
            include_dirs.extend(Config.include['file'][self.realpath]['path'])
            include_dirs.append(os.path.dirname(self.realpath))

        # fparse
        self.tree = api.parse('\n'.join(new_lines), ignore_comments=False, analyze=True, isfree=isfree, \
            isstrict=isstrict, include_dirs=include_dirs, source_only=None )
        self.tree.prep = new_lines

        # parse f2003
        lineno = 0
        linediff = 0
        for stmt, depth in api.walk(self.tree, -1):
            stmt.parse_f2003()

        # rename reader.id
        self.tree.reader.id = self.realpath

        # collect module information
        for mod_name, mod_stmt in self.tree.a.module.iteritems(): 
            if not Config.modules.has_key(mod_name):
                Config.modules[mod_name] = collections.OrderedDict()
                Config.modules[mod_name]['stmt'] = mod_stmt
                Config.modules[mod_name]['file'] = self
                Config.modules[mod_name]['path'] = self.realpath
        
        # collect program unit information
        for item in self.tree.content:
            if item.__class__ not in [ Module, Comment, Program ]:
                if item.reader.id not in Config.program_units.keys():
                    Config.program_units[item.reader.id] = []
                Config.program_units[item.reader.id].append(item)

        # create a tuple for file dependency
        Config.srcfiles[self.realpath] = ( self, [], [] )

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
        from kgsearch import f2003_search_unknowns
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
        for stmt, depth in api.walk(self.tree):
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
                        Config.kernel['name'] = clause
                    elif dname.startswith('end_'):
                        ename = dname[4:]
                        if directs[-1]==ename:
                            directs.pop()
                            if ename=='callsite':
                                while isinstance(Config.callsite['stmts'][-1], Comment):
                                    Config.callsite['stmts'].pop()
                            else:
                                raise UserException('WARNING: Not supported KGEN directive: %s'%ename)
                        else:
                            raise UserException('Directive name mismatch: %s, %s'%(dname_stack[-1], ename))
                    elif dname=='callsite':
                        next_fort_stmt = get_next_non_comment(stmt)
                        if next_fort_stmt:
                            Config.kernel['name'] = clause
                            Config.callsite['stmts'].append(next_fort_stmt)
                        else:
                            raise UserException('WARNING: callsite is not found')
                    elif dname=='write':
                        if clause:
                            stmt.write_state = tuple( c.strip() for c in clause.split(',') )
                            if not hasattr(stmt, 'unknowns'):
                                f2003_search_unknowns(stmt, stmt.f2003)
                            if hasattr(stmt, 'unknowns'):
                                for unk, req in stmt.unknowns.iteritems():
                                    if req.state != ResState.RESOLVED:
                                        stmt.resolve(req) 

                    elif dname=='exclude':
                        next_fort_stmt = get_next_non_comment(stmt)
                        if next_fort_stmt:
                            next_fort_stmt.f2003.skip_search = True
                            next_fort_stmt.f2003.after_exclude = True
                        else:
                            raise UserException('WARNING: exclude target is not found')

                elif 'callsite' in directs: # if not match and within callsite
                    if Config.callsite['stmts'] or not isinstance(stmt, Comment):
                        Config.callsite['stmts'].append(stmt)
            elif 'callsite' in directs: # if not Comment
                Config.callsite['stmts'].append(stmt)
            else: # not in callsite
                if Config.callsite['namepath'] and stmt.__class__ in executable_construct:
                    names = []
                    kgutils.traverse(stmt.f2003, get_names, names)
                    for name in names:
                        if kgutils.match_namepath(Config.callsite['namepath'], kgutils.pack_exnamepath(stmt, name), internal=False):
                            Config.kernel['name'] = name
                            for _s, _d in api.walk(stmt):
                                Config.callsite['stmts'].append(_s)
                            return
                elif len(directs)>0 and directs[-1]=='callsite':
                    Config.callsite['stmts'].append(stmt)
