# kgen_prepost.py

import sys
import os.path
from kgen_utils import Logger, Config, traverse, UserException, show_tree
from kgen_state import State
from readfortran import FortranFileReader
from Fortran2003 import Specification_Part, Type_Declaration_Stmt, Entity_Decl, Parameter_Stmt, Named_Constant_Def, \
    NoMatchError, Module_Stmt, Program_Stmt, Named_Constant_Def_List

exclude_list = [ Module_Stmt, Program_Stmt ]
not_supported = {}
not_parsed = {}


def get_MPI_PARAM(node, bag, depth):
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

def check_mode():
    from kgen_utils import Config, exec_cmd
    from utils import module_file_extensions
    from api import parse, walk
    from statements import Comment
    from kgen_search import f2003_search_unknowns, SearchException
    import logging

    logger = logging.getLogger('kgen') # KGEN addition
    logger.setLevel(logging.WARNING)

    files = []

    # collect source files
    for path in Config.check_mode:
        if os.path.basename(path).startswith('.'): continue

        if os.path.isdir(path):
            for root, dirnames, filenames in os.walk(os.path.abspath(path)):
                for filename in filenames:
                    if os.path.basename(filename).startswith('.'): continue
                    fname, fext = os.path.splitext(filename)
                    if len(fext)>1 and fext.lower() in module_file_extensions:
                        files.append(os.path.join(root, filename))
        elif os.path.isfile(path):
            if os.path.isfile(path):
                files.append(os.path.abspath(path))
        else:
            raise '%s is not a direcotory nor a file'%path

    # TODO: support #include cpp directive
    # parse source files
    for n, file in enumerate(files):
        print 'Reading(%d/%d): '%(n+1, len(files)), file

#        fsrc  = open(file, 'rb')

        # prepare include paths and macro definitions
        path_src = []
        macros_src = []
        if Config.include['file'].has_key(self.abspath):
            path_src = Config.include['file'][self.abspath]['path']+[os.path.dirname(self.abspath)]
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
        prep = Config.bin['pp']
        if prep.endswith('fpp'): flags = Config.bin['fpp_flags']
        elif prep.endswith('cpp'): flags = Config.bin['cpp_flags']
        else: raise UserException('Preprocessor is not either fpp or cpp')

        output = exec_cmd('%s %s %s %s %s' % (prep, flags, includes, macros, file))

        # convert the preprocessed for fparser
        prep = map(lambda l: '!KGEN'+l if l.startswith('#') else l, output.split('\n'))

        # fparse
        tree = parse('\n'.join(prep), ignore_comments=False, analyze=False, isfree=True, isstrict=False, \
            include_dirs=None, source_only=None )

        # parse f2003
        Config.search['promote_exception'] = True

        lineno = 0
        linediff = 0
        for stmt, depth in walk(tree, -1):
            try:
                if isinstance(stmt, Comment) and stmt.item.comment.startswith('!KGEN#'):
                    comment_split = stmt.item.comment.split(' ')
                    lineno = int(comment_split[1])
                    stmt.item.span = ( 0, 0 )
                else:
                    if lineno>0:
                        linediff = stmt.item.span[0] - lineno
                        lineno = 0
                    stmt.item.span = ( stmt.item.span[0]-linediff, stmt.item.span[1]-linediff )

                stmt.parse_f2003()
                if stmt.f2003.__class__ not in exclude_list:
                    f2003_search_unknowns(stmt, stmt.f2003, gentype=KGGenType.KERNEL) 
            except (NoMatchError, AttributeError) as e:
                if file not in not_parsed:
                    not_parsed[file] = []
                not_parsed[file].append(stmt)
            except NameError as e:
                errmsg = str(e)
                pos = errmsg.find('search_')
                if len(errmsg)>7 and pos>0:
                    clsname = errmsg[pos+7:-16]
                    #print "NOT SUPPORTED: '%s' Fortran statement is not supported yet"%clsname
                    if file not in not_supported:
                        not_supported[file] = []
                    not_supported[file].append((clsname, stmt.item.span[0]))
            except Exception as e:
                #import pdb; pdb.set_trace()
                print 'WARNING: Following statement is not correctly parsed'
                print stmt
                print ''

    print ''
    print '********************'
    print '*** CHECK RESULT ***'
    print '********************'
    print ''
    print 'NOTE: KGEN may be able to extract kernel even though not all source code lines are parsed or supported.'
    print ''

    print '*** KGEN Parsing Error(s) ***'
    print ''
    for file, stmts in not_parsed.iteritems():
        print file
        lines = []
        for stmt in stmts:
            if hasattr(stmt, 'item'):
                lines.append('Near line # %d:'%stmt.item.span[0])
                lines.append(stmt.tokgen()+'\n')
            else:
                lines.append(str(stmt)+'\n')
        print '\n'.join(lines), '\n'

    print '*** Not Supported Fortran Statement(s) ***'
    print ''
    for file, clsnames in not_supported.iteritems():
        print file
        lines = []
        for clsname, lineno in clsnames:
            lines.append("'%s' Fortran statment near line # %d"%(clsname, lineno))
        print '\n'.join(lines), '\n'

    if len(not_parsed)==0 and len(not_supported)==0:
        print 'Current KGEN version can support all source code lines.'

def preprocess():
    from kgen_state import SrcFile

    if Config.check_mode:
        check_mode()
        sys.exit(0)
    else:
        if Config.mpi['enabled']:
            # get path of mpif.h
            mpifpath = ''
            if os.path.isabs(Config.mpi['header']):
                if os.path.exists(Config.mpi['header']):
                    mpifpath = Config.mpi['header']
                else:
                    raise UserException('Can not find %s'%Config.mpi['header'])
            else:
                for p in Config.include['path']: # KGEN addition
                    fp = os.path.join(p, Config.mpi['header'])
                    if os.path.exists(fp):
                        mpifpath = fp
                        break

            # collect required information
            if mpifpath:
                try:
                    reader = FortranFileReader(mpifpath, include_dirs = Config.include['path'])
                    spec = Specification_Part(reader)

                    bag = {}
                    if Config.mpi['comm'] is None:
                        bag['key'] = 'MPI_COMM_WORLD'
                        bag[bag['key']] = []
                        traverse(spec, get_MPI_PARAM, bag, subnode='content')
                        if bag.has_key(bag['key']):
                            Config.mpi['comm'] = bag[bag['key']][-1]
                        else:
                            raise UserException('Can not find MPI_COMM_WORLD in mpif.h')

                    if Config.mpi['logical'] is None:
                        bag['key'] = 'MPI_LOGICAL'
                        bag[bag['key']] = []
                        traverse(spec, get_MPI_PARAM, bag, subnode='content')
                        if bag.has_key(bag['key']):
                            Config.mpi['logical'] = bag[bag['key']][-1]
                        else:
                            raise UserException('Can not find MPI_LOGICAL in mpif.h')

                    if Config.mpi['status_size'] is None:
                        bag['key'] = 'MPI_STATUS_SIZE'
                        bag[bag['key']] = []
                        traverse(spec, get_MPI_PARAM, bag, subnode='content')
                        if bag.has_key(bag['key']):
                            Config.mpi['status_size'] = bag[bag['key']][-1]
                        else:
                            raise UserException('Can not find MPI_STATUS_SIZE in mpif.h')

                    if Config.mpi['any_source'] is None:
                        bag['key'] = 'MPI_ANY_SOURCE'
                        bag[bag['key']] = []
                        traverse(spec, get_MPI_PARAM, bag, subnode='content')
                        if bag.has_key(bag['key']):
                            Config.mpi['any_source'] =  bag[bag['key']][-1]
                        else:
                            raise UserException('Can not find MPI_ANY_SOURCE in mpif.h')

                    if Config.mpi['source'] is None:
                        bag['key'] = 'MPI_SOURCE'
                        bag[bag['key']] = []
                        traverse(spec, get_MPI_PARAM, bag, subnode='content')
                        if bag.has_key(bag['key']):
                            Config.mpi['source'] =  bag[bag['key']][-1]
                        else:
                            raise UserException('Can not find MPI_SOURCE in mpif.h')

                except Exception as e:
                    import pdb; pdb.set_trace()
                    raise UserException('Error occurred during reading %s.'%mpifpath)
            else:
                raise UserException('Can not find mpif.h. Please provide a path to the file')

        # parse imported source files through include.ini
        for path, import_type in Config.include['import'].iteritems(): 
            if 'source'==import_type:
                State.imported['source'].append(SrcFile(path))

def postprocess():
    # TODO: display summary for kernel generation
    from kgen_utils import exec_cmd

    # copy object files into kernel folder 
    if Config.include.has_key('import'):
        for path, import_type in Config.include['import'].iteritems():
            if 'object'==import_type:
                exec_cmd('cp -f %s %s'%(path, Config.path['kernel']))
