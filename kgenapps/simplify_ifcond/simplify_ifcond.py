''' Simplify IF condition in Fortran source files.'''

import sys
import os
import shutil
import optparse

IFCOND_APP = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%IFCOND_APP

sys.path.insert(0, '%s/base'%KGEN_HOME)
sys.path.insert(0, IFCOND_APP)

from kgen_utils import UserException, ProgramException, Logger, run_shcmd, show_tree, traverse
from kgen_state import SrcFile
from block_statements import IfThen, If, specification_part
from typedecl_statements import Logical
from statements import ElseIf, Comment, DummyStatement
import api

file_exts = ['f', 'f90', 'f95', 'f03', 'f08', '.ftn', 'F', 'F90', 'F95', 'F03', 'F08', '.FTN', '.inc']

def ifcondcheck(condexpr, threshold):
    def get_nodes(node, bag, depth):
        if node and not hasattr(node, 'items'):
            bag['nodes'].append(node)

    bag = {'nodes': []}
    traverse(condexpr, get_nodes, bag)
    nodes = bag['nodes']

    if len(nodes) > threshold:
        return True
    else:
        return False

def get_lastspecstmt(blockstmt):
    if not blockstmt: return None, None
    if not hasattr(blockstmt, 'content'): return None, None
    if isinstance(blockstmt.content, str) or len(blockstmt.content) < 1: return None, None

    specstmt = blockstmt
    idx = 0
    for i, stmt in enumerate(blockstmt.content):
        if stmt.__class__ in specification_part:
            specstmt = stmt
            idx = i+1

    return specstmt, idx
        
def add_specstmts(parstmt, numifstmts):
    # create decl stmt
    varstr = ', '.join([ 'ifcondvar%d'%num for num in range(numifstmts) ])
    declstmt = DummyStatement()
    #declstmt.forced_lines = [ 'LOGICAL %s'%varstr ]
    declstmt.forced_lines = [ 'LOGICAL ifcondvar0']

    # add decl
    lastspecstmt, idx = get_lastspecstmt(parstmt)
    if lastspecstmt:
        parstmt.content.insert(idx, declstmt)

    # add openmp dirs

def simplify(ifstmt, condexpr, ifstmtid):
    # create assign stmt
    #assignstr = 'ifcondvar%d = %s'%(ifstmtid, condexpr.tofortran())
    assignstr = 'ifcondvar0 = %s'%condexpr.tofortran()
    assignstmt = DummyStatement()
    assignstmt.forced_lines = [ assignstr ]

    # create simplified if stmt
    #ifstmt.expr = 'ifcondvar%d'%ifstmtid
    ifstmt.expr = 'ifcondvar0'
    ifstmt.forced_lines = [ ifstmt.tokgen() ]

    # add stmts
    p = ifstmt.parent
    idx = p.content.index(ifstmt)
    p.content.insert(idx, assignstmt)

def main():
    version = [ 0, 1, '0' ]
    outdir = '.'
    retval = 0
    varid = 0

    Logger.info('Starting simplify_ifcond', stdout=True)

    try:
        # option parser
        parser = optparse.OptionParser(version='simplify_ifcond version %d.%d.%s'%tuple(version))

        # common options
        parser.add_option("--outdir", dest="outdir", action='store', type='string', default='output', help="path to create outputs")
        parser.add_option("--add-ext", dest="ext", action='store', type='string', default=None, help="File extensions to parse")
        parser.add_option("-t", "--threshold", dest="threshold", action='store', type='int', default=20, help="Max number of identifiers if condition before simplifying.")

        opts, args = parser.parse_args()

        if len(args)<1:
            print 'ERROR: Target source folders are not provided.'
            print 'Usage: simplify_ifcond [options] <target folder path[, target folder path, ...]>'
            sys.exit(-1)

        # create output directory
        outpath = os.path.abspath(opts.outdir)
        if not os.path.exists(outpath):
            os.makedirs(outpath)
        outsrcpath = '%s/src'%outpath
        shutil.rmtree(outsrcpath)
        os.makedirs(outsrcpath)

        # walk through source directory tree
        for srcdir in args:
            abssrcpath = os.path.abspath(srcdir)

            for dirName, subdirList, fileList in os.walk(abssrcpath):
                relpath = os.path.relpath(dirName, start=abssrcpath)

                outfilepath = '%s/%s'%(outsrcpath, relpath.replace('.', ''))
                if not os.path.exists(outfilepath):
                    os.makedirs(outfilepath)

                for srcfile in fileList:
                    if any(srcfile.endswith(ext) for ext in file_exts):
                        try:
                            # read source file
                            parsed = SrcFile(os.path.join(dirName, srcfile), preprocess=False)

                            # create analysis container
                            parstmts = []

                            # anlyze
                            last_span = ( 1, 1 )
                            for stmt, depth in api.walk(parsed.tree):
                                if isinstance(stmt, Comment):
                                    if stmt.item.span[0] >= last_span[0] and stmt.item.span[1] <= last_span[1]:
                                        stmt.ignore = True
                                elif isinstance(stmt, (IfThen, If, ElseIf)):
                                    if ifcondcheck(stmt.f2003.items[0], opts.threshold):
                                        p = stmt.ancestors()[-1]
                                        if p not in parstmts:
                                            parstmts.append(p)
                                        if not hasattr(p, 'simplify_ifstmts'):
                                            p.simplify_ifstmts = []
                                        if stmt not in p.simplify_ifstmts:
                                            p.simplify_ifstmts.append(stmt)
                                last_span = stmt.item.span

                            # modify
                            for parstmt in parstmts:
                                # add openmp directive
                                add_specstmts(parstmt, len(parstmt.simplify_ifstmts))

                                # simplify
                                for i, ifstmt in enumerate(parstmt.simplify_ifstmts):
                                        simplify(ifstmt, ifstmt.f2003.items[0], i) 

                            # generate modified source files
                            if len(parstmts) > 0:

                                lines = []
                                for stmt, depth in api.walk(parsed.tree):
                                    if hasattr(stmt, 'forced_lines'):
                                        lines.extend(stmt.forced_lines)
                                    elif not stmt.ignore:
                                        start = stmt.item.span[0]-1
                                        end = stmt.item.span[1]
                                        for line in stmt.top.prep[start:end]:
                                            split = line.split()
                                            if len(split) > 2 and split[0].startswith('!KGEN#') and split[1].isdigit():
                                                continue
                                            lines.append(line)

                                with open(os.path.join(outfilepath, srcfile), 'w') as f:
                                    print 'Generating %s\n'%outfilepath
                                    f.write('\n'.join(lines))
                                    f.write('\n')

                        except Exception as e:
                            #import pdb; pdb.set_trace()
                            raise
                            pass


        # switch source files if directed

    except UserException as e:
        print 'ERROR: %s'%str(e)
        Logger.info(e)
        #Logger.critical(e)
        retval = -1
    except ProgramException as e:
        Logger.critical(e)
        retval = -1
    except Exception as e:
        Logger.critical(e)
        retval = -1
    finally:
        pass

    Logger.info('simplify_ifcond is finished.', stdout=True)

    return retval

if __name__ == '__main__':
    sys.exit(main())
