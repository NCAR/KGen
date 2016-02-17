#!/usr/bin/env python

from __future__ import print_function

import sys
import os
import inspect

# Python version check
if sys.hexversion < 0x020700F0:
    print('ERROR: KGEN works with Python Version 2.7 or later.')
    sys.exit(-1)

SCRIPT_HOME, SCRIPT_NAME = os.path.split(os.path.realpath(__file__))
KGEN_HOME = '%s/..'%SCRIPT_HOME
sys.path.insert(0, '%s/base'%KGEN_HOME)
sys.path.insert(1, '%s/packages'%KGEN_HOME)
TEST_SCRIPT = 'runtest.py'

from kgen_test import KGenTest
from ordereddict import OrderedDict

def report(testDB):
    ntests = len(testDB)
    npassed = len([ result for result in testDB.values() if result['general']['passed']])
    nfailed = ntests - npassed

    print( '' )
    print( '*********** TEST REPORT ***********' )
    print( '' )
    print( 'Total # of tests : %d'%ntests )
    print( '# of passed tests: %d'%npassed )
    print( '# of failed tests: %d'%nfailed )
    print( '' )

    for testid, result in testDB.iteritems():
        if result['general']['passed']:
            pass
        else:
            print( '%s : FAILED'%testid )
            print( 'ERROR MSG:', result['general']['errmsg'] )
            print( '' )
    
    print( '***********************************' )


def main():
    # search through test subdirectories
    import argparse

    testDB = OrderedDict()

    parser = argparse.ArgumentParser(description='Perform KGEN tests.')
    parser.add_argument('tests', type=str, nargs='*', help='Specify tests.')
    parser.add_argument('-f', dest='func_tests', action='store_true', default=False, help='Functional test only.')
    parser.add_argument('-s', dest='sys_tests', action='store_true', default=False, help='System test only.')
    parser.add_argument('-c', dest='changed', action='store_true', default=False, help='Changed test only.')
    parser.add_argument('-t', dest='leavetemp', action='store_true', default=False, help='Leave temporary directory.')
    parser.add_argument('-r', dest='rebuild', action='store_true', default=False, help='Rebuild target software.')
    parser.add_argument('-w', dest='work_dir', type=str, default=None, help='Set working directory.')
    parser.add_argument('-o', dest='user_options', type=str, default='', help='User-specific options.')
    parser.add_argument('--compiler', dest='compiler', type=str, default='ifort', help='Default compiler to be used for tests.')
    parser.add_argument('--compiler-flags', dest='compiler_flags', type=str, default='', help='Default compiler flgas to be used for tests.')

    # parse command line arguments
    args = parser.parse_args()

    # activate all test if no specific test is requested
    if not args.func_tests and not args.sys_tests:
        args.func_tests = True
        args.sys_tests = True

    # walk through test directory tree
    for dirName, subdirList, fileList in os.walk(SCRIPT_HOME):
        relpath = os.path.relpath(dirName, SCRIPT_HOME)
        if relpath.startswith('packages'): continue
        if relpath.startswith('old'):
            del subdirList[:]
            continue
        if relpath.endswith('templates'):
            del subdirList[:]
            continue

        if args.tests:
            if all(not relpath.startswith(argtest.rstrip(' /')) for argtest in args.tests) and \
                all( len(relpath.split('/'))>=len(argtest.rstrip(' /').split('/')) for argtest in args.tests ):
                continue

        # if kgen test script exists in a directory
        # the name of test script is fixed according to relative path to SCRIPT HOME
        path_script = '%s_test.py'%relpath.replace('/', '_')
        if path_script in fileList and dirName != SCRIPT_HOME:
            pathsave = sys.path[:]
            sys.path.insert(0, dirName)
            mod = __import__(path_script[:-3])

            test_found = False

            # find classes inherited from KGenTest class
            match = lambda x: inspect.isclass(x) and issubclass(x, KGenTest) and x is not KGenTest
            for name, cls in inspect.getmembers(mod, match):
                test_found = True
                break
            #if not test_found: sys.path = pathsave
            sys.path = pathsave


        # if TEST_SCRIPT exists in a directory
        if TEST_SCRIPT in fileList:
            pathsave = sys.path[:]
            sys.path.insert(0, dirName)
            mod = __import__(TEST_SCRIPT[:-3])

            test_found = False

            # find classes inherited from KGenTest class
            match = lambda x: inspect.isclass(x) and issubclass(x, KGenTest) and x is not KGenTest and len(x.__subclasses__())==0
            for name, cls in inspect.getmembers(mod, match):
                # process module level preparation
                print('Testing %s: ' % relpath, end='')
                sys.stdout.flush()

                #  generate test object
                try:
                    obj = cls()
                    obj.KGEN_HOME = KGEN_HOME
                    obj.WORK_DIR = args.work_dir
                    obj.TEST_HOME = SCRIPT_HOME
                    obj.TEST_SCRIPT = TEST_SCRIPT
                    obj.TEST_DIR = dirName
                    obj.TEST_NUM += 1
                    obj.TEST_ID = '%s/%s'%(relpath, name)
                    obj.COMPILER = args.compiler
                    obj.COMPILER_FLAGS = args.compiler_flags
                    obj.LEAVE_TEMP = args.leavetemp
                    obj.REBUILD = args.rebuild

                    if args.user_options:
                        options = [ opt.split('=') for opt in args.user_options.split(',') ]
                        obj.OPTIONS = {key: value for (key, value) in options}
                    else:
                        obj.OPTIONS = {}

                    # process class level preparation
                    #obj.configure_test()
                    testDB[obj.TEST_ID] = obj.perform_test()
                except Exception as e:
                    print('FAILED: %s'%str(e))
                    raise
            
            sys.path = pathsave

    report(testDB)

if __name__ == "__main__":
    main()
