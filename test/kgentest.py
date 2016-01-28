#!/usr/bin/env python

import sys
import os
import inspect

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)

SCRIPT_HOME, SCRIPT_NAME = os.path.split(os.path.realpath(__file__))
KGEN_HOME = '%s/..'%SCRIPT_HOME
sys.path.insert(0, '%s/base'%KGEN_HOME)
sys.path.insert(1, '%s/packages'%KGEN_HOME)
TEST_SCRIPT = 'runtest.py'

from doit.loader import generate_tasks
from doit.doit_cmd import DoitMain
from doit.cmd_base import TaskLoader
from kgen_test import KGenTest, KExtTest, KCoverTest
from ordereddict import OrderedDict

class KGenTestTaskLoader(TaskLoader):
    """create test tasks on the fly based on cmd-line arguments"""
    DOIT_CONFIG = {
        'verbosity': 2,
        'continue': True,
        'dep_file': os.path.join(SCRIPT_HOME, '.%s.db'%SCRIPT_NAME),
        'num_process': 1,
        }

    def get_title(task):
        return "testing... %s" % task.name

    def __init__(self):
        import argparse

        self.testDB = OrderedDict()

        parser = argparse.ArgumentParser(description='Perform KGEN tests.')
        parser.add_argument('tests', type=str, nargs='*', help='Specify tests.')
        parser.add_argument('-f', dest='func_tests', action='store_true', default=False, help='Functional test only.')
        parser.add_argument('-s', dest='sys_tests', action='store_true', default=False, help='System test only.')
        parser.add_argument('-c', dest='changed', action='store_true', default=False, help='Changed test only.')
        parser.add_argument('-k', dest='kgen', type=str, default='%s/bin/kgen'%KGEN_HOME, help='Default KGEN command.')
        parser.add_argument('-t', dest='leavetemp', action='store_true', default=False, help='Leave temporary directory.')
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
            if relpath.startswith('old'): continue
            if relpath.endswith('templates'):
                del subdirList[:]
                continue

            # if TEST_SCRIPT exists in a directory
            if TEST_SCRIPT in fileList:
                pathsave = sys.path
                sys.path.insert(0, dirName)
                mod = __import__(TEST_SCRIPT[:-3])

                test_found = False

                # find classes inherited from KGenTest class
                match = lambda x: inspect.isclass(x) and x not in [ KGenTest, KExtTest, KCoverTest ] and issubclass(x, KGenTest)
                for name, cls in inspect.getmembers(mod, match):
                    # process module level preparation
                    if not test_found:
                        print('Adding Test: %s' % relpath)
                        test_found = True

                    #  generate test object
                    obj = cls()
                    obj.taskid += 1
                    obj.TEST_SCRIPT = TEST_SCRIPT
                    obj.TEST_DIR = dirName
                    obj.KGEN = args.kgen
                    obj.COMPILER = args.compiler
                    obj.COMPILER_FLAGS = args.compiler_flags
                    obj.LEAVE_TEMP = args.leavetemp
                    obj.task = {}
                    obj.test = {}

                    testname = '%s/%s'%(relpath, name)
                    self.testDB[testname] = obj
                    
                    # process class level preparation
                    obj.configure_test()

                    if not obj.task.has_key('name'): obj.task['name'] = testname
                    if not obj.task.has_key('uptodate'): obj.task['uptodate'] = [ False ]
                    #if not obj.task.has_key('title'): obj.task['title'] = (self.get_title, None)
                    if not obj.task.has_key('actions'): obj.task['actions'] = [ 'echo "WARNING: No test is configured."' ]

                    if not obj.test.has_key('result'): obj.test['result'] = False
                    if not obj.test.has_key('detail'): obj.test['detail'] = 'Unknown'

                    # add the class in a test list

                if not test_found: sys.path = pathsave

    # def download build ref execution for homme, cesm, ....

    def testtask(self):

        task = {}
        task['name'] = 'testtask'
        task['uptodate'] = [False]
        task['actions'] = ['pwd']
        return task

    def report(self):
        ntests = len(self.testDB)
        npassed = len([ obj for obj in self.testDB.values() if obj.test['result'] ])
        nfailed = ntests - npassed

        print ''
        print '*********** TEST REPORT ***********'
        print ''
        print 'Total # of tests : %d'%ntests
        print '# of passed tests: %d'%npassed
        print '# of failed tests: %d'%nfailed
        print ''

        for testname, testobj in self.testDB.iteritems():
            if testobj.test['result']:
                pass
                #print '%s : PASSED'%testname
            else:
                print '%s : FAILED'%testname
                print 'ERROR MSG:'
                print testobj.test['detail']
                print ''
        
        print '***********************************'

    def gentask_report(self, deptasks):

        task = {}
        task['name'] = 'generate_report'
        task['task_dep'] = deptasks
        task['actions'] = [(self.report, None, None)]
        return task

    def _gen_tasks(self):
        # generate test tasks
        testnames = []
        for testname, testobj in self.testDB.iteritems():
            testnames.append('KGEN_TESTS:%s'%testname)
            yield testobj._gentask()
        yield self.gentask_report(testnames)

    def load_tasks(self, cmd, params, args):
        """implements loader interface, return (tasks, config)"""
        return generate_tasks('KGEN_TESTS', self._gen_tasks()), self.DOIT_CONFIG

if __name__ == "__main__":
    doit_main = DoitMain(KGenTestTaskLoader())
    sys.exit(doit_main.run(['run']))
