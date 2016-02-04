# kgen_test.py

import subprocess
from doit.tools import set_trace

class KGenTest(object):

    (NOT_EXECUTED, FAILED, PASSED) = range(3)
    TEST_NUM = 0


    def get_tasks(self):
        for taskname, taskfunc in self.task_map:
            yield self.tasks[taskname]

    def get_result(self, key, result_type='general'):
        return self.result[result_type][key]

    def set_status(self, taskname, status, errmsg=''):
        self.result[taskname]['status'] = status
        if status==self.PASSED: pass
        elif status==self.FAILED:
            self.result['general']['errmsg'].append(errmsg)
        elif status==self.NOT_EXECUTED: pass
        else:
            raise Exception('Unknown status: %s'%status)

        print 'YYY: ', taskname, id(self.result['general']), id(self.result['general']['errmsg'])

    def configure_test(self, task_dep=[]):

        self.result = { 'general': {'passed': False, 'errmsg': []} }
        self.tasks = {}
        self.uptodate = {}

        self.task_map = \
            [ ('prep_task', self.preprocess), ('mkdir_task', self.mkworkdir), ('download_task', self.download), \
            ('extract_task', self.extract), ('replace_task', self.replace), ('config_task', self.config), \
            ('build_task', self.build), ('recover_task', self.recover), ('genstate_task', self.genstate), \
            ('runkernel_task', self.runkernel), ('verify_task', self.verify), ('savestate_task', self.savestate), \
            ('rmdir_task', self.rmdir), ('postp_task', self.postprocess), ('_finalize_task', self._finalize) ]

        self.result['prep_task'] = { 'status': self.NOT_EXECUTED }
        self.result['prep_task']['errmsg'] = 'prep_task is not performed.'
        self.uptodate['prep_task'] = False

        self.prep_task = {
            'name': '%s.prep_task'%self.TEST_ID,
            'task_dep': task_dep,
            'uptodate': [ self.is_uptodate('prep_task') ],
            'actions': [ (self.preprocess, [ 'prep_task'], None) ]
        }
        self.tasks['prep_task'] = self.prep_task

        ntasks = len(self.task_map)
        for (t1name, t1func), (t2name, t2func) in zip(self.task_map[0:(ntasks-1)], self.task_map[1:ntasks]):

            self.uptodate[t2name] = False

            self.result[t2name] = {}
            self.result[t2name]['status'] = self.NOT_EXECUTED
            self.result[t2name]['errmsg'] = '%s is not performed.'%t2name

            setattr(self, t2name, {})
            task2 = getattr(self, t2name)
            task2['name'] = '%s.%s'%(self.TEST_ID, t2name)
            task2['task_dep'] = ['%s:%s'%(self.TEST_PREFIX, getattr(self, t1name)['name'])]
            task2['uptodate'] = [ self.is_uptodate(t2name) ]
            task2['actions'] = [ ( t2func, [ t2name ], None ) ]

            self.tasks[t2name] = task2

        self.result['_finalize_task'] = { 'status': self.NOT_EXECUTED }
        self.result['_finalize_task']['errmsg'] = '_finalize_task is not performed.'
        self.uptodate['_finalize_task'] = False

        self._finalize_task = {
            'name': '%s._finalize_task'%self.TEST_ID,
            'task_dep': ['%s:%s'%(self.TEST_PREFIX, getattr(self, 'postp_task')['name'])],
            'uptodate': [ False ],
            'actions': [ (self._finalize, [ '_finalize_task'], None) ]
        }
        self.tasks['_finalize_task'] = self._finalize_task

    def is_uptodate(self, taskname):
        return self.uptodate[taskname]

    def run_shcmd(self, cmd, input=None, **kwargs):
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
        return proc.communicate(input=input)

    def preprocess(self, myname):
        self.result[myname]['status'] = self.PASSED

    def mkworkdir(self, myname):
        self.result[myname]['status'] = self.PASSED

    def download(self, myname):
        self.result[myname]['status'] = self.PASSED

    def extract(self, myname):
        self.result[myname]['status'] = self.PASSED

    def replace(self, myname):
        self.result[myname]['status'] = self.PASSED

    def config(self, myname):
        self.result[myname]['status'] = self.PASSED

    def build(self, myname):
        self.result[myname]['status'] = self.PASSED

    def recover(self, myname):
        self.result[myname]['status'] = self.PASSED

    def genstate(self, myname):
        self.result[myname]['status'] = self.PASSED

    def runkernel(self, myname):
        self.result[myname]['status'] = self.PASSED

    def verify(self, myname):
        self.result[myname]['status'] = self.PASSED

    def savestate(self, myname):
        self.result[myname]['status'] = self.PASSED

    def rmdir(self, myname):
        self.result[myname]['status'] = self.PASSED

    def postprocess(self, myname):
        self.result[myname]['status'] = self.PASSED

    def _finalize(self, myname):
        self.result[myname]['status'] = self.PASSED
        errmsg = []
        is_passed = True
        for taskname in [ name for name, func in self.task_map ]:
            if self.result[taskname]['status'] not in [ self.PASSED ]:
                is_passed = False
                errmsg.append(self.result[taskname]['errmsg'])
        self.result['general']['errmsg'] = errmsg
        self.result['general']['passed'] = is_passed
