# kgen_test.py

from __future__ import print_function
import sys
#import subprocess

class KGenTest(object):

    (NOT_EXECUTED, FAILED, PASSED) = range(3)
    TEST_NUM = 0


    def get_tasks(self):
        for taskname, taskfunc in self.task_map:
            yield self.tasks[taskname]

    def get_result(self, key, result_type='general'):
        return self.result[result_type][key]

    def set_status(self, result, taskname, status, errmsg=''):
        result[taskname]['status'] = status
        if status==self.PASSED:
            pass
        elif status==self.FAILED:
            result[taskname]['errmsg'] = errmsg
        elif status==self.NOT_EXECUTED:
            pass
        else:
            raise Exception('Unknown status: %s'%status)

    def perform_test(self):

        result = { 'general': {'passed': False, 'errmsg': [], 'mandatory_tasks': ['verify_task']} }

        self.task_map = \
            [ ('prep_task', self.preprocess), ('mkdir_task', self.mkworkdir), ('download_task', self.download), \
            ('config_task', self.config), ('generate_task', self.generate), ('replace_task', self.replace), \
            ('build_task', self.build), ('recover_task', self.recover), ('genstate_task', self.genstate), \
            ('runkernel_task', self.runkernel), ('verify_task', self.verify), ('savestate_task', self.savestate), \
            ('rmdir_task', self.rmdir), ('postp_task', self.postprocess), ('_finalize_task', self._finalize) ]

        for taskname, taskfunc in self.task_map:
            result[taskname] = {}
            result[taskname]['status'] = self.NOT_EXECUTED
            result[taskname]['errmsg'] = ''
            #result[taskname]['errmsg'] = '%s is not performed.'%taskname

        for taskname, taskfunc in self.task_map:
            print('.', end='')
            sys.stdout.flush()

            if 'goto' in result and result['goto'] and taskname != result['goto']:
                self.set_status(result, taskname, self.PASSED)
                continue
            result['goto'] = None

            result = taskfunc(taskname, result)

            is_passed = True
            if taskname in result['general']['mandatory_tasks']:
                if result[taskname]['status'] == self.NOT_EXECUTED:
                    result[taskname]['errmsg'] = 'Mandatory task, "%s", is not executed.'%taskname
                    is_passed = False
                elif result[taskname]['status'] != self.PASSED:
                    is_passed = False
            elif result[taskname]['status'] not in [ self.NOT_EXECUTED, self.PASSED ]:
                is_passed = False

            if not is_passed:
                if taskname !='_finalize_task':
                    result = self._finalize('_finalize_task', result)
                break

            if taskname==self.STOP_AT:
                print ('Test is stopped at', taskname)
                break

        return result

    def is_uptodate(self, taskname):
        return self.uptodate[taskname]

#    def run_shcmd(self, cmd, input=None, **kwargs):
#        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
#            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
#        out, err = proc.communicate(input=input)
#        return out, err, proc.returncode

    def preprocess(self, myname, result):
        result[myname]['status'] = self.NOT_EXECUTED
        return result

    def mkworkdir(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def download(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def config(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def generate(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def replace(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def build(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def recover(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def genstate(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def runkernel(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def verify(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def savestate(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def rmdir(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def postprocess(self, myname, result):
        self.set_status(result, myname, self.NOT_EXECUTED)
        return result

    def _finalize(self, myname, result):
        self.set_status(result, myname, self.PASSED)
        errmsg = []
        is_passed = True
        for taskname in [ name for name, func in self.task_map ]:
            if taskname in result['general']['mandatory_tasks']:
                if result[taskname]['status'] == self.NOT_EXECUTED or \
                    result[taskname]['status'] != self.PASSED:
                    is_passed = False
            elif result[taskname]['status'] not in [ self.NOT_EXECUTED, self.PASSED ]:
                is_passed = False

            if not is_passed:
                if result[taskname]['errmsg']:
                    errmsg.append('%s: %s'%(taskname, result[taskname]['errmsg']))
        result['general']['errmsg'] = errmsg
        result['general']['passed'] = is_passed

        if is_passed: print('PASSED')
        else: print('FAILED')
        return result
