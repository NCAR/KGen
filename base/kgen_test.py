# kgen_test.py

import subprocess

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
            result['general']['errmsg'].append(errmsg)
        elif status==self.NOT_EXECUTED:
            pass
        else:
            raise Exception('Unknown status: %s'%status)

    def perform_test(self):

        result = { 'general': {'passed': False, 'errmsg': []} }

        self.task_map = \
            [ ('prep_task', self.preprocess), ('mkdir_task', self.mkworkdir), ('download_task', self.download), \
            ('extract_task', self.extract), ('replace_task', self.replace), ('config_task', self.config), \
            ('build_task', self.build), ('recover_task', self.recover), ('genstate_task', self.genstate), \
            ('runkernel_task', self.runkernel), ('verify_task', self.verify), ('savestate_task', self.savestate), \
            ('rmdir_task', self.rmdir), ('postp_task', self.postprocess), ('_finalize_task', self._finalize) ]

        for taskname, taskfunc in self.task_map:

            result[taskname] = {}
            result[taskname]['status'] = self.NOT_EXECUTED
            result[taskname]['errmsg'] = '%s is not performed.'%taskname

            result = taskfunc(taskname, result)

        return result

    def is_uptodate(self, taskname):
        return self.uptodate[taskname]

    def run_shcmd(self, cmd, input=None, **kwargs):
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
        return proc.communicate(input=input)

    def preprocess(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def mkworkdir(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def download(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def extract(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def replace(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def config(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def build(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def recover(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def genstate(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def runkernel(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def verify(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def savestate(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def rmdir(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def postprocess(self, myname, result):
        result[myname]['status'] = self.PASSED
        return result

    def _finalize(self, myname, result):
        result[myname]['status'] = self.PASSED
        errmsg = []
        is_passed = True
        for taskname in [ name for name, func in self.task_map ]:
            if result[taskname]['status'] not in [ self.PASSED ]:
                is_passed = False
                errmsg.append(result[taskname]['errmsg'])
        result['general']['errmsg'] = errmsg
        result['general']['passed'] = is_passed

        if is_passed: print('PASSED')
        else: print('FAILED')
        return result
