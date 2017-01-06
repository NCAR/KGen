from kapp_func_ys_manual_state_gen_test import KAppFuncYSMSGTest

import os

class CustomTest(KAppFuncYSMSGTest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel intel/16.0.1'
        result[myname]['prerun_run'] = 'module swap intel intel/16.0.1'

        self.set_status(result, myname, self.PASSED)

        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']

        prerun_build = result['config_task']['prerun_build']
        prerun_run = result['config_task']['prerun_run']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', \
            __cmd_clean='"cd %s; make clean"'%tmpsrc, \
            __cmd_build='"cd %s; make build"'%tmpsrc, \
            __cmd_run='"cd %s; make run "'%tmpsrc, \
            __prerun='build="%s",run="%s"'%(prerun_build, prerun_run), \
            __invocation='0:0:0,0:0:2', \
            __source='state=%s/kernel.F90'%tmpsrc, \
            __outdir=workdir)

            #__prerun='kernel_build="%s",kernel_run="%s"'%(prerun_build, prerun_run), \
            #__rebuild='all', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.0.0.0', 'add.0.0.2']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
