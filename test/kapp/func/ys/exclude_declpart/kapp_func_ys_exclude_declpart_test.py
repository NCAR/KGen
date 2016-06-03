import os
from kapp_func_ys_test import KAppFuncYSTest

class KAppFuncYSEDPTest(KAppFuncYSTest):
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
            _e='%s/exclude.ini'%workdir, \
            __rebuild='all', \
            __prerun='build="%s",run="%s"'%(prerun_build, prerun_run), \
            __invocation='0:0:0', \
            __outdir=workdir)

            #__prerun='kernel_build="%s",kernel_run="%s"'%(prerun_build, prerun_run), \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.0.0.0']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result
