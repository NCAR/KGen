# runtest.py
# 
import os
from kapp_func_ch_multiline_typedecls_test import KAppFuncCHMLTTest

class CustomTest(KAppFuncCHMLTTest):

    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel pgi'
        result[myname]['prerun_run'] = 'module swap intel pgi'

        self.set_status(result, myname, self.PASSED)

        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']

        prerun_build = result['config_task']['prerun_build']
        prerun_run = result['config_task']['prerun_run']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            None, \
            __invocation='0:0:0', \
            __cmd_clean='"cd %s; make clean FC=pgfortran"'%tmpsrc, \
            __cmd_build='"cd %s; make build FC=pgfortran"'%tmpsrc, \
            __cmd_run='"cd %s; make run FC=pgfortran"'%tmpsrc, \
            __rebuild='all', \
            __prerun='build="%s",run="%s"'%(prerun_build, prerun_run), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.0.0.0']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result


if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
