# runtest.py
# 
import os
from kext_func_ys_multiline_typedecls_test import KExtFuncYSMLTTest

class CustomTest(KExtFuncYSMLTTest):
    def config(self, myname, result):

        result[myname]['FC'] = 'gfortran'
        result[myname]['FC_FLAGS'] = ''
        result[myname]['PRERUN'] = 'module purge; module try-load ncarenv/1.0; module try-load ncarbinlibs/1.1; module try-load ncarcompilers/1.0; module try-load gnu/5.3.0'

        self.set_status(result, myname, self.PASSED)

        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']
        FC = result['config_task']['FC']
        FC_FLAGS = result['config_task']['FC_FLAGS']
        PRERUN = result['config_task']['PRERUN']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            None, _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __invocation='0:0:0', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s",PRERUN="%s"'%(FC, FC_FLAGS, PRERUN), \
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
