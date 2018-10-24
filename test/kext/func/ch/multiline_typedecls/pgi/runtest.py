# runtest.py
# 
import os
import glob
from kext_func_ch_multiline_typedecls_test import KExtFuncCHMLTTest

class CustomTest(KExtFuncCHMLTTest):
    def config(self, myname, result):

        result[myname]['FC'] = 'pgfortran'
        result[myname]['FC_FLAGS'] = ''
        result[myname]['PRERUN'] = 'module purge; module try-load ncarenv/1.2; module try-load ncarcompilers/0.4.1; module try-load pgi/17.9; module try-load netcdf/4.4.1.1; module try-load mpt/2.15f'

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
            __cmd_clean='"cd %s; make -f Makefile clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile run"'%tmpsrc, \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(FC, FC_FLAGS), \
            __prerun='build="%s",run="%s"'%(PRERUN, PRERUN), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob(workdir+"/kernel/add.*.*.*")
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result


if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
