import sys
from kapp_sys_ys_calc_calc_mpi_stategen_test import KAppSysYSCalcCMSTest
import time

class Test(KAppSysYSCalcCMSTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/bridge_mod.F90'%tmpsrc
        prerun = 'module swap intel intel/16.0.1; module try-load impi/5.0.1.035'

        passed, out, err = self.extract_kernel(srcfile, None, \
            __cmd_clean='"cd %s; make -f Makefile.mpirun clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile.mpirun build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile.mpirun run"'%tmpsrc, \
            __invocation='0-1:0:1,2-3:0:3', \
            __timing='repeat=1', \
            __prerun='build="%s",run="%s"'%(prerun, prerun), \
            __mpi='enable', \
            __rebuild='state', \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            result[myname]['statefiles'] = ['update.0.0.1', 'update.1.0.1', 'update.2.0.3', 'update.3.0.3' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
