import sys
from kapp_sys_ys_calc_calc_mpi_openmp_test import KAppSysYSCalcCMOTest
import time

class Test(KAppSysYSCalcCMOTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        prerun = 'module swap intel intel/16.0.1; module try-load impi/5.0.1.035'

        passed, out, err = self.extract_kernel(srcfile, namepath, \
            '"cd %s; make -f Makefile.mpirun clean"'%tmpsrc, \
            '"cd %s; make -f Makefile.mpirun build"'%tmpsrc, \
            '"cd %s; make -f Makefile.mpirun run"'%tmpsrc, \
            __invocation='0-1:0-1:1,0-1:2-3:3', \
            __timing='repeat=1', \
            __prerun='build="%s",run="%s"'%(prerun, prerun), \
            __mpi='enable', \
            __rebuild='all', \
            __openmp='enable', \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            result[myname]['statefiles'] = ['calc.0.0.1', 'calc.0.1.1', 'calc.0.2.3', 'calc.0.3.3', \
                'calc.1.0.1', 'calc.1.1.1', 'calc.1.2.3', 'calc.1.3.3']
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
