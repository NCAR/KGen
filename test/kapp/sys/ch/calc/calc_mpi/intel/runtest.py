import sys
import os
from kapp_sys_ch_calc_calc_mpi_test import KAppSysCHCalcCMPTest
import time
import glob

class Test(KAppSysCHCalcCMPTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        kerneldir = workdir+ "/kernel" 
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        prerun = 'module swap intel intel/17.0.1; module swap mpt impi'

        passed, out, err = self.extract_kernel(srcfile, namepath, \
            __cmd_clean='"cd %s; make -f Makefile.mpirun clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile.mpirun build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile.mpirun run"'%tmpsrc, \
            __timing='repeat=1', \
            __prerun='build="%s",run="%s"'%(prerun, prerun), \
            __mpi='enable', \
            __rebuild='state', \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',
            #__invocation='0-1:0:1,2-3:0:3', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            result[myname]['statefiles'] = []
            for dfile in glob.glob('%s/calc.*.*.*'%kerneldir):
                result[myname]['statefiles'].append(os.path.basename(dfile))
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
