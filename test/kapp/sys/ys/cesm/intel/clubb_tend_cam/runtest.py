import os
import sys
import glob
import shutil
from kapp_sys_ys_cesm_intel_test import KAppSysYSCesmIntelTest


class Test(KAppSysYSCesmIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']

        srcfile = '%s/components/cam/src/physics/cam/clubb_intr.F90'%tmpsrc

        if os.path.exists(os.path.join(self.TEST_DIR, 'clubb_intr.F90')):
            shutil.copy(srcfile, workdir)
            shutil.copyfile(os.path.join(self.TEST_DIR, 'clubb_intr.F90'), srcfile)

        prerun_krun = 'export LD_LIBRARY_PATH=/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/mkl/lib/intel64_lin:/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/compiler/lib/intel64_lin:\${LD_LIBRARY_PATH}'

        passed, out, err = self.extract_kernel(srcfile, None, \
            __cmd_clean='"cd %s; ./case.clean_build all"'%casedir, \
            __cmd_build='"cd %s; ./case.build"'%casedir, \
            __cmd_run='"cd %s; ./case.submit"'%casedir, \
            __invocation='100:0-1:10,100:0-1:50,300:0-1:10,300:0-1:50,500:0-1:10,500:0-1:50', \
            __kernel_option='-mkl=link', \
            _e='%s/exclude.ini'%workdir, \
            __prerun='kernel_run="%s"'%prerun_krun, \
            __timing='repeat=1', \
            __mpi='comm=mpicom,use="spmd_utils:mpicom"', \
            __openmp='enable', \
            __outdir=workdir)
            #__rebuild='all',
            #__kernel_compile='PRERUN="module load intel/16.0.2"',
            #__debug='printvar=:i,:j,:output',
            #__mpi='comm=mpicom,use="spmd_utils:mpicom",header="/ncar/opt/intel/12.1.0.233/impi/4.0.3.008/intel64/include/mpif.h"', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if os.path.exists(os.path.join(workdir, 'clubb_intr.F90')):
            shutil.copyfile(os.path.join(workdir, 'clubb_intr.F90'), srcfile)

        if passed:
            result[myname]['statefiles'] = ['clubb.100.0.10', 'clubb.100.1.10', 'clubb.100.0.50', \
                'clubb.100.1.50', 'clubb.300.0.10', 'clubb.300.1.10', 'clubb.300.0.50', \
                'clubb.300.1.50', 'clubb.500.0.10', 'clubb.500.1.10', 'clubb.500.0.50', \
                'clubb.500.1.50' ]
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

