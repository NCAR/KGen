import os
import sys
import glob
import shutil
from kapp_sys_ys_mpas_intel_test import KAppSysYSMpasIntelTest

class Test(KAppSysYSMpasIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        coredir = '%s/core_atmosphere'%tmpsrc
        initdir = '%s/init_atmosphere'%tmpsrc
        rundir = '%s/run'%tmpsrc
        srcfile = '%s/src/core_atmosphere/dynamics/mpas_atm_time_integration.F'%coredir

        if os.path.exists(os.path.join(self.TEST_DIR, 'mpas_atm_time_integration.F')):
            shutil.copy(srcfile, workdir)
            shutil.copyfile(os.path.join(self.TEST_DIR, 'mpas_atm_time_integration.F'), srcfile)

        #prerun_krun = 'export LD_LIBRARY_PATH=/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/mkl/lib/intel64_lin:/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/compiler/lib/intel64_lin:\${LD_LIBRARY_PATH}'

        passed, out, err = self.extract_kernel(srcfile, None, \
            __cmd_clean='"cd %s; ./clean_core.sh"'%tmpsrc, \
            __cmd_build='"cd %s; ./build_core_intel.sh"'%tmpsrc, \
            __cmd_run='"cd %s; bsub < run_core_intel.sh"'%tmpsrc, \
            __invocation='0:0:5,e/2:0:5,e:0:5,0:0:10,e/2:0:10,e:0:10', \
            _e='%s/exclude.ini'%workdir, \
            __timing='repeat=1', \
            __mpi='enable', \
            __openmp='enable', \
            __outdir=workdir)
            #__rebuild='all',
            #__kernel_compile='PRERUN="module load intel/16.0.2"',
            #__debug='printvar=:i,:j,:output',
            #__prerun='kernel_run="%s"'%prerun_krun, \
            #__invocation='0:0:5,e/2:0:5,e:0:5,0:e/2:5,e/2:e/2:5,e:e/2:5,0:e:5,e/2:e:5,e:e:5,0:0:10,e/2:0:10,e:0:10,0:e/2:10,e/2:e/2:10,e:e/2:10,0:e:10,e/2:e:10,e:e:10', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if os.path.exists(os.path.join(workdir, 'mpas_atm_time_integration.F')):
            shutil.copyfile(os.path.join(workdir, 'mpas_atm_time_integration.F'), srcfile)

        if passed:
            result[myname]['statefiles'] = ['acoustics_rrpk.0.0.10', 'acoustics_rrpk.0.0.5', 'acoustics_rrpk.31.0.10', \
                'acoustics_rrpk.31.0.5', 'acoustics_rrpk.63.0.10', 'acoustics_rrpk.63.0.5']
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

