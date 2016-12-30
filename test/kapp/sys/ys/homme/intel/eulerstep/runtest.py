# runtest.py
# 
import os
import sys
import glob
import shutil
from kapp_sys_ys_homme_intel_test import KAppSysYSHommeIntelTest


class Test(KAppSysYSHommeIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        blddir = result['config_task']['blddir']
        rundir = result['config_task']['rundir']

        srcfile = '%s/src/share/prim_advection_mod.F90'%tmpsrc
        namepath = 'prim_advection_mod:euler_step_driver:euler_step'
        prerun_build = ';'.join(result['config_task']['prerun_build'])
        prerun_run = ';'.join(result['config_task']['prerun_run'])
        passed, out, err = self.extract_kernel(srcfile, namepath, \
            __cmd_clean='"cd %s; make clean"'%blddir, \
            __cmd_build='"cd %s; %s; make -j 8 perfTestWACCM"'%(blddir, prerun_build), \
            __cmd_run='"cd %s; bsub < homme.submit"'%rundir, \
            __prerun='kernel_build="%s",kernel_run="%s"'%(prerun_build, prerun_run), \
            __exclude='%s/exclude.ini'%workdir, \
            __invocation='0:0:10,0:0:50,10:0:10,10:0:50', \
            __timing='repeat=1', \
            __mpi='enable', \
            __openmp='enable,kernel-in-critical-region=no', \
            __outdir=workdir)

            #__cmd_build='"cd %s; %s; make -j 8 perfTest"'%(blddir, prerun_build), \

        result[myname]['datadir'] = '%s/data'%workdir
        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['euler_step.0.0.10', 'euler_step.0.0.50', \
                'euler_step.10.0.10', 'euler_step.10.0.50']
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
