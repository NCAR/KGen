# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ch_mpas_intel_test import KExtSysCHMpasIntelTest

here = os.path.dirname(__file__)

class Test(KExtSysCHMpasIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        tmprun = result['download_task']['tmprun']

        srcfile = '%s/src/core_atmosphere/dynamics/mpas_atm_time_integration.F'%tmpsrc
        namepath = 'atm_time_integration:atm_compute_dyn_tend:atm_compute_dyn_tend_work'
        fc = 'ifort'
        fc_flags = '-O3 -convert big_endian -FR'
        prerun_kernel_cmds = ';'.join(result['config_task']['prerun_kernel'])
        prerun_app_cmds = ';'.join(result['config_task']['prerun_app'])
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _e='%s/exclude.ini'%here, \
            __cmd_clean='"cd %s; %s; make clean CORE=atmosphere"'%(tmpsrc, prerun_app_cmds), \
            __cmd_build='"cd %s; %s; ulimit -s unlimited;  make ifort CORE=atmosphere PRECISION=single USE_PIO2=true"'%(tmpsrc, prerun_app_cmds), \
            __cmd_run='"cd %s; qsub execute.sh"'%tmprun, \
            __mpi='enable', \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __prerun='build="%s",run="%s"'%(prerun_app_cmds, prerun_app_cmds), \
            __outdir=workdir)

            #__openmp='enable', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('atm_compute_dyn_tend_work.*.*.*')
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def replace(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        camsrcmods = result['generate_task']['camsrcmods']
#
#        out, err, retcode = self.run_shcmd('rm -f *', cwd=camsrcmods)
#
#        for instrumented in glob.glob('%s/state/*.F90'%workdir):
#            shutil.copy2(instrumented, camsrcmods)
#            
#        self.set_status(result, myname, self.PASSED)
#
#        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
