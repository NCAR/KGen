# runtest.py
# 
import os
import sys
import glob
import shutil
from kgutils import run_shcmd
from kext_sys_ch_wrf_intel_test import KExtSysCHWrfIntelTest

here = os.path.abspath(os.path.dirname(__file__))

class Test(KExtSysCHWrfIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        jobscript = result['download_task']['jobscript']

        srcfile = '%s/phys/module_radiation_driver.f90'%tmpsrc
        namepath = 'module_radiation_driver:radiation_driver:RRTMG_LWRAD'
        fc = 'ifort'
        fc_flags = '-O3 -ip -fp-model precise -w -ftz -align all -fno-alias -FR -convert big_endian -xHost -fp-model fast=2 -no-heap-arrays -no-prec-div -no-prec-sqrt -fno-common -xCORE-AVX2'
        prerun_cmds = ';'.join(result['config_task']['prerun'])
        prerun_kernel_cmds = ';'.join(result['config_task']['prerun_kernel'])

        rundir = "%s/run"%tmpsrc
        src = "%s/../../submit.sh"%here
        dst = "%s/submit.sh"%rundir
        run_shcmd('sed "s,WORKDIR,%s," %s > %s'%(rundir, src, dst), cwd=tmpsrc)

        passed, out, err = self.extract_kernel(srcfile, namepath, \
            _e='"%s"'%os.path.join(here, "exclude.ini"), \
            __cmd_clean='"cd %s; ./clean"'%tmpsrc, \
            __cmd_build='"cd %s; ./compile em_real"'%tmpsrc, \
            __cmd_run='"cd %s; qsub %s/submit.sh"'%(tmpsrc, jobscript), \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __prerun='build="%s",run="%s"'%(prerun_cmds, prerun_cmds), \
            __outdir=workdir)

            #_e='exclude.ini', \
            #_i='%s/include.ini'%here, \
            #__source='format=free,strict=no,alias=/glade/scratch/youngsun:/glade/u/home/youngsun/trepo/temp', \
            #__mpi='comm=mpicom,use="spmd_utils:mpicom"', \
            #__openmp='enable', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('rrtmg_lwrad.*.*.*')
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
