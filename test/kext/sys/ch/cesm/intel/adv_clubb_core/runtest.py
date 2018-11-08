# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ch_cesm_intel_test import KExtSysCHCesmIntelTest

here = os.path.dirname(__file__)

class Test(KExtSysCHCesmIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']

        result[myname]['datadir'] = '%s/data'%workdir

        camsrcmods = '%s/src.cam'%srcmods
        result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/physics/cam/clubb_intr.F90'%tmpsrc
        namepath = 'clubb_intr:clubb_tend_cam:advance_clubb_core_api'
        fc = 'ifort'
        fc_flags = '-qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -qopt-report -xCORE_AVX2 -no-fma -O2 -debug minimal -free -mkl'
        prerun_cmds = ';'.join(result['config_task']['prerun_kernel'])
        passed, out, err = self.extract_kernel(srcfile, namepath, \
            __source='format=free,strict=no,alias=/glade/scratch/youngsun:/glade/u/home/youngsun/trepo/temp', \
            __mpi='comm=mpicom,use="spmd_utils:mpicom"', \
            __openmp='enable', \
            __cmd_clean='"cd %s; ./case.build --clean"'%casedir, \
            __cmd_build='"cd %s; qcmd -q premium -- ./case.build"'%casedir, \
            __cmd_run='"cd %s; ./case.submit"'%casedir, \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __prerun='build="%s",run="%s"'%(prerun_cmds, prerun_cmds), \
            __outdir=workdir)

            #_e='exclude.ini', \
            #_i='%s/include.ini'%here, \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('advance_clubb_core.*.*.*')
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def replace(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        srcmods = result['config_task']['srcmods']
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
