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

        # parser bug workaround
        shutil.copy("%s/micro_mg_utils.F90"%here, "%s/components/cam/src/physics/cam"%tmpsrc)

        srcfile = '%s/components/cam/src/physics/cam/micro_mg_cam.F90'%tmpsrc
        namepath = 'micro_mg_cam:micro_mg_cam_tend_pack:micro_mg_tend2_0'
        fc = 'ifort'
        fc_flags = '-no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2'
        prerun_cmds = ';'.join(result['config_task']['prerun_kernel'])
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            __intrinsic='skip,except=shr_spfn_mod:shr_spfn_gamma_nonintrinsic_r8:sum', \
            __cmd_clean='"cd %s; ./case.build --clean"'%casedir, \
            __cmd_build='"cd %s; ./case.build"'%casedir, \
            __cmd_run='"cd %s; ./case.submit"'%casedir, \
            __mpi='comm=mpicom,use="spmd_utils:mpicom",header="/glade/u/apps/opt/intel/2017u1/impi/2017.1.132/intel64/include/mpif.h"', \
            __openmp='enable', \
            __add_cache_pollution='1024', \
            __prerun='build="%s",run="%s"'%(prerun_cmds, prerun_cmds), \
            __outdir=workdir)

            #__kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            #__cmd_build='"cd %s; qcmd -q premium -- ./case.build"'%casedir, \
            #__cmd_build='"cd %s; ./case.build"'%casedir, \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('micro_mg_tend2_0.*.*.*')
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
