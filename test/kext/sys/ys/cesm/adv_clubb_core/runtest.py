# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ys_cesm_test import KExtSysYSCesmTest


class Test(KExtSysYSCesmTest):

    def extract(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']

        camsrcmods = '%s/src.cam'%srcmods
        result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/physics/cam/clubb_intr.F90'%tmpsrc
        namepath = 'clubb_intr:clubb_tend_cam:advance_clubb_core'
        fc_flags = '-no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2 -mkl'
        passed, out, err = self.extract_kernel(srcfile, namepath, \
            _i='include.ini', \
            __invocation='"10:50"', \
            __source='format=free,strict=no,alias=/glade/scratch/youngsun:/glade/u/home/youngsun/trepo/temp', \
            __timing='repeat=1', \
            __mpi='ranks=100:300:500,comm=mpicom,use="spmd_utils:mpicom"', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

            #_e='exclude.ini', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['advance_clubb_core.10.100', 'advance_clubb_core.10.300', 'advance_clubb_core.10.500', \
                'advance_clubb_core.50.100', 'advance_clubb_core.50.300', 'advance_clubb_core.50.500' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

    def replace(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        srcmods = result['config_task']['srcmods']
        camsrcmods = result['extract_task']['camsrcmods']

        out, err, retcode = self.run_shcmd('rm -f *', cwd=camsrcmods)

        for instrumented in glob.glob('%s/state/*.F90'%workdir):
            shutil.copy2(instrumented, camsrcmods)
            
        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
