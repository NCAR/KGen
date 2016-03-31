# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ys_homme_test import KExtSysYSHommeTest


class Test(KExtSysYSHommeTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']

        camsrcmods = '%s/src.cam'%srcmods
        result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/physics/cam/micro_mg_cam.F90'%tmpsrc
        namepath = 'micro_mg_cam:micro_mg_cam_tend:micro_mg_tend2_0'
        fc_flags = '-no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='include.ini', \
            __invocation='10:50:100', \
            __timing='repeat=1', \
            __intrinsic='skip,except=shr_spfn_mod:shr_spfn_gamma_nonintrinsic_r8:sum', \
            __mpi='ranks=0:100:300,comm=mpicom,use="spmd_utils:mpicom"', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['micro_mg_tend2_0.10.0', 'micro_mg_tend2_0.10.100', 'micro_mg_tend2_0.10.300', \
                'micro_mg_tend2_0.50.0', 'micro_mg_tend2_0.50.100', 'micro_mg_tend2_0.50.300', \
                'micro_mg_tend2_0.100.0', 'micro_mg_tend2_0.100.100', 'micro_mg_tend2_0.100.300']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

    def replace(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        camsrcmods = result['generate_task']['camsrcmods']

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
