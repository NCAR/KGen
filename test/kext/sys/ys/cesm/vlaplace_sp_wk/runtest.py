# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ys_cesm_test import KExtSysYSCesmTest


class Test(KExtSysYSCesmTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']

        camsrcmods = '%s/src.cam'%srcmods
        result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/dynamics/se/share/viscosity_mod.F90'%tmpsrc
        namepath = 'viscosity_mod:biharmonic_wk_dp3d:laplace_sphere_wk'
        fc_flags = '-no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='include.ini', \
            _e='exclude.ini', \
            __invocation='1:10:30', \
            __timing='repeat=100', \
            __mpi='ranks=0:10:100:500,comm=mpicom,use="spmd_utils:mpicom"', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['laplace_sphere_wk.1.0', 'laplace_sphere_wk.1.10', 'laplace_sphere_wk.1.100', 'laplace_sphere_wk.1.500', \
                'laplace_sphere_wk.10.0', 'laplace_sphere_wk.10.10', 'laplace_sphere_wk.10.100', 'laplace_sphere_wk.10.500', \
                'laplace_sphere_wk.30.0', 'laplace_sphere_wk.30.10', 'laplace_sphere_wk.30.100', 'laplace_sphere_wk.30.500' ]
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
