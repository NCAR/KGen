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
            __invocation='0:0:1,0:0:10,0:0:30,10:0:1,10:0:10,10:0:30,100:0:1,100:0:10,100:0:30,500:0:1,500:0:10,500:0:30', \
            __timing='repeat=100', \
            __mpi='comm=mpicom,use="spmd_utils:mpicom"', \
            __openmp='enable', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['laplace_sphere_wk.0.0.1', 'laplace_sphere_wk.0.0.10', 'laplace_sphere_wk.0.0.30', 'laplace_sphere_wk.10.0.1', \
                'laplace_sphere_wk.10.0.10', 'laplace_sphere_wk.10.0.30', 'laplace_sphere_wk.100.0.1', 'laplace_sphere_wk.100.0.10', \
                'laplace_sphere_wk.100.0.30', 'laplace_sphere_wk.500.0.1', 'laplace_sphere_wk.500.0.10', 'laplace_sphere_wk.500.0.30' ]
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
