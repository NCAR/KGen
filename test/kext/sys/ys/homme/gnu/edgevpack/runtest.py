# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ys_homme_gnu_test import KExtSysYSHommeGnuTest


class Test(KExtSysYSHommeGnuTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        blddir = result['config_task']['blddir']

        srcfile = '%s/src/share/prim_advection_mod.F90'%tmpsrc
        namepath = 'prim_advection_mod:euler_step:edgevpack'
        fc = 'gfortran'
        fc_flags = '-ffree-line-length-none -O3 -g -fopenmp'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='include.ini', \
            _I='%s/src:%s/src/share:%s/test_execs/perfTest'%(tmpsrc, tmpsrc, blddir), \
            _e='exclude.ini', \
            _D='HAVE_CONFIG_H', \
            __invocation='0:0-1:10,0:0-1:50,10:0-1:10,10:0-1:50', \
            __timing='repeat=1', \
            __mpi='enable', \
            __openmp='enable', \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['edgevpack.0.0.10', 'edgevpack.0.0.50', 'edgevpack.0.1.10', 'edgevpack.0.1.50', \
                'edgevpack.10.0.10', 'edgevpack.10.0.50', 'edgevpack.10.1.10', 'edgevpack.10.1.50']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

    def replace(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        for instrumented in glob.glob('%s/state/*.F90'%workdir):
            fname = os.path.basename(instrumented)
            if not os.path.exists('%s/src/share/%s.kgen'%(tmpsrc, fname)): 
                shutil.copy2('%s/src/share/%s'%(tmpsrc, fname), '%s/src/share/%s.kgen'%(tmpsrc, fname))
            os.remove('%s/src/share/%s'%(tmpsrc, fname))
            shutil.copy2(instrumented, '%s/src/share'%tmpsrc)
 
        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
