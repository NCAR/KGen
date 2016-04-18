# runtest.py
# 
import os
import sys
import glob
import time
import shutil
from kext_sys_ys_calc_test import KExtSysYSCalcTest


class Test(KExtSysYSCalcTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        fc_flags = '-O3'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='include.ini', \
            _I=tmpsrc, \
            __invocation='0-1:0:1,2-3:0:3,3:0:4-5', \
            __timing='repeat=10', \
            __mpi='enable', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['calc.0.0.1', 'calc.1.0.1', 'calc.2.0.3', 'calc.3.0.3', \
                'calc.3.0.4', 'calc.3.0.5']
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
