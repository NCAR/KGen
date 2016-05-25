# runtest.py
# 
import os
import sys
import glob
import shutil
from kapp_sys_ys_psrad_test import KAppSysYSPsradTest


class Test(KAppSysYSPsradTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        
        srcfile = '%s/src/mo_psrad_interface.f90'%tmpsrc
        namepath = 'mo_psrad_interface:psrad_interface:lrtm'
        passed, out, err = self.extract_kernel(srcfile, namepath, \
            '"cd %s; make clean"'%tmpsrc, \
            '"cd %s; make"'%tmpsrc, \
            '"cd %s/work; ../PSrad.exe namelist"'%tmpsrc, \
            __invocation='0:0:2,0:0:10', \
            __timing='repeat=10', \
            __intrinsic='skip,except="mo_lrtm_driver:planckfunction:index;mo_lrtm_driver:planckfunction:fraction"', \
            __outdir=workdir)


        result[myname]['datadir'] = datadir = '%s/data'%workdir
        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['lrtm.0.0.2', 'lrtm.0.0.10']
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
