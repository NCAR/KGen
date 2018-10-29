# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ch_psrad_test import KExtSysCHPsradTest

here = os.path.dirname(__file__)

class Test(KExtSysCHPsradTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        result[myname]['datadir'] = '%s/data'%workdir

        srcfile = '%s/src/mo_psrad_interface.f90'%tmpsrc
        namepath = 'mo_psrad_interface:psrad_interface:lrtm'
        fc_flags = '-fpp -g -traceback -m64 -O3 -xHost'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _I='%s/src:%s/include'%(tmpsrc, tmpsrc), \
            __intrinsic='skip,except="mo_lrtm_driver:planckfunction:index;mo_lrtm_driver:planckfunction:fraction"', \
            __cmd_clean='"cd %s; make -f Makefile.snb clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile.snb all"'%tmpsrc, \
            __cmd_run='"cd %s/work; ../PSrad.exe namelist"'%tmpsrc, \
            __kernel_option='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __prerun='build="module load mkl",run="module load mkl"', \
            __check='tolerance="1.0D-10"', \
            __outdir=workdir)

#            _I='%s/src:%s/include'%(tmpsrc, tmpsrc), \
#            __invocation='0:0:2,0:0:10', \
#            __timing='repeat=10', \
#            __intrinsic='skip,except="mo_lrtm_driver:planckfunction:index;mo_lrtm_driver:planckfunction:fraction"', \
#            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
#            __outdir=workdir)


        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob(workdir+"/kernel/lrtm.*.*.*") 
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def replace(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        tmpsrc = result['download_task']['tmpsrc']
#
#        for instrumented in glob.glob('%s/state/*.f90'%workdir):
#            shutil.copyfile(instrumented, '%s/src/%s'%(tmpsrc, os.path.basename(instrumented)))
#            
#        self.set_status(result, myname, self.PASSED)
#
#        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
