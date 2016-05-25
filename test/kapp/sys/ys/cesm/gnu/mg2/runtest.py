import os
import sys
import glob
import shutil
from kapp_sys_ys_cesm_gnu_test import KAppSysYSCesmGnuTest


class Test(KAppSysYSCesmGnuTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        #srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']

        #camsrcmods = '%s/src.cam'%srcmods
        #result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/physics/cam/micro_mg_cam.F90'%tmpsrc
        namepath = 'micro_mg_cam:micro_mg_cam_tend:micro_mg_tend2_0'

        passed, out, err = self.extract_kernel(srcfile, namepath, \
            '"cd %s; ./%s.clean_build all"'%(casedir, casename), \
            '"cd %s; ./%s.build"'%(casedir, casename), \
            '"cd %s; ./%s.submit"'%(casedir, casename), \
            __prerun='kernel_build="module swap intel gnu/6.1.0",kernel_run="module swap intel gnu/6.1.0"', \
            __invocation='0:0:10,0:0:50,0:0:100,100:0:10,100:0:50,100:0:100,300:0:10,300:0:50,300:0:100', \
            __timing='repeat=1', \
            __intrinsic='skip,except=shr_spfn_mod:shr_spfn_gamma_nonintrinsic_r8:sum', \
            __mpi='comm=mpicom,use="spmd_utils:mpicom"', \
            __openmp='enable', \
            __outdir=workdir)
            #__rebuild='all',

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir


        if passed:
            result[myname]['statefiles'] = ['micro_mg_tend2_0.0.0.10', 'micro_mg_tend2_0.0.0.50', 'micro_mg_tend2_0.0.0.100', \
                'micro_mg_tend2_0.100.0.10', 'micro_mg_tend2_0.100.0.50', 'micro_mg_tend2_0.100.0.100', \
                'micro_mg_tend2_0.300.0.10', 'micro_mg_tend2_0.300.0.50', 'micro_mg_tend2_0.300.0.100']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result
#
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

