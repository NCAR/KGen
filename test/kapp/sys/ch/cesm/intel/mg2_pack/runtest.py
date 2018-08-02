import os
import sys
import glob
import shutil
from kapp_sys_ch_cesm_intel_test import KAppSysCHCesmIntelTest

#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/ncdio_pio.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/atm/obj/subcol_pack_mod.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/restUtilMod.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/atm/obj/subcol_utils.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/initInterp2dvar.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/array_utils.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/atm/obj/buffer.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/dynVarTimeInterpMod.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/atm/obj/physics_buffer.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/dynVarMod.F90]
#[/glade2/scratch2/youngsun/KINTCESM/bld/intel/mpt/nodebug/nothreads/mct/noesmf/clm/obj/dynVarTimeUninterpMod.F90]

here = os.path.dirname(os.path.abspath(__file__))
atmgendir = os.path.join(here, "..", "..", "genfiles", "atm")

class Test(KAppSysCHCesmIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']

        camsrcmods = '%s/src.cam'%srcmods
        result[myname]['camsrcmods'] = camsrcmods

        srcfile = '%s/components/cam/src/physics/cam/micro_mg_cam.F90'%tmpsrc
        namepath = 'micro_mg_cam:micro_mg_cam_tend:micro_mg_cam_tend_pack'

        for f in os.listdir(atmgendir):
            src = os.path.join(atmgendir, f)
            dst = os.path.join(result['config_task']['cesmtmpdir'], "bld", "atm", "obj", f)
            if os.path.isfile(src):
                shutil.copyfile(src, dst)
 
        passed, out, err = self.extract_kernel(srcfile, namepath, \
            __cmd_clean='"cd %s; ./case.clean_build all"'%casedir, \
            __cmd_build='"cd %s; ./case.build"'%casedir, \
            __cmd_run='"cd %s; ./case.submit"'%casedir, \
            __timing='repeat=1', \
            __intrinsic='skip,except=shr_spfn_mod:shr_spfn_gamma_nonintrinsic_r8:sum', \
            __mpi='comm=mpicom,use="spmd_utils:mpicom",header="/glade/u/apps/ch/opt/mpt/2.16/include/mpif.h"', \
            __openmp='enable', \
            __outdir=workdir)
            #__rebuild='all',
            #__kernel_compile='PRERUN="module load intel/16.0.2"',
            #__debug='printvar=:i,:j,:output',
            #__invocation='0:0:10,0:0:50,0:0:100,100:0:10,100:0:50,100:0:100,300:0:10,300:0:50,300:0:100', \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            for dfile in glob.glob('%s/micro_mg_cam_tend_pack.*.*.*'%kerneldir):
                result[myname]['statefiles'].append(os.path.basename(dfile))
            #result[myname]['statefiles'] = ['micro_mg_get_cols2_0.0.0.10', 'micro_mg_get_cols2_0.0.0.50', 'micro_mg_get_cols2_0.0.0.100', \
            #    'micro_mg_get_cols2_0.100.0.10', 'micro_mg_get_cols2_0.100.0.50', 'micro_mg_get_cols2_0.100.0.100', \
            #    'micro_mg_get_cols2_0.300.0.10', 'micro_mg_get_cols2_0.300.0.50', 'micro_mg_get_cols2_0.300.0.100']
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

