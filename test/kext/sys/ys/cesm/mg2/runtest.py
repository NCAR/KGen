# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ys_cesm_test import KExtSysYSCesmTest


class CustomTest(KExtSysYSCesmTest):

    def config(self, myname, result):

        tmpsrc = result['download_task']['tmpsrc']
        scriptdir = '%s/cime/scripts'%tmpsrc
        casename = 'KGENSYSTEST-FC5-MG2'
        casedir = '%s/%s'%(scriptdir, casename)

        # check if project option exists
        if 'project' not in self.OPTIONS:
            result[myname]['errmsg'] = 'project user option is not provided.'
            return result

        # create a case
        if not os.path.exists(casedir):
            out, err, retcode = self.run_shcmd('./create_newcase -project %s -mach yellowstone -compset FC5 -res ne16_ne16 -case %s'%(self.OPTIONS['project'], casename), cwd=scriptdir)
            if retcode!=0:
                result[myname]['errmsg'] = 'MG2 case generation is failed.'
                return result

        # modify env_build.xml to enable MG2
        out, err, retcode = self.run_shcmd('grep mg2 env_build.xml', cwd=casedir)
        if retcode!=0:
            out, err, retcode = self.run_shcmd('./xmlchange -f env_build.xml -id CAM_CONFIG_OPTS -val "-microphys mg2" -a', cwd=casedir)
            if retcode!=0:
                result[myname]['errmsg'] = 'Modification of env_build.xml is failed.'
                return result

        # cesm.setup
        if not os.path.exists('%s/%s.run'%(casedir, casename)):
            out, err, retcode = self.run_shcmd('./cesm.setup', cwd=casedir)

        # include.ini was created manually

        result[myname]['srcmods'] = '%s/SourceMods'%casedir
        result[myname]['casedir'] = casedir
        result[myname]['casename'] = casename

        self.set_status(result, myname, self.PASSED)

        return result

    def extract(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        srcmods = result['config_task']['srcmods']
        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']

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
            __state_switch='type=copy,cmds="rm -f %s/*; cp -f %s/state/*.F90 %s"'%(camsrcmods, workdir, camsrcmods), \
            __state_build='cmds="cd %s; %s.clean_build; %s.build"'%(casedir, casename, casename), \
            __state_run='cmds="cd %s; %s.submit"'%(casedir, casename), \
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
