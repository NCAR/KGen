# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ch_homme_intel_test import KExtSysCHHommeIntelTest

here = os.path.dirname(__file__)

class Test(KExtSysCHHommeIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        blddir = result['config_task']['blddir']
        prerun_cmds = result['config_task']['prerun_run'] 
        mpirun = result['config_task']['mpirun'] 
        job_script = result['config_task']['job_script']
        namelist = result['config_task']['namelist']

        result[myname]['datadir'] = '%s/data'%workdir

        rundir = '%s/run'%workdir
        if os.path.exists(rundir):
            shutil.rmtree(rundir)
        os.mkdir(rundir)
        os.mkdir('%s/movies'%rundir)
        result[myname]['rundir'] = rundir
        
        # prepare namelist
        params = {'nelem': '6', 'nth': '2', 'nath': '2', 'tstep': '360'}
        if os.path.exists('%s/homme.nl'%rundir): os.system('rm -f %s/homme.nl'%rundir)
        with open('%s/homme.nl'%rundir, 'w') as fd:
            fd.write(namelist%params)

        # create symbolic linke to input data
        if os.path.exists('%s/vcoord'%rundir): os.system('unlink %s/vcoord'%rundir)
        os.system('ln -s %s/test/vcoord %s/vcoord'%(tmpsrc, rundir))

        os.system('rm -f %s/homme.*.err'%rundir)
        os.system('rm -f %s/homme.*.out'%rundir)

        # create job submit script
        with open('%s/homme.submit'%rundir, 'w') as fd:
            fd.write(job_script%('\n'.join(prerun_cmds), mpirun, '%s/test_execs/perfTest/perfTest'%blddir, '%s/homme.nl'%rundir))


        srcfile = '%s/src/share/prim_advection_mod.F90'%tmpsrc
        namepath = 'prim_advection_mod:euler_step:edgevunpack'
        fc = 'ifort'
        #fc_flags = '-assume byterecl -fp-model precise -ftz -O3 -g -openmp'
        fc_flags = '-assume byterecl -fp-model precise -ftz -O3 -g'
        prerun_cmds = ';'.join(result['config_task']['prerun_kernel'])
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _e='%s/exclude.dat'%here, \
            _i='%s/include.dat'%here, \
            _I='%s/src:%s/src/share:%s/test_execs/perfTest'%(tmpsrc, tmpsrc, blddir), \
            _D='HAVE_CONFIG_H', \
            __cmd_clean='"cd %s; make clean"'%blddir, \
            __cmd_build='"cd %s; make -j 4 perfTest"'%blddir, \
            __cmd_run='"cd %s; qsub homme.submit"'%rundir, \
            __mpi='enable', \
            __openmp='enable', \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __prerun='build="%s",run="%s"'%(prerun_cmds, prerun_cmds), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('edgevunpack.*.*.*')
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
#        for instrumented in glob.glob('%s/state/*.F90'%workdir):
#            fname = os.path.basename(instrumented)
#            if not os.path.exists('%s/src/share/%s.kgen'%(tmpsrc, fname)): 
#                shutil.copy2('%s/src/share/%s'%(tmpsrc, fname), '%s/src/share/%s.kgen'%(tmpsrc, fname))
#            os.remove('%s/src/share/%s'%(tmpsrc, fname))
#            shutil.copy2(instrumented, '%s/src/share'%tmpsrc)
# 
#        self.set_status(result, myname, self.PASSED)
#
#        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
