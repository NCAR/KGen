# runtest.py
# 
import os
import sys
import glob
import time
import shutil
from kext_sys_ch_calc_test import KExtSysCHCalcTest

here = os.path.dirname(__file__)

class Test(KExtSysCHCalcTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        result[myname]['datadir'] = '%s/data'%workdir

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        fc_flags = '-O3'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='%s/include.ini'%here, \
            _I=tmpsrc, \
            __cmd_clean='"cd %s; make clean"'%tmpsrc, \
            __cmd_build='"cd %s; make build"'%tmpsrc, \
            __cmd_run='"cd %s; make run"'%tmpsrc, \
            __openmp='enable', \
            __kernel_option='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob(workdir+"/kernel/add.*.*.*")
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def build(self, myname, result):
#
#        statefiles = result['generate_task']['statefiles']
#        workdir = result['mkdir_task']['workdir']
#        tmpsrc = result['download_task']['tmpsrc']
#
#        datadir = '%s/data'%workdir
#        result[myname]['datadir'] = datadir
#
#        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
#            # clean build
#            out, err, retcode = self.run_shcmd('make -f Makefile clean', cwd=tmpsrc)
#            if retcode != 0:
#                self.set_status(result, myname, self.FAILED, errmsg='make -f Makefile clean is failed.')
#            else:
#                # build
#                out, err, retcode = self.run_shcmd('make -f Makefile build', cwd=tmpsrc)
#                if retcode != 0:
#                    self.set_status(result, myname, self.FAILED, errmsg='make -f Makefile build is failed.')
#                else:
#                    self.set_status(result, myname, self.PASSED)
#        else:
#            # copy files from data to kernel directory
#            for statefile in statefiles:
#                shutil.copyfile(os.path.join(datadir, statefile), '%s/kernel/%s'%(workdir, statefile))
#
#            shutil.copyfile(os.path.join(datadir, 'kgen_statefile.lst'), '%s/kernel/kgen_statefile.lst'%workdir)
#
#            result['goto'] = 'runkernel_task'
#            self.set_status(result, myname, self.PASSED)
#
#        return result
#
#    def genstate(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        tmpsrc = result['download_task']['tmpsrc']
#
#        out, err, retcode = self.run_shcmd('make -f Makefile run', cwd=tmpsrc)
#
#        if retcode != 0:
#            self.set_status(result, myname, self.FAILED, errmsg='State generation is failed: %s\n\n%s'%(out, err))
#            return result
#        else:
#            self.set_status(result, myname, self.PASSED)
#
#        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
