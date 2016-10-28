import sys
from kapp_sys_ys_calc_calc_mpi_stategen_test import KAppSysYSCalcCMSTest
import time
from kgen_utils import run_shcmd

class Test(KAppSysYSCalcCMSTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/bridge_mod.F90'%tmpsrc
        prerun = 'module swap intel gnu/5.3.0'

        passed, out, err = self.extract_kernel(srcfile, None, \
            __cmd_clean='"cd %s; make -f Makefile.mpirun clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile.mpirun build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile.mpirun run"'%tmpsrc, \
            __invocation='0-1:0:1,2-3:0:3', \
            __timing='repeat=1', \
            __prerun='build="%s",run="%s"'%(prerun, prerun), \
            __mpi='enable', \
            __rebuild='state', \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            result[myname]['statefiles'] = ['update.0.0.1', 'update.1.0.1', 'update.2.0.3', 'update.3.0.3' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def genstate(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        reuse_data = result['mkdir_task']['reuse_data']
#
#        if not reuse_data:
#            # find jobid
#            jobid = None
#            for iter in range(120):
#                time.sleep(5)
#                out, err, retcode = run_shcmd('bjobs')
#                for line in out.split('\n'):
#                    items = line.split()
#                    if any(item=='KGCALC' for item in items):
#                    #if len(items)>6 and items[6].endswith('KHOMME'):
#                        jobid = items[0]
#                        break
#                if jobid: break
#
#            if jobid is None:
#                self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
#                return result
#
#            status = ''
#            maxiter = 3600
#            iter = 0
#            while status not in [ 'DONE', 'PSUSP', 'USUSP', 'SSUSP', 'EXIT', 'UNKWN', 'ZOMBI', 'FINISHED' ]:
#                time.sleep(1)
#                out, err, retcode = run_shcmd('bjobs %s'%jobid)
#                if retcode==0:
#                    for line in out.split('\n'):
#                        items = line.split()
#                        if len(items)>3 and items[0]==jobid:
#                            status = items[2]
#                        elif len(items)>0 and items[-1]=='found':
#                            status = 'FINISHED'
#                else:
#                    print('DEBUG: ', out, err, retcode)
#
#                iter += 1
#                if iter>=maxiter:
#                    break
#
#            if status=='DONE' or 'FINISHED':
#                self.set_status(result, myname, self.PASSED)
#            else:
#                self.set_status(result, myname, self.FAILED, errmsg='Job completion status is not expected.')
#        else:
#            self.set_status(result, myname, self.PASSED)
#
#        return result
#
#
if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
