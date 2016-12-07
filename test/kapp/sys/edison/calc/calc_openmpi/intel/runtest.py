import sys
import time
import getpass
from kgen_utils import run_shcmd
from kapp_sys_edison_calc_calc_openmpi_test import KAppSysEdisonCalcCOMTest

class Test(KAppSysEdisonCalcCOMTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        prerun = 'module load openmpi-ccm'

        passed, out, err = self.extract_kernel(srcfile, namepath, \
            __cmd_clean='"cd %s; make -f Makefile.slurm clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile.slurm build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile.slurm run"'%tmpsrc, \
            __invocation='0-1:0:1,2-3:0:3', \
            __timing='repeat=1', \
            __rebuild='all', \
            __prerun='build="%s",run="%s"'%(prerun, prerun), \
            __mpi='enable', \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err
        result[myname]['datadir'] = '%s/data'%workdir

        if passed:
            result[myname]['statefiles'] = ['calc.0.0.1', 'calc.1.0.1', 'calc.2.0.3', 'calc.3.0.3' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

    def genstate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        reuse_data = result['mkdir_task']['reuse_data']

        if not reuse_data:
            # find jobid
            jobid = None
            for iter in range(120):
                time.sleep(5)
                out, err, retcode = run_shcmd('squeue -u %s'%getpass.getuser())
                for line in out.split('\n'):
                    items = line.split()
                    if any(item=='KGCALC' for item in items):
                    #if len(items)>6 and items[6].endswith('KHOMME'):
                        jobid = items[0]
                        break
                if jobid: break

            if jobid is None:
                self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
                return result

            status = ''
            maxiter = 3600
            iter = 0
            while status not in [ 'BF', 'CA', 'CD', 'F', 'NF', 'PR', 'SE', 'ST', 'TO' ]:
                time.sleep(1)
                out, err, retcode = run_shcmd('squeue -j %s'%jobid)
                if retcode==0:
                    lines = out.strip().split('\n')
                    if len(lines) == 1: break

                    for line in lines[1:]:
                        items = line.split()
                        if len(items) > 9:
                            if items[0]==jobid:
                                status = items[9]
                else:
                    print('DEBUG: ', out, err, retcode)
                    break

                iter += 1
                if iter>=maxiter:
                    break

            if status=='F' or 'CD' or 'CG':
                self.set_status(result, myname, self.PASSED)
            else:
                self.set_status(result, myname, self.FAILED, errmsg='Job completion status is not expected.')
        else:
            self.set_status(result, myname, self.PASSED)

        return result


if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
