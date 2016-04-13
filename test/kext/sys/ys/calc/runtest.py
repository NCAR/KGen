# runtest.py
# 
import os
import sys
import glob
import time
import shutil
from kext_sys_ys_test import KExtSysYSTest


class Test(KExtSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/src'%self.TEST_DIR

        # copy cesm src into test specific src dir
        tmpsrc = '%s/src'%workdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        srcfile = '%s/update_mod.F90'%tmpsrc
        namepath = 'update_mod:update:calc'
        fc_flags = '-O3'
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='include.ini', \
            _I=tmpsrc, \
            __invocation='0-1:0:1,0-1:0:3', \
            __timing='repeat=1', \
            __mpi='enable', \
            __kernel_compile='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
            __outdir=workdir)

            #__debug='printvar=:i,:j,:output',

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['calc.0.0.1', 'calc.0.0.3', 'calc.1.0.1', 'calc.1.0.3']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

    def replace(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        for instrumented in glob.glob('%s/state/*.F90'%workdir):
            shutil.copy2(instrumented, tmpsrc)
            
        self.set_status(result, myname, self.PASSED)

        return result

    def build(self, myname, result):

        statefiles = result['generate_task']['statefiles']
        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        datadir = '%s/data'%workdir
        result[myname]['datadir'] = datadir

        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
            # clean build
            out, err, retcode = self.run_shcmd('make -f Makefile.lsf clean', cwd=tmpsrc)
            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg='make -f Makefile.lsf clean is failed.')
            else:
                # build
                out, err, retcode = self.run_shcmd('make -f Makefile.lsf build', cwd=tmpsrc)
                if retcode != 0:
                    self.set_status(result, myname, self.FAILED, errmsg='make -f Makefile.lsf build is failed.')
                else:
                    self.set_status(result, myname, self.PASSED)
        else:
            # copy files from data to kernel directory
            for statefile in statefiles:
                shutil.copyfile(os.path.join(datadir, statefile), '%s/kernel/%s'%(workdir, statefile))

            result['goto'] = 'runkernel_task'
            self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        # may need to add -P BSUB directive in .run and .st_archive scripts

        # run cesm
        out, err, retcode = self.run_shcmd('make -f Makefile.lsf run', cwd=tmpsrc)

        if retcode != 0 or not out:
            self.set_status(result, myname, self.FAILED, errmsg='Job submission is failed.')
            return result

        # find jobid
        jobid = None
        for iter in range(120):
            time.sleep(5)
            out, err, retcode = self.run_shcmd('bjobs')
            for line in out.split('\n'):
                items = line.split()
                if len(items)>6 and items[6].endswith('KGCALC'):
                    jobid = items[0]
                    break
            if jobid: break

        if jobid is None:
            self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
            return result

        status = ''
        maxiter = 3600
        iter = 0
        while status not in [ 'DONE', 'PSUSP', 'USUSP', 'SSUSP', 'EXIT', 'UNKWN', 'ZOMBI', 'FINISHED' ]:
            time.sleep(1)
            out, err, retcode = self.run_shcmd('bjobs %s'%jobid)
            if retcode==0:
                for line in out.split('\n'):
                    items = line.split()
                    if len(items)>3 and items[0]==jobid:
                        status = items[2]
                    elif len(items)>0 and items[-1]=='found':
                        status = 'FINISHED'
            else:
                print('DEBUG: ', out, err, retcode)

            iter += 1
            if iter>=maxiter:
                break

        if status=='DONE' or 'FINISHED':
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, errmsg='Job completion status is not expected.')

        return result

if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
