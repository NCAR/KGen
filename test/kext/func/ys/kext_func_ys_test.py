# kgentest.py
from __future__ import print_function

import os
import shutil
import getpass
from kext_func_test import KExtFuncTest

class KExtFuncYSTest(KExtFuncTest):

    def mkworkdir(self, myname, result):
        if self.WORK_DIR is None:
            self.WORK_DIR = '/glade/scratch/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

        if os.path.exists('%s/kernel'%workdir):
            shutil.rmtree('%s/kernel'%workdir)

        if os.path.exists('%s/state'%workdir):
            shutil.rmtree('%s/state'%workdir)

        tmpsrc = '%s/src'%workdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        os.mkdir(tmpsrc)

        result[myname]['sysdir'] = systestdir
        result[myname]['workdir'] = workdir
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def download(self, myname, result):
        testfiles = [ os.path.join(self.TEST_DIR,f) for f in os.listdir(self.TEST_DIR) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(self.TEST_DIR, f))]

        tmpsrc = result['mkdir_task']['tmpsrc']

        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, tmpsrc)
            dest_files.append(os.path.join(tmpsrc, os.path.basename(testfile)))

        result[myname]['srcfiles'] = dest_files

        result[myname]['status'] = self.PASSED
        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __invocation='1', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(self.COMPILER, self.COMPILER_FLAGS), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.1']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result

    def genstate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        statefiles = result['generate_task']['statefiles']

        out, err, retcode = self.run_shcmd('make', cwd='%s/state'%workdir)
        result[myname]['stdout'] = out 
        result[myname]['stderr'] = err

        if err:
            self.set_status(result, myname, self.FAILED, err)
            return result

        outfiles = []
        for statefile in statefiles:
            outfile = os.path.join('%s/kernel'%workdir, statefile)
            if os.path.exists(outfile):
                outfiles.append(outfile)

        result[myname]['outfiles'] = outfiles

        if len(outfiles)==len(statefiles):
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, errmsg=str(outfiles))
        return result
#
#    def runkernel(self, myname, result):
#        workdir = result['mkdir_task']['workdir']
#
#        out, err, retcode = self.run_shcmd('make', cwd='%s/kernel'%workdir)
#
#        result[myname]['stdout'] = out
#        result[myname]['stderr'] = err
#
#        if err:
#            result[myname]['status'] = self.FAILED
#        else:
#            result[myname]['status'] = self.PASSED
#        return result

#    def verify(self, myname, result):
#        outcome = result['runkernel_task']['stdout']
#
#        if not outcome or outcome.find('FAILED')>0 or outcome.find('PASSED')<0:
#            result[myname]['status'] = self.FAILED
#        else:
#            result[myname]['status'] = self.PASSED
#        return result

    def rmdir(self, myname, result):
        workdir = result['mkdir_task']['workdir']

        if not self.LEAVE_TEMP and os.path.exists(workdir):
            shutil.rmtree(workdir)

        if os.path.exists(os.path.join(self.TEST_DIR, 'kgen.log')):
            os.remove(os.path.join(self.TEST_DIR, 'kgen.log'))

        self.set_status(result, myname, self.PASSED)

        return result
