# kgentest.py

import os
import shutil
from kext_test import KExtTest
from doit.tools import set_trace

class KExtFuncTest(KExtTest):
    def mkworkdir(self, myname):
        tmpdir = '%s/tmp'%self.TEST_DIR
        if os.path.exists(tmpdir): shutil.rmtree(tmpdir)
        os.mkdir(tmpdir)

        tmpsrc = '%s/src'%tmpdir
        os.mkdir(tmpsrc)

        self.result[myname]['tmdir'] = tmpdir
        self.result[myname]['tmpsrc'] = tmpsrc

        self.result[myname]['status'] = self.PASSED

    def download(self, myname):
        testfiles = [ os.path.join(self.TEST_DIR,f) for f in os.listdir(self.TEST_DIR) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(self.TEST_DIR, f))]

        tmpsrc = self.result['mkdir_task']['tmpsrc']

        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, tmpsrc)
            dest_files.append(os.path.join(tmpsrc, os.path.basename(testfile)))

        self.result[myname]['srcfiles'] = dest_files

        self.result[myname]['status'] = self.PASSED

    def extract(self, myname):

        tmpdir = self.result['mkdir_task']['tmdir']
        tmpsrc = self.result['mkdir_task']['tmpsrc']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __invocation='1', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(self.COMPILER, self.COMPILER_FLAGS), \
            __outdir=tmpdir)

        self.result[myname]['stdout'] = out
        self.result[myname]['stderr'] = err

        if passed:
            self.result[myname]['statefiles'] = ['add.1']
            self.set_status(myname, self.PASSED)
        else:
            self.result[myname]['statefiles'] = []
            self.set_status(myname, self.FAILED, err)

    def genstate(self, myname):

        tmpdir = self.result['mkdir_task']['tmdir']
        statefiles = self.result['extract_task']['statefiles']

        out, err = self.run_shcmd('make', cwd='%s/state'%tmpdir)
        self.result[myname]['stdout'] = out 
        self.result[myname]['stderr'] = err

        if err:
            self.set_status(myname, self.FAILED, err)
            return

        outfiles = []
        for statefile in statefiles:
            outfile = os.path.join('%s/kernel'%tmpdir, statefile)
            if os.path.exists(outfile):
                outfiles.append(outfile)

        self.result[myname]['outfiles'] = outfiles

        print(outfiles, statefiles)
        if cmp(outfiles, statefiles)==0:
            self.set_status(myname, self.PASSED)
        else:
            self.set_status(myname, self.FAILED, errmsg=str(outfiles))

    def runkernel(self, myname):
        tmpdir = self.result['mkdir_task']['tmdir']

        out, err = self.run_shcmd('make', cwd='%s/kernel'%tmpdir)

        self.result[myname]['stdout'] = out
        self.result[myname]['stderr'] = err

        if err:
            self.result[myname]['status'] = self.FAILED
        else:
            self.result[myname]['status'] = self.PASSED

    def verify(self, myname):
        result = self.result['runkernel_task']['stdout']

        if not result or result.find('FAILED')>0 or result.find('PASSED')<0:
            self.result[myname]['status'] = self.FAILED
            print '        ==> PASSED'
        else:
            self.result[myname]['status'] = self.PASSED
            print '        ==> FAILED'

    def rmdir(self, myname):
        tmpdir = self.result['mkdir_task']['tmdir']

        if not self.LEAVE_TEMP and os.path.exists(tmpdir):
            shutil.rmtree(tmpdir)
