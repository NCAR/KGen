# kgentest.py
from __future__ import print_function

import os
import shutil
from kext_test import KExtTest

class KExtFuncTest(KExtTest):
    def mkworkdir(self, myname, result):
        workdir = '%s/tmp'%self.TEST_DIR
        if os.path.exists(workdir): shutil.rmtree(workdir)
        os.mkdir(workdir)

        tmpsrc = '%s/src'%workdir
        os.mkdir(tmpsrc)

        result[myname]['workdir'] = workdir
        result[myname]['tmpsrc'] = tmpsrc

        result[myname]['status'] = self.PASSED
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

    def extract(self, myname, result):

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
        statefiles = result['extract_task']['statefiles']

        out, err, retcode = self.run_shcmd('make', cwd='%s/state'%workdir)
        result[myname]['stdout'] = out 
        result[myname]['stderr'] = err

        if err:
            self.set_status(result, myname, self.FAILED, err)
            return

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

        self.set_status(result, myname, self.PASSED)

        return result
