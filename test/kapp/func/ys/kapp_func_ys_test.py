import os
import shutil
import getpass

from kapp_func_test import KAppFuncTest

class KAppFuncYSTest(KAppFuncTest):

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
        srcdir = '%s/..'%self.TEST_DIR

        testfiles = [ os.path.join(srcdir,f) for f in os.listdir(srcdir) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(srcdir, f))]
        if os.path.exists(os.path.join(self.TEST_DIR, 'Makefile')):
            testfiles.append(os.path.join(self.TEST_DIR, 'Makefile'))

        tmpsrc = result['mkdir_task']['tmpsrc']
        workdir = result['mkdir_task']['workdir']

        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, tmpsrc)
            dest_files.append(os.path.join(tmpsrc, os.path.basename(testfile)))

        if os.path.exists(os.path.join(srcdir, 'exclude.ini')):
            shutil.copy(os.path.join(srcdir, 'exclude.ini'), workdir)

        result[myname]['srcfiles'] = dest_files

        self.set_status(result, myname, self.PASSED)
        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']

        prerun_build = result['config_task']['prerun_build']
        prerun_run = result['config_task']['prerun_run']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', \
            __cmd_clean='"cd %s; make clean"'%tmpsrc, \
            __cmd_build='"cd %s; make build"'%tmpsrc, \
            __cmd_run='"cd %s; make run "'%tmpsrc, \
            __rebuild='all', \
            __prerun='build="%s",run="%s"'%(prerun_build, prerun_run), \
            __invocation='0:0:0', \
            __outdir=workdir)

            #__prerun='kernel_build="%s",kernel_run="%s"'%(prerun_build, prerun_run), \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.0.0.0']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result

    def rmdir(self, myname, result):
        workdir = result['mkdir_task']['workdir']

        if not self.LEAVE_TEMP:

            if os.path.exists(workdir):
                shutil.rmtree(workdir)

            if os.path.exists(os.path.join(workdir, 'kgen_cmds.sh')):
                os.remove(os.path.join(workdir, 'kgen_cmds.sh'))

            if os.path.exists(os.path.join(self.TEST_DIR, 'kgen.log')):
                os.remove(os.path.join(self.TEST_DIR, 'kgen.log'))

        self.set_status(result, myname, self.PASSED)

        return result
