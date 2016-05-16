import os
import getpass
import shutil
from compflag_test import CompflagTest

class CompflagFuncTest(CompflagTest):
    def mkworkdir(self, myname, result):
        if self.WORK_DIR is None:
            self.WORK_DIR = '/glade/scratch/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

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

        passed, out, err = self.run_compflag('make', \
            tmpsrc, __ini='outpath=%s/include.ini'%workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['compflagfiles'] = [ 'include.ini' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['compflagfiles'] = None
            self.set_status(result, myname, self.FAILED, '%s\n\n%s'%(out, err))
        return result

    def verify(self, myname, result):
        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']
        compflagfiles = result['generate_task']['compflagfiles']

        if any(not os.path.exists(os.path.join(workdir, cfile)) for cfile in compflagfiles):
            self.set_status(result, myname, self.FAILED, errmsg='Compflag files could not be found.')
            return result


        self.set_status(result, myname, self.PASSED)
        return result

    def rmdir(self, myname, result):

        workdir = result['mkdir_task']['workdir']

        if os.path.exists(os.path.join(self.TEST_DIR, 'kgen.log')):
            os.remove(os.path.join(self.TEST_DIR, 'kgen.log'))

        if not self.LEAVE_TEMP:
            if os.path.exists(workdir):
                shutil.rmtree(workdir)

        self.set_status(result, myname, self.PASSED)

        return result

