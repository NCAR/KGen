import os
import glob
import shutil
from kapp_sys_edison_test import KAppSysEdisonTest

class KAppSysEdisonCalcTest(KAppSysEdisonTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/../src'%self.TEST_DIR

        # copy src into test specific src dir
        tmpsrc = '%s/src'%workdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        shutil.copytree(appsrc, tmpsrc)
        if os.path.exists('%s/Makefile'%self.TEST_DIR):
            shutil.copy('%s/Makefile'%self.TEST_DIR, tmpsrc)
        if os.path.exists('%s/Makefile.mpirun'%self.TEST_DIR):
            shutil.copy('%s/Makefile.mpirun'%self.TEST_DIR, tmpsrc)
        if os.path.exists('%s/Makefile.slurm'%self.TEST_DIR):
            shutil.copy('%s/Makefile.slurm'%self.TEST_DIR, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def replace(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']

        for instrumented in glob.glob('%s/state/*.F90'%workdir):
            shutil.copy2(instrumented, tmpsrc)

        self.set_status(result, myname, self.PASSED)

        return result
