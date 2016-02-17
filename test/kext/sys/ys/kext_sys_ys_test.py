# kgentest.py

import os
import shutil
import getpass
from kext_sys_test import KExtSysTest

class KExtSysYSTest(KExtSysTest):

    def preprocess(self, myname, result):
        out, err, retcode = self.run_shcmd('bqueues')

        if retcode != 0 or out.find('caldera')<0 or out.find('geyser')<0 or out.find('regular')<0 or out.find('premium')<0:
            errmsg = 'Current system is not Yellowstone of NCAR'
            self.set_status(result, myname, self.FAILED, errmsg)
        else:
            self.set_status(result, myname, self.PASSED)

        return result

    def mkworkdir(self, myname, result):
        if not self.WORK_DIR:
            self.WORK_DIR = '/glade/scratch/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

        result[myname]['sysdir'] = systestdir
        result[myname]['workdir'] = workdir

        self.set_status(result, myname, self.PASSED)

        return result


    def rmdir(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        kerneldir = '%s/kernel'%workdir
        statedir = '%s/state'%workdir
        kgenlog = '%s/kgen.log'%workdir

        if not self.LEAVE_TEMP:
            if os.path.exists(kerneldir):
                shutil.rmtree(kerneldir)

            if os.path.exists(statedir):
                shutil.rmtree(statedir)

            if os.path.exists(kgenlog):
                os.remove(kgenlog)

        self.set_status(result, myname, self.PASSED)

        return result
