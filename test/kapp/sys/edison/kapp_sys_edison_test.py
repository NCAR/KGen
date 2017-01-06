import os
import shutil
import getpass
from kapp_sys_test import KAppSysTest
from kgen_utils import run_shcmd

class KAppSysEdisonTest(KAppSysTest):
    def preprocess(self, myname, result):
        out, err, retcode = run_shcmd("sinfo | cut -d ' ' -f 1")

        if retcode != 0 or out.find('debug')<0 or out.find('realtime')<0 or out.find('special')<0 or out.find('shared')<0:
            errmsg = 'Current system is not Edison of NERSC'
            self.set_status(result, myname, self.FAILED, errmsg)
        else:
            self.set_status(result, myname, self.PASSED)

        return result

    def mkworkdir(self, myname, result):
        if self.WORK_DIR is None:
            self.WORK_DIR = '/scratch1/scratchdirs/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

        if os.path.exists('%s/kernel'%workdir):
            shutil.rmtree('%s/kernel'%workdir)
        os.makedirs('%s/kernel'%workdir)

        if os.path.exists('%s/state'%workdir):
            shutil.rmtree('%s/state'%workdir)
        os.makedirs('%s/state'%workdir)

        result[myname]['reuse_data'] = False
        if os.path.exists('%s/data'%workdir):
            src_files = os.listdir('%s/data'%workdir)
            for file_name in src_files:
                full_file_name = os.path.join('%s/data'%workdir, file_name)
                if (os.path.isfile(full_file_name)):
                    result[myname]['reuse_data'] = True
                    shutil.copy(full_file_name, '%s/kernel'%workdir)

        result[myname]['sysdir'] = systestdir
        result[myname]['workdir'] = workdir

        self.set_status(result, myname, self.PASSED)

        return result
