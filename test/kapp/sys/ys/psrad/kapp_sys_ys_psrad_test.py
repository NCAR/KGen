# kgentest.py

import os
import shutil
from kapp_sys_ys_test import KAppSysYSTest

class KAppSysYSPsradTest(KAppSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/psrad_ref'%systestdir

        # check if psrad exists in appsrc dir
        if not os.path.exists(appsrc):
            shutil.copytree('/glade/u/tdd/asap/contrib/echam-PSrad', appsrc)

        # copy psrad src into test specific src dir
        tmpsrc = '%s/psrad_work'%systestdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result
