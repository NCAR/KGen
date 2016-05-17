# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_test import KAppSysYSTest

class KAppSysYSCesmTest(KAppSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/cesm_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if cesm exists in appsrc dir
        out, err, retcode = run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            out, err, retcode = self.run_shcmd('svn checkout -r 76722 https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_4_beta06 .', cwd=appsrc)

        # copy cesm src into test specific src dir
        tmpsrc = '%s/cesm_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result
