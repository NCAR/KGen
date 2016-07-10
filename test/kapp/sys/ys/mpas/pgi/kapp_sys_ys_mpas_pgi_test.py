# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_mpas_test import KAppSysYSMpasTest

class KAppSysYSMpasPgiTest(KAppSysYSMpasTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/mpas_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if mpas exists in appsrc dir
        #out, err, retcode = run_shcmd('svn info | grep URL', cwd=appsrc)
        #if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
        #    out, err, retcode = run_shcmd('svn checkout -r 76722 https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_4_beta06 .', cwd=appsrc)

        # copy mpas src into test specific src dir
        tmpsrc = '%s/mpas_pgi_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        if os.path.exists(os.path.join(self.TEST_DIR, 'exclude.ini')):
            shutil.copy(os.path.join(self.TEST_DIR, 'exclude.ini'), workdir)

        self.set_status(result, myname, self.PASSED)

        return result

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        datadir = '%s/data'%workdir

        if self.REBUILD or not os.path.exists(datadir) or len([name for name in os.listdir(datadir) if os.path.isfile(os.path.join(datadir, name))])==0:

            # check if project option exists
            if 'project' not in self.OPTIONS:
                self.set_status(result, myname, self.FAILED, errmsg='"project" user option is not provided. Use "-o project=<your porject id>"')
                return result

        self.set_status(result, myname, self.PASSED)

        return result
