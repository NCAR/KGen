# kgentest.py

import os
import shutil
import re
import time
from kgutils import run_shcmd
from kext_sys_ch_test import KExtSysCHTest

here = os.path.abspath(os.path.dirname(__file__))

class KExtSysCHWrfTest(KExtSysCHTest):

    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/wrf_ref'%systestdir

        if os.path.exists(appsrc):
            # check if wrf exists in appsrc dir
            out, err, retcode = run_shcmd('git status | grep "nothing to commit"', cwd=appsrc)
            if retcode != 0 or not out or not out.startswith('nothing to commit'):
                run_shcmd('rm -rf ' + appsrc, cwd=systestdir)
                run_shcmd('git clone https://github.com/NCAR/WRFV3.git wrf_ref', cwd=systestdir)
                run_shcmd('git checkout V3.9.1.1', cwd=appsrc)
        else:
            run_shcmd('git clone https://github.com/NCAR/WRFV3.git wrf_ref', cwd=systestdir)
            run_shcmd('git checkout V3.9.1.1', cwd=appsrc)

        # copy wrf src into test specific src dir
        tmpsrc = '%s/wrf_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc
        result[myname]['jobscript'] = os.path.join(here, "submit.sh")

        self.set_status(result, myname, self.PASSED)

        return result
