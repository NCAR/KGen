# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_mpas_test import KAppSysYSMpasTest

class KAppSysYSMpasIntelTest(KAppSysYSMpasTest):
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
