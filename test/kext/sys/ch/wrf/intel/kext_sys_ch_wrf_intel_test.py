# kgentest.py

import os
import shutil
import re
import time
from kgutils import run_shcmd
from kext_sys_ch_wrf_test import KExtSysCHWrfTest

class KExtSysCHWrfIntelTest(KExtSysCHWrfTest):

    def get_prerun_kernel_cmds(self):
        prerun_cmds = []

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module load ncarenv/1.2')
        prerun_cmds.append('module load intel/17.0.1')
        prerun_cmds.append('module load mpt/2.15f')
        prerun_cmds.append('module load ncarcompilers/0.4.1')
        prerun_cmds.append('module try-load netcdf/4.4.1.1')

        return prerun_cmds

    def config(self, myname, result):

        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()
        result[myname]['prerun'] = self.get_prerun_cmds()

        self.set_status(result, myname, self.PASSED)

        return result

