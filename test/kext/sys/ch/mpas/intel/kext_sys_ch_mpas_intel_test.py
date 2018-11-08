# kgentest.py

import os
import shutil
import re
import time
from kgutils import run_shcmd
from kext_sys_ch_mpas_test import KExtSysCHMpasTest

class KExtSysCHMpasIntelTest(KExtSysCHMpasTest):

    def get_prerun_kernel_cmds(self):

        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module load ncarenv/1.2')
        prerun_cmds.append('module load intel/17.0.1')
        prerun_cmds.append('module load mpt/2.15f')
        prerun_cmds.append('module load ncarcompilers/0.4.1')
        prerun_cmds.append('module load pio/2.3.1')

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = self.get_prerun_kernel_cmds()

        return prerun_cmds

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']
        tmprun = result['download_task']['tmprun']

        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()
        result[myname]['prerun_app'] = self.get_prerun_cmds()

        if 'project' not in self.OPTIONS:
            self.OPTIONS['project'] = 'NTDD0004'

        self.set_status(result, myname, self.PASSED)

        return result

