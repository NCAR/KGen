# kgentest.py

import os
import shutil
from kext_test import KExtTest

class KExtSysTest(KExtTest):

    def savestate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        statefiles = result['extract_task']['statefiles']
        datadir = result['build_task']['datadir']

        # if data folder does not exist, create it
        if not os.path.exists(datadir):
            os.mkdir(datadir)

        # copy statefiles if they are newer.
        for statefile in statefiles:
            kerneldata = '%s/kernel/%s'%(workdir, statefile)
            datafile = os.path.join(datadir, statefile)
            if not os.path.exists(datafile) or \
                os.path.getctime(kerneldata) < os.path.getctime(datafile):
                shutil.copyfile(kerneldata, datafile)

        self.set_status(result, myname, self.PASSED)

        return result
