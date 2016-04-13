# kgentest.py

import os
import shutil
from kext_test import KExtTest

class KExtSysTest(KExtTest):

    def savestate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        statefiles = result['generate_task']['statefiles']
        datadir = result['build_task']['datadir']

        # if data folder does not exist, create it
        if not os.path.exists(datadir):
            os.mkdir(datadir)

        # copy statefiles if they are newer.
        statefile = 'state_file.lst'
        statelist = '%s/kernel/%s'%(workdir, statefile)
        if os.path.exists('%s/%s'%(datadir, statefile)):
            os.remove('%s/%s'%(datadir, statefile))
        shutil.copyfile(statelist, '%s/%s'%(datadir, statefile))

        for statefile in statefiles:
            kerneldata = '%s/kernel/%s'%(workdir, statefile)
            datafile = os.path.join(datadir, statefile)
            if not os.path.exists(datafile) or \
                os.path.getctime(kerneldata) < os.path.getctime(datafile):
                shutil.copyfile(kerneldata, datafile)

        self.set_status(result, myname, self.PASSED)

        return result

    def rmdir(self, myname, result):

        workdir = result['mkdir_task']['workdir'] 
        kerneldir = '%s/kernel'%workdir
        statedir = '%s/state'%workdir
        kgenlog = os.path.join(self.TEST_DIR, 'kgen.log')

        if not self.LEAVE_TEMP:
            if os.path.exists(kerneldir):
                shutil.rmtree(kerneldir)

            if os.path.exists(statedir):
                shutil.rmtree(statedir)

            if os.path.exists(kgenlog):
                os.remove(kgenlog)

        self.set_status(result, myname, self.PASSED)

        return result
