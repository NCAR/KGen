# kgentest.py

import os
import shutil
from kext_test import KExtTest

class KExtSysTest(KExtTest):

    def savestate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        statefiles = result['generate_task']['statefiles']
        datadir = result['generate_task']['datadir']

        # if data folder does not exist, create it
        if not os.path.exists(datadir):
            os.mkdir(datadir)

        # copy statefiles if they are newer.
        statelst = 'kgen_statefile.lst'
        kernel_sf = '%s/kernel/%s'%(workdir, statelst)
        data_sf = '%s/data/%s'%(workdir, statelst)
        if os.path.exists(kernel_sf) and os.path.exists(data_sf):
            os.remove(data_sf)
        if os.path.exists(kernel_sf):
            shutil.copyfile(kernel_sf, data_sf)

        for statefile in statefiles:
            dataname = os.path.basename(statefile)
            kerneldata = '%s/kernel/%s'%(workdir, dataname)
            if not os.path.exists(statefile) or \
                os.path.getctime(kerneldata) < os.path.getctime(statefile):
                shutil.copyfile(kerneldata, statefile)

        self.set_status(result, myname, self.PASSED)

        return result

    def rmdir(self, myname, result):

        workdir = result['mkdir_task']['workdir'] 
        kerneldir = '%s/kernel'%workdir
        statedir = '%s/state'%workdir
        kgenlog = os.path.join(workdir, 'kgen.log')
        kgencmd = os.path.join(workdir, 'kgen_cmds.sh')

        if not self.LEAVE_TEMP:
            if os.path.exists(kerneldir):
                shutil.rmtree(kerneldir)

            if os.path.exists(statedir):
                shutil.rmtree(statedir)

            if os.path.exists(kgenlog):
                os.remove(kgenlog)

            if os.path.exists(kgencmd):
                os.remove(kgencmd)

        self.set_status(result, myname, self.PASSED)

        return result
