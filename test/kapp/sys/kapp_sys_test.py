import os
import shutil
from kapp_test import KAppTest

class KAppSysTest(KAppTest):
    def savestate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        statefiles = result['generate_task']['statefiles']
        datadir = result['generate_task']['datadir']

        # if data folder does not exist, create it
        if not os.path.exists(datadir):
            os.mkdir(datadir)

        # copy statefiles if they are newer.
        statefile = 'kgen_statefile.lst'
        kernel_sf = '%s/kernel/%s'%(workdir, statefile)
        data_sf = '%s/data/%s'%(workdir, statefile)
        if os.path.exists(kernel_sf) and os.path.exists(data_sf):
            os.remove(data_sf)
        if os.path.exists(kernel_sf):
            shutil.copyfile(kernel_sf, data_sf)

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
        kgencmd = os.path.join(self.TEST_DIR, 'kgen_cmds.sh')
        inifile = os.path.join(workdir, 'include.ini')
        stracefile = os.path.join(workdir, 'strace.log')

        if not self.LEAVE_TEMP:
            if os.path.exists(kerneldir):
                shutil.rmtree(kerneldir)

            if os.path.exists(statedir):
                shutil.rmtree(statedir)

            if os.path.exists(kgenlog):
                os.remove(kgenlog)

            if os.path.exists(kgencmd):
                os.remove(kgencmd)

            if os.path.exists(inifile):
                os.remove(inifile)

            if os.path.exists(stracefile):
                os.remove(stracefile)

        self.set_status(result, myname, self.PASSED)

        return result
