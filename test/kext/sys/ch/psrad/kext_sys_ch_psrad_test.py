# kgentest.py

import os
import shutil
import re
import time
from kext_sys_ch_test import KExtSysCHTest

class KExtSysCHPsradTest(KExtSysCHTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/psrad_ref'%systestdir

        # check if psrad exists in appsrc dir
        if not os.path.exists(appsrc):
            shutil.copytree('/glade/p/cisl/asap/youngsun/app/echam-PSrad', appsrc)

        # copy psrad src into test specific src dir
        tmpsrc = '%s/psrad_work'%systestdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

#    def build(self, myname, result):
#
#        statefiles = result['generate_task']['statefiles']
#        workdir = result['mkdir_task']['workdir']
#
#        tmpsrc = result['download_task']['tmpsrc']
#
#        datadir = '%s/data'%workdir
#        result[myname]['datadir'] = datadir
#        
#        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
#            # clean build
#            out, err, retcode = self.run_shcmd('make clean', cwd=tmpsrc)
#            if retcode != 0:
#                self.set_status(result, myname, self.FAILED, errmsg='make clean is failed.')
#            else:
#                # build
#                out, err, retcode = self.run_shcmd('make', cwd=tmpsrc)
#                if retcode != 0:
#                    self.set_status(result, myname, self.FAILED, errmsg='make is failed.')
#                else:
#                    self.set_status(result, myname, self.PASSED)
#        else:
#            # copy files from data to kernel directory
#            for statefile in statefiles:
#                shutil.copyfile(os.path.join(datadir, statefile), '%s/kernel/%s'%(workdir, statefile))
#
#            shutil.copyfile(os.path.join(datadir, 'kgen_statefile.lst'), '%s/kernel/kgen_statefile.lst'%workdir)
#
#            result['goto'] = 'runkernel_task'
#            self.set_status(result, myname, self.PASSED)
#
#        return result
#
#    def genstate(self, myname, result):
#
#        tmpsrc = result['download_task']['tmpsrc']
#        workdir = result['mkdir_task']['workdir']
#
#        # may need to add -P BSUB directive in .run and .st_archive scripts
#
#        # run psrad
#        out, err, retcode = self.run_shcmd('../PSrad.exe namelist', cwd='%s/work'%tmpsrc)
#
#        if retcode != 0 or not out:
#            self.set_status(result, myname, self.FAILED, errmsg='Execution of PSrad is failed.')
#        else:
#            self.set_status(result, myname, self.PASSED)
#
#        return result
