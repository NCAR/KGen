# kgentest.py

import os
import shutil
import re
import time
from kgutils import run_shcmd
from kext_sys_ch_cesm_test import KExtSysCHCesmTest

class KExtSysCHCesmIntelTest(KExtSysCHCesmTest):

    def get_prerun_kernel_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module try-load ncarenv/1.2')
        prerun_cmds.append('module try-load ncarcompilers/0.4.1')
        prerun_cmds.append('module try-load intel/17.0.1')
        prerun_cmds.append('module try-load mkl/2017.0.1')
        prerun_cmds.append('module try-load impi/2017.1.132')

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = self.get_prerun_kernel_cmds()
        prerun_cmds.append('module try-load netcdf/4.4.1.1')
        prerun_cmds.append('module try-load pnetcdf/1.10.0')
        prerun_cmds.append('module try-load cmake/3.9.1')

        return prerun_cmds

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']

        scriptdir = '%s/cime/scripts'%tmpsrc
        casename = 'KINTCESM'
        casedir = '%s/%s'%(systestdir, casename)
        scratchdir = "/glade/scratch/%s"%os.getenv("USER")
        caseworkdir = "%s/%s"%(scratchdir, casename)

        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()

        datadir = '%s/data'%workdir

        if self.REBUILD or not os.path.exists(datadir) or len([name for name in os.listdir(datadir) if os.path.isfile(os.path.join(datadir, name))])==0:

            # check if project option exists
            if 'project' not in self.OPTIONS:
                self.OPTIONS['project'] = 'NTDD0004'

            # create a case
            if not os.path.exists(casedir):
                run_shcmd("rm -rf "+caseworkdir, cwd=scriptdir)
                casecmd = './create_newcase --project %s --mach cheyenne --compset B1850 --res f19_g17 --compiler intel --queue premium --case %s'%(self.OPTIONS['project'], casedir)
                out, err, retcode = run_shcmd(casecmd, cwd=scriptdir)
                if retcode!=0:
                    self.set_status(result, myname, self.FAILED, errmsg='KINTCESM case generation is failed: %s\n\n%s'%(err, out))
                    return result

            # modify env_build.xml to enable MG2
            out, err, retcode = run_shcmd('grep mg2 env_build.xml', cwd=casedir)
            if retcode!=0:
                xmlchange = './xmlchange CAM_CONFIG_OPTS="-microphys mg2 -clubb_sgs" -a'
                out, err, retcode = run_shcmd(xmlchange, cwd=casedir)
                if retcode!=0:
                    self.set_status(result, myname, self.FAILED, errmsg='Modification of env_build.xml is failed: %s, %s'%(err, out))
                    return result

            batch_flags = "-W block=true -N KINTCESM.run -r n -j oe -V -S /bin/bash -l select=16:ncpus=36:mpiprocs=36:ompthreads=1 -l walltime=01:15:00 -A NTDD0004 -q premium"
            out, err, retcode = run_shcmd('./xmlchange BATCH_COMMAND_FLAGS="%s"'%batch_flags, cwd=casedir)

            # cesm.setup
            out, err, retcode = run_shcmd('./case.setup', cwd=casedir)
            if retcode!=0:
                self.set_status(result, myname, self.FAILED, errmsg='case.setup is failed: %s\n\n%s'%(err, out))
                return result

        for fname in os.listdir('%s/SourceMods'%casedir):
            if fname.startswith('src.') and os.path.isdir(os.path.join('%s/SourceMods'%casedir, fname)):
                for srcfile in os.listdir('%s/SourceMods/%s'%(casedir, fname)):
                    if os.path.isfile(os.path.join('%s/SourceMods/%s'%(casedir, fname), srcfile)):
                        os.remove(os.path.join('%s/SourceMods/%s'%(casedir, fname), srcfile))

        # include.ini was created manually

        result[myname]['srcmods'] = '%s/SourceMods'%casedir
        result[myname]['casedir'] = casedir
        result[myname]['casename'] = casename

        self.set_status(result, myname, self.PASSED)

        return result

