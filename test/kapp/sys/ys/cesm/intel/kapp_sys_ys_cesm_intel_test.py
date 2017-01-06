# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_cesm_test import KAppSysYSCesmTest

class KAppSysYSCesmIntelTest(KAppSysYSCesmTest):
    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']
        scriptdir = '%s/cime/scripts'%tmpsrc
        casename = 'KINTCESM'
        casedir = '%s/%s'%(systestdir, casename)

        datadir = '%s/data'%workdir

        # NOTE: svn co https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_4_beta07/ systestdir/cesm_ref
        if self.REBUILD or not os.path.exists(datadir) or len([name for name in os.listdir(datadir) if os.path.isfile(os.path.join(datadir, name))])==0 or \
            not os.path.exists(casedir):

            # check if project option exists
            if 'project' not in self.OPTIONS:
                self.set_status(result, myname, self.FAILED, errmsg='"project" user option is not provided. Use "-o project=<your porject id>"')
                return result

            # create a case
            if not os.path.exists(casedir):
                casecmd = './create_newcase -project %s -mach yellowstone -compset FC5 -res ne16_ne16 -compiler intel -case %s'%(self.OPTIONS['project'], casedir)
                out, err, retcode = run_shcmd(casecmd, cwd=scriptdir)
                if retcode!=0:
                    self.set_status(result, myname, self.FAILED, errmsg='MG2 case generation is failed: %s\n\n%s'%(err, out))
                    return result

            # modify env_build.xml to enable MG2
            out, err, retcode = run_shcmd('grep mg2 env_build.xml', cwd=casedir)
            if retcode!=0:
                xmlchange = './xmlchange -f env_build.xml -id CAM_CONFIG_OPTS -val "-microphys mg2 -clubb_sgs" -a'
                out, err, retcode = run_shcmd(xmlchange, cwd=casedir)
                if retcode!=0:
                    self.set_status(result, myname, self.FAILED, errmsg='Modification of env_build.xml is failed: %s, %s '%(err, out))
                    return result

            # cesm.setup
            if not os.path.exists('%s/case.run'%casedir):
                #import pdb; pdb.set_trace()
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

