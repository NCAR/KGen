# kgentest.py

import os
import shutil
import re
import time
from kext_sys_ys_test import KExtSysYSTest

class KExtSysYSCesmTest(KExtSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/cesm_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if cesm exists in appsrc dir
        out, err, retcode = self.run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            out, err, retcode = self.run_shcmd('svn checkout https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_4_beta06 .', cwd=appsrc)

        # copy cesm src into test specific src dir
        tmpsrc = '%s/cesm_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def config(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']
        scriptdir = '%s/cime/scripts'%tmpsrc
        casename = 'KGENCESM'
        casedir = '%s/%s'%(systestdir, casename)

        # check if project option exists
        if 'project' not in self.OPTIONS:
            result[myname]['errmsg'] = 'project user option is not provided.'
            return result

        # create a case
        if not os.path.exists(casedir):
            casecmd = './create_newcase -project %s -mach yellowstone -compset FC5 -res ne16_ne16 -compiler intel -case %s'%(self.OPTIONS['project'], casedir)
            out, err, retcode = self.run_shcmd(casecmd, cwd=scriptdir)
            if retcode!=0:
                result[myname]['errmsg'] = 'MG2 case generation is failed.'
                return result

        # modify env_build.xml to enable MG2
        out, err, retcode = self.run_shcmd('grep mg2 env_build.xml', cwd=casedir)
        if retcode!=0:
            out, err, retcode = self.run_shcmd('./xmlchange -f env_build.xml -id CAM_CONFIG_OPTS -val "-microphys mg2 -clubb_sgs" -a', cwd=casedir)
            if retcode!=0:
                result[myname]['errmsg'] = 'Modification of env_build.xml is failed.'
                return result

        # cesm.setup
        if not os.path.exists('%s/%s.run'%(casedir, casename)):
            out, err, retcode = self.run_shcmd('./cesm.setup', cwd=casedir)

        # include.ini was created manually

        result[myname]['srcmods'] = '%s/SourceMods'%casedir
        result[myname]['casedir'] = casedir
        result[myname]['casename'] = casename

        self.set_status(result, myname, self.PASSED)

        return result

    def build(self, myname, result):

        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']
        statefiles = result['extract_task']['statefiles']
        workdir = result['mkdir_task']['workdir']

        datadir = '%s/data'%workdir
        result[myname]['datadir'] = datadir

        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
            # clean build
            out, err, retcode = self.run_shcmd('./%s.clean_build'%casename, cwd=casedir)
            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg='%s.clean_build is failed.'%casename)
            else:
                # build
                out, err, retcode = self.run_shcmd('./%s.build'%casename, cwd=casedir)
                if retcode != 0:
                    self.set_status(result, myname, self.FAILED, errmsg='%s.build is failed.'%casename)
                else:
                    self.set_status(result, myname, self.PASSED)
        else:
            # copy files from data to kernel directory
            for statefile in statefiles:
                shutil.copyfile(os.path.join(datadir, statefile), '%s/kernel/%s'%(workdir, statefile))

            result['goto'] = 'runkernel_task'
            self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']
        workdir = result['mkdir_task']['workdir']

        # may need to add -P BSUB directive in .run and .st_archive scripts

        # run cesm
        out, err, retcode = self.run_shcmd('./%s.submit'%casename, cwd=casedir)

        if retcode != 0 or not out:
            self.set_status(result, myname, self.FAILED, errmsg='Job submission is failed.')
            return result

        # find jobid
        jobid = None
        for iter in range(120):
            time.sleep(5)
            out, err, retcode = self.run_shcmd('bjobs')
            for line in out.split('\n'):
                items = line.split()
                if len(items)>6 and items[6].endswith('KGENCESM'):
                    jobid = items[0]
                    break
            if jobid: break

        if jobid is None:
            self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
            return result

        status = ''
        maxiter = 3600
        iter = 0
        while status not in [ 'DONE', 'PSUSP', 'USUSP', 'SSUSP', 'EXIT', 'UNKWN', 'ZOMBI', 'FINISHED' ]:
            time.sleep(1)
            out, err, retcode = self.run_shcmd('bjobs %s'%jobid)
            if retcode==0:
                for line in out.split('\n'):
                    items = line.split()
                    if len(items)>3 and items[0]==jobid:
                        status = items[2]
                    elif len(items)>0 and items[-1]=='found':
                        status = 'FINISHED'
            else:
                print('DEBUG: ', out, err, retcode)

            iter += 1
            if iter>=maxiter:
                break

        if status=='DONE' or 'FINISHED':
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, errmsg='Job completion status is not expected.')

        return result
