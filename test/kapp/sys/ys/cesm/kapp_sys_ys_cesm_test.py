# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_test import KAppSysYSTest

class KAppSysYSCesmTest(KAppSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        #appsrc = '%s/cesm_ref'%systestdir
        appsrc = '/glade/u/home/youngsun/apps/cesm/cesm1_5_beta07'
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if cesm exists in appsrc dir
        out, err, retcode = run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            out, err, retcode = run_shcmd('svn checkout -r 82434 https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_5_beta07 .', cwd=appsrc)

        # copy cesm src into test specific src dir
        tmpsrc = '%s/cesm_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        if os.path.exists(os.path.join(self.TEST_DIR, 'exclude.ini')):
            shutil.copy(os.path.join(self.TEST_DIR, 'exclude.ini'), workdir)

        self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']
        workdir = result['mkdir_task']['workdir']
        reuse_data = result['mkdir_task']['reuse_data']

        if not reuse_data:
            # find jobid
            jobid = None
            for iter in range(120):
                time.sleep(5)
                out, err, retcode = run_shcmd('bjobs')
                for line in out.split('\n'):
                    items = line.split()
                    if any(item==casename for item in items):
                    #if len(items)>6 and items[6].endswith(casename):
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
                out, err, retcode = run_shcmd('bjobs %s'%jobid)
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
        else:
            self.set_status(result, myname, self.PASSED)

        return result

