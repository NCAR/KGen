# kgentest.py

import os
import shutil
import re
import time
import getpass
from kgen_utils import run_shcmd
from kapp_sys_ch_test import KAppSysCHTest

class KAppSysCHCesmTest(KAppSysCHTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        #appsrc = '%s/cesm_ref'%systestdir
        cesmsrc = '/glade/u/home/youngsun/apps/cesm'
        appsrc = cesmsrc + '/cesm2.0.0'

        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if cesm exists in appsrc dir
        out, err, retcode = run_shcmd('git tag | grep "cesm"', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('cesm'):
            out, err, retcode = run_shcmd('git clone https://github.com/ESCOMP/cesm.git cesm2.0.0', cwd=cesmsrc)
            out, err, retcode = run_shcmd('git checkout release-cesm2.0.0', cwd=appsrc)
            out, err, retcode = run_shcmd('./manage_externals/checkout_externals', cwd=appsrc)

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

        import pdb; pdb.set_trace()
        if not reuse_data:
            # find jobid
            jobid = None
            for iter in range(120):
                time.sleep(5)
                out, err, retcode = run_shcmd('qstat -u %s'%getpass.getuser())
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

