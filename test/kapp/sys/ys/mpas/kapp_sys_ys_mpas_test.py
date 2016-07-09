# kgentest.py

import os
import shutil
import re
import time
from kgen_utils import run_shcmd
from kapp_sys_ys_test import KAppSysYSTest

class KAppSysYSMpasTest(KAppSysYSTest):
    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/mpas_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if mpas exists in appsrc dir
        #out, err, retcode = run_shcmd('svn info | grep URL', cwd=appsrc)
        #if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
        #    out, err, retcode = run_shcmd('svn checkout -r 76722 https://svn-ccsm-models.cgd.ucar.edu/cesm1/tags/cesm1_4_beta06 .', cwd=appsrc)

        # copy mpas src into test specific src dir
        tmpsrc = '%s/mpas_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        if os.path.exists(os.path.join(self.TEST_DIR, 'exclude.ini')):
            shutil.copy(os.path.join(self.TEST_DIR, 'exclude.ini'), workdir)

        self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        reuse_data = result['mkdir_task']['reuse_data']

        if not reuse_data:
            # find jobid
            jobid = None
            for iter in range(120):
                #print('Waiting for "KGENMPAS" job to be initiated.')
                time.sleep(5)
                out, err, retcode = run_shcmd('bjobs')
                for line in out.split('\n'):
                    items = line.split()
                    if any(item=='KGENMPAS' for item in items):
                        jobid = items[0]
                        #print('"KGENMPAS" job is initiated.')
                        break
                if jobid: break

            if jobid is None:
                self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
                return result

            status = ''
            maxiter = 3600
            iter = 0
            sleep_sec = 1
            while status not in [ 'DONE', 'PSUSP', 'USUSP', 'SSUSP', 'EXIT', 'UNKWN', 'ZOMBI', 'FINISHED' ]:
                #if iter % 10 == 0:
                #    print('Waiting for "KGENMPAS" job to be finished. %d seconds has been passed.'%iter*sleep_sec)
                time.sleep(sleep_sec)
                out, err, retcode = run_shcmd('bjobs %s'%jobid)
                if retcode==0:
                    for line in out.split('\n'):
                        items = line.split()
                        if len(items)>3 and items[0]==jobid:
                            status = items[2]
                        elif len(items)>0 and items[-1]=='found':
                            print('"KGENMPAS" job is finished.')
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

