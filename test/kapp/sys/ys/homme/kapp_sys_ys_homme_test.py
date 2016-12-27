# kgentest.py

import os
import shutil
import re
import time
from kapp_sys_ys_test import KAppSysYSTest
from kgen_utils import run_shcmd

job_script = \
"""#!/bin/bash

#BSUB -a poe
#BSUB -P STDD0002
#BSUB -q premium
#BSUB -W 0:20
#BSUB -x
#BSUB -J KHOMME
#BSUB -e homme.%%J.err
#BSUB -o homme.%%J.out
#BSUB -n %s
#BSUB -R "span[ptile=%s]" 

%s

# Pure MPI test 1
%s %s < %s
"""

class KAppSysYSHommeTest(KAppSysYSTest):

    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        #appsrc = '%s/homme_ref'%systestdir
        appsrc = '/glade/u/home/youngsun/apps/homme/trunk'
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if homme exists in appsrc dir
        out, err, retcode = run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            #out, err, retcode = run_shcmd('svn checkout -r 4971 https://svn-homme-model.cgd.ucar.edu/trunk/ .', cwd=appsrc) # r 4971 has broken pio external link
            #out, err, retcode = run_shcmd('svn checkout -r 5438 https://svn-homme-model.cgd.ucar.edu/trunk/ .', cwd=appsrc)
            out, err, retcode = run_shcmd('svn checkout -r 5704 https://svn-homme-model.cgd.ucar.edu/trunk/ .', cwd=appsrc)
            #out, err, retcode = run_shcmd('svn checkout -r 5650 https://svn-homme-model.cgd.ucar.edu/branch_tags/dungeon_tags/dungeon06 .', cwd=appsrc)

        # copy homme src into test specific src dir
        tmpsrc = '%s/homme_work'%systestdir
#        if os.path.exists(tmpsrc):
#            shutil.rmtree(tmpsrc)
#        shutil.copytree(appsrc, tmpsrc)
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)
        else:
            for fname in os.listdir('%s/src'%tmpsrc):
                if fname.endswith('.kgen'):
                    shutil.copyfile(os.path.join('%s/src'%tmpsrc, fname), os.path.join('%s/src'%tmpsrc, fname[:-5]))
            for fname in os.listdir('%s/src/share'%tmpsrc):
                if fname.endswith('.kgen'):
                    shutil.copyfile(os.path.join('%s/src/share'%tmpsrc, fname), os.path.join('%s/src/share'%tmpsrc, fname[:-5]))

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

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
                    if any(item=='KHOMME' for item in items):
                    #if len(items)>6 and items[6].endswith('KHOMME'):
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
