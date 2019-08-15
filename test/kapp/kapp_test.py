# kgentest.py

import os
import time
from kgtest import KGenTest
from kgutils import run_shcmd

class KAppTest(KGenTest):

    def extract_kernel(self, target, namepath, *args, **kwargs):

        outdir = '.'
        cmds = [ '%s/bin/kgen'%self.KGEN_HOME ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
            if flag=='--outdir':
                outdir = kwarg
        if namepath:
            cmds.append('%s:%s'%(target, namepath))
        else:
            cmds.append(target)

        for arg in args:
            cmds.append(arg)

        if self.LEAVE_TEMP:
            with open('%s/kgen_cmds.sh'%outdir, 'w') as f:
                f.write('#!/bin/bash\n')
                f.write('\n')
                for cmd in cmds[:-1]:
                    f.write('    %s \\\n'%cmd)
                f.write('    %s'%cmds[-1])
            os.chmod('%s/kgen_cmds.sh'%outdir, 0755)

        out, err, retcode = run_shcmd(' '.join(cmds))

        # debug
        #print 'CMDS: ', ' '.join(cmds)
        #print 'STDOUT: ', out
        #print 'STDERR: ', err
        #print 'RETCODE: ', retcode

        if not out or out.find('ERROR')>=0 or out.find('CRITICAL')>=0 or err.lower().find('error')>=0 or retcode!=0:

            return False, out, err

        return True, out, err

    def runkernel(self, myname, result):
        workdir = result['mkdir_task']['workdir']

        out, err, retcode = run_shcmd('make clean; make run', cwd='%s/kernel'%workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        #if retcode != 0 or err:
        #print 'STDOUT: ', out
        #print 'STDERR: ', err
        if retcode != 0:
            self.set_status(result, myname, self.FAILED, errmsg='kernel execution is failed: %s'%err)
        else:
            self.set_status(result, myname, self.PASSED)

        return result

    def verify(self, myname, result):
        outcome = result['runkernel_task']['stdout']

        if not outcome or outcome.find('FAILED')>0 or outcome.find('ERROR')>0 or outcome.find('PASSED')<0:
            self.set_status(result, myname, self.FAILED, outcome)
        else:
            self.set_status(result, myname, self.PASSED)
        return result


    def recover(self, myname, result):

        workdir = result['mkdir_task']['workdir']

        out, err, retcode = run_shcmd('make recover_from_locals', cwd='%s/state'%workdir)

        self.set_status(result, myname, self.PASSED)

        return result

