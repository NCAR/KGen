# kgentest.py

from kgen_test import KGenTest

class KExtTest(KGenTest):

    def extract_kernel(self, target, namepath, *args, **kwargs):

        cmds = [ '%s/bin/kgen'%self.KGEN_HOME ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
        if namepath:
            cmds.append('%s:%s'%(target, namepath))
        else:
            cmds.append(target)

        out, err, retcode = self.run_shcmd(' '.join(cmds), cwd=self.TEST_DIR)

        # debug
        #print ' '.join(cmds)
        #print out

        if not out or out.find('ERROR')>=0 or out.find('CRITICAL')>=0 or err or retcode!=0:
            return False, out, err
        return True, out, err

    def runkernel(self, myname, result):
        workdir = result['mkdir_task']['workdir']

        out, err, retcode = self.run_shcmd('make clean; make', cwd='%s/kernel'%workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if retcode != 0 or err:
            self.set_status(result, myname, self.FAILED, errmsg='kernel execution is failed.')
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
