# cover_test.py

from kgen_test import KGenTest

class CoverTest(KGenTest):

    def preprocess(self, myname, result):
        result['general']['mandatory_tasks'].extend([ 'generate_task', 'genstate_task', 'verify_task' ])
        self.set_status(result, myname, self.PASSED)
        return result

    def run_coverage(self, target, namepath, *args, **kwargs):

        cmds = [ '%s/bin/coverage'%self.KGEN_HOME ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
        cmds.append('%s:%s'%(target, namepath))

        print('CC: ', cmds)
        out, err, retcode = self.run_shcmd(' '.join(cmds), cwd=self.TEST_DIR)
        print('RETURN CODE: ', retcode)
        print 'STDOUT: ', out
        print 'STDERR: ', err
        if not out or out.find('ERROR')>=0 or out.find('CRITICAL')>=0 or err or retcode!=0:
            return False, out, err
        return True, out, err
