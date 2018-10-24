# cover_test.py

import os
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

        out, err, retcode = self.run_shcmd(' '.join(cmds), cwd=self.TEST_DIR)

        # debug
        #print ' '.join(cmds)
        #print out
        if self.LEAVE_TEMP:
            with open('%s/kgen_cmds.sh'%self.TEST_DIR, 'w') as f:
                f.write('#!/bin/bash\n')
                f.write('\n')
                for cmd in cmds[:-1]:
                    f.write('    %s \\\n'%cmd)
                f.write('    %s'%cmds[-1])
            os.chmod('%s/kgen_cmds.sh'%self.TEST_DIR, 0755)

        if not out or out.find('ERROR')>=0 or out.find('CRITICAL')>=0 or err or retcode!=0:
            return False, out, err
        return True, out, err
