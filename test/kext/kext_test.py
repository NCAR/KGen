# kgentest.py

from kgen_test import KGenTest

class KExtTest(KGenTest):

    def extract_kernel(self, target, namepath, *args, **kwargs):

        cmds = [ '%s/bin/kgen'%self.KGEN_HOME ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
        cmds.append('%s:%s'%(target, namepath))

        out, err = self.run_shcmd(' '.join(cmds), cwd=self.TEST_DIR)
        if not out or out.find('ERROR')>0 or err:
            return False, out, err
        return True, out, err
