# cover_test.py

from kgen_test import KGenTest

class CompflagTest(KGenTest):
#    def preprocess(self, myname, result):
#        result['general']['mandatory_tasks'].extend([ 'generate_task', 'genstate_task', 'verify_task' ])
#        self.set_status(result, myname, self.PASSED)
#        return result
 
    def run_compflag(self, target, cwd, *args, **kwargs):
 
        cmds = [ '%s/bin/compflag'%self.KGEN_HOME ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
        cmds.append('-- %s'%target)
 
        out, err, retcode = self.run_shcmd(' '.join(cmds), cwd=cwd)
        #print('CMD: ', ' '.join(cmds))
        #print('RETURN CODE: ', retcode)
        #print 'STDOUT: ', out
        #print 'STDERR: ', err
        if not out or out.find('ERROR')>=0 or out.find('CRITICAL')>=0 or err or retcode!=0:
            return False, out, err
        return True, out, err
