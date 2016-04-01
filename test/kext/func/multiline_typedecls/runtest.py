# runtest.py
# 
import os
from kext_func_test import KExtFuncTest

class CustomTest(KExtFuncTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            '', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __invocation='1', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(self.COMPILER, self.COMPILER_FLAGS), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = ['add.1']
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, '%s\n\n%s'%(out, err))
        return result
    pass

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
