# runtest.py
# 
from kext_func_ys_test import KExtFuncYSTest

class CustomTest(KExtFuncYSTest):
    def verify(self, myname, result):
        workdir = result['mkdir_task']['workdir']
        outcome = result['runkernel_task']['stdout']

        if not outcome or outcome.find('FAILED')>0 or outcome.find('ERROR')>0 or outcome.find('PASSED')<0:
            self.set_status(result, myname, self.FAILED, outcome)
        else:
            f1 = open('%s/kernel/calling_module.F90'%workdir, 'r')
            f2 = open('%s/kernel/kernel.F90'%workdir, 'r')
            if 'comment5' in f1.read() or 'comment g' in f2.read():
                self.set_status(result, myname, self.FAILED, 'comment out of kernel codes still exist')
            else:
                result[myname]['status'] = self.PASSED
            f1.close()
            f2.close()

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
