# runtest.py
# 
from kext_func_ys_implicit_do_loop_data_test import KExtFuncYSIDLTest

class CustomTest(KExtFuncYSIDLTest):
    def config(self, myname, result):

        result[myname]['FC'] = 'ifort'
        result[myname]['FC_FLAGS'] = ''
        result[myname]['PRERUN'] = 'module purge; module try-load ncarenv/1.0; module try-load ncarbinlibs/1.1; module try-load ncarcompilers/1.0; module try-load intel/16.0.1'

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
