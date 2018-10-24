# runtest.py
# 
from kext_func_ch_generic_spec_assignment_test import KExtFuncCHGSATest

class CustomTest(KExtFuncCHGSATest):
    def config(self, myname, result):

        result[myname]['FC'] = 'pgfortran'
        result[myname]['FC_FLAGS'] = ''
        result[myname]['PRERUN'] = 'module purge; module try-load ncarenv/1.2; module try-load ncarcompilers/0.4.1; module try-load pgi/17.9; module try-load netcdf/4.4.1.1; module try-load mpt/2.15f' 

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
