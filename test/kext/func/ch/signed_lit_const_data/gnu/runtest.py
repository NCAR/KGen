# runtest.py
# 
from kext_func_ch_signed_lit_const_data_test import KExtFuncCHSLCTest

class CustomTest(KExtFuncCHSLCTest):
    def config(self, myname, result):

        result[myname]['FC'] = 'gfortran'
        result[myname]['FC_FLAGS'] = ''
        result[myname]['PRERUN'] = 'module purge; module try-load ncarenv/1.2; module try-load ncarcompilers/0.4.1; module try-load gnu/6.3.0; module try-load mpt/2.15f; module try-load netcdf/4.4.1.1'

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
