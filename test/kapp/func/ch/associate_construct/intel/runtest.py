from kapp_func_ch_associate_construct_test import KAppFuncCHASCTest

class CustomTest(KAppFuncCHASCTest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel intel/18.0.5'
        result[myname]['prerun_run'] = 'module swap intel intel/18.0.5'

        self.set_status(result, myname, self.PASSED)

        return result
    pass

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
