from kapp_func_ch_associate_construct_test import KAppFuncCHASCTest

class CustomTest(KAppFuncCHASCTest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel gnu'
        result[myname]['prerun_run'] = 'module swap intel gnu'

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
