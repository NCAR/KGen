from kapp_func_ys_elems_of_complex_datatypes_test import KAppFuncYSECDTest

class CustomTest(KAppFuncYSECDTest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel intel/16.0.1'
        result[myname]['prerun_run'] = 'module swap intel intel/16.0.1'

        self.set_status(result, myname, self.PASSED)

        return result
    pass

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
