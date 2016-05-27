from kapp_func_ys_alloc_dealloc_opt_test import KAppFuncYSADOTest

class CustomTest(KAppFuncYSADOTest):
    def config(self, myname, result):

        result[myname]['prerun_build'] = 'module swap intel pgi/16.1'
        result[myname]['prerun_run'] = 'module swap intel pgi/16.1'

        self.set_status(result, myname, self.PASSED)

        return result

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
