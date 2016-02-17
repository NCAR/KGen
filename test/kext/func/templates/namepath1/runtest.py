# runtest.py
# 
from kext_func_test import KExtFuncTest

class CustomTest(KExtFuncTest):
    pass

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
