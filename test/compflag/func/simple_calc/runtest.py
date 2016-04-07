# runtest.py
# 
from compflag_func_test import CompflagFuncTest

class SimpleCalc(CompflagFuncTest):
    pass

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
