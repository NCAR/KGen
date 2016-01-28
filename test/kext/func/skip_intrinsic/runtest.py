# runtest.py
# 
import os
from kgen_test import KExtTest

class CustomTest(KExtTest):
    def configure_test(self):
        self.configure_predefined_test('namepath1')

if __name__ == "__main__":
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
