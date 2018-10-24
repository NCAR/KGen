# kgentest.py
from __future__ import print_function
import os
import glob

from kext_func_ch_test import KExtFuncCHTest

class KExtFuncCHEDPTest(KExtFuncCHTest):
    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']
        FC = result['config_task']['FC']
        FC_FLAGS = result['config_task']['FC_FLAGS']
        PRERUN = result['config_task']['PRERUN']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            _e='exclude.ini', \
            __cmd_clean='"cd %s; make -f Makefile clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile run"'%tmpsrc, \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(FC, FC_FLAGS), \
            __prerun='build="%s",run="%s"'%(PRERUN, PRERUN), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        result[myname]['statefiles'] = glob.glob(workdir+"/kernel/add.*.*.*")
        if passed:
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, err)
        return result
    pass
