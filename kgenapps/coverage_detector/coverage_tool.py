#!/usr/bin/python
#
# coverage.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )

import os
import sys
import stat

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)

KGEN_COVERAGE = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%KGEN_COVERAGE
KGEN_BASE = '%s/base'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_COVERAGE)

from kgen_tool import KGenTool
from kgen_utils import Config, run_shcmd, Logger, ProgramException
from kgen_compiler import CompilerFactory
from coverage_config import CoverageConfig
import subprocess

from coverage_make import generate_makefile

STR_EX = 'execve('
STR_EN = 'ENOENT'
STR_UF = '<unfinished'
TEMP_SH = '#!/bin/bash\n%s\n%s\n%s\n%s\n'
SH = '%s/_kgen_coverage_cmdwrapper.sh'

def _getpwd(env):
    for item in env:
        if item.startswith('PWD='):
            return item[4:]
    return None

class CoverageDetect(KGenTool):

    def init(self, argv=None):
        self.invocation = []

        self.config= CoverageConfig(argv=argv)

        Config.register(self.config)

    # generate instrumented source filed for coverage and makefile to drive
    # geneartion of coverage raw data
    def main(self):
        
        Logger.info('Starting KCoverage', stdout=True)

        if not os.path.exists(self.config.coverage['cwd']):
            os.mkdir(self.config.coverage['cwd'])

        coverage_srcfiles = []
        # generate instrumented source files


        # generate makefile
        generate_makefile(coverage_srcfiles)

    # read coverage raw data and generate invocation command line flag
    def fini(self):
        import ConfigParser

        Logger.info('KCoverage is finished.', stdout=True)

        #return { 'invocation': '0:0:0' }
        return { 'invocation': ','.join(self.invocation) }

if __name__ == '__main__':
    coverage = CoverageDetect()
    coverage.init()
    coverage.main()
    coverage.fini()

