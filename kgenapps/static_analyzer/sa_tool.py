#!/usr/bin/python
'''
This is the main Static Analyzer  script.
Author: Youngsung Kim <youngsun@ucar.edu>
'''
import sys
import os


# Python version check
#if sys.hexversion < 0x020600F0:
#    print 'ERROR: KGEN works with Python Version 2.7 or later.'
#    sys.exit(-1)

KGEN_SA = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%KGEN_SA
KGEN_BASE = '%s/base'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_SA)

from sa_config import SAConfig
from kgen_utils import Logger, Config, ProgramException, KGGenType
from kgen_tool import KGenTool

class SATool(KGenTool):
    def initialize(self):
        myconfig = SAConfig(KGEN_SA)

        Config.apply(myconfig)

    def transform(self):
        pass

    def output(self):
        pass

        # clean, build and run application

        # analyze output

        # generate result

