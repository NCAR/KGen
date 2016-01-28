#!/usr/bin/python
'''
This is the main Coverage detector  script.
Author: Youngsung Kim <youngsun@ucar.edu>
'''
import sys
import os


# Python version check
#if sys.hexversion < 0x020600F0:
#    print 'ERROR: KGEN works with Python Version 2.7 or later.'
#    sys.exit(-1)

KGEN_HOME = '%s/../..'%os.path.dirname(os.path.realpath(__file__))
KGEN_BASE = '%s/base'%KGEN_HOME
KGEN_CDETECT = '%s/kgentools/coverage_detector'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_CDETECT)

from kgen_utils import Logger, Config
from genfile import generate_srcfiles
from genmake import generate_makefiles
from kgen_app import KGenApp

class CDetectApp(KGenApp):
    def initialize(self):
        # create state directories
        if not os.path.exists(Config.path['state']):
            os.makedirs(Config.path['state'])

    def transform(self):

        Config.plugin['priority']['ext.core'] = '%s/plugins/core'%KGEN_CDETECT

        self.apply_plugins()

    def output(self):
                
        # generate source files from each node of the tree
        for kfile, sfile, filepath in self.genfiles:
            filename = os.path.basename(filepath)
            self.set_indent('')

            if sfile.kgen_stmt.used4genstate:
                self.set_indent('')
                slines = sfile.tostring()
                if slines is not None:
                    with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                        fd.write(slines)
