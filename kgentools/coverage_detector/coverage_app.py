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

from api import walk
from kgen_utils import Logger, Config, ProgramException, KGGenType
from kgen_state import State
from kgen_genfile import genkobj, gensobj, KERNEL_ID_0, init_plugins, event_register
from genmake import generate_makefiles
from kgen_app import KGenApp

class CDetectApp(KGenApp):
    def initialize(self):
        # create state directories
        if not os.path.exists(Config.path['state']):
            os.makedirs(Config.path['state'])

    def transform(self):

        Config.plugin['priority']['cov.core'] = '%s/plugins/core'%KGEN_CDETECT

        # init plugin framework
        init_plugins([KERNEL_ID_0])

        # construct a generation tree
        for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
            if srcobj.tree == State.topblock['stmt'].top:
                callsite_sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                self.genfiles.append((callsite_sfile, filepath))
                State.used_srcfiles[filepath] = (srcobj, mods_used, units_used)

        # process each nodes in the tree
        for plugin_name in event_register.keys():
            for obj, filepath in self.genfiles:
                obj.created([plugin_name])
            for tree in self._trees:
                tree.created([plugin_name])

            for obj, filepath in self.genfiles:
                obj.process([plugin_name])
            for tree in self._trees:
                tree.process([plugin_name])

            for obj, filepath in self.genfiles:
                obj.finalize([plugin_name])
            for tree in self._trees:
                tree.finalize([plugin_name])

            for obj, filepath in self.genfiles:
                obj.flatten(KERNEL_ID_0, [plugin_name])
            for tree in self._trees:
                tree.flatten(KERNEL_ID_0, [plugin_name])

    def output(self):

        # generate source files from each node of the tree
        for obj, filepath in self.genfiles:
            filename = os.path.basename(filepath)
            self.set_indent('')
            lines = obj.tostring()
            if lines is not None:
                with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                    fd.write(lines)

        Logger.info('Makefiles are generated', stdout=True)
