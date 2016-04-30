#!/usr/bin/python
'''
This is the main KGEN script.
________________________
Created on Apr 7, 2015

Author: Youngsung Kim <youngsun@ucar.edu>
'''
import sys
import os

# NOTE:
# to move tostring and kgenutils into this tool
# to split Config parameters between base and this tool
# to split command line arguments between base and this tool

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)

KGEN_EXTRACTOR = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%KGEN_EXTRACTOR
KGEN_BASE = '%s/base'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_EXTRACTOR)

from kext_config import KExtConfig
from kgen_utils import Logger, Config, ProgramException, KGGenType
from kgen_state import State
from kgen_genfile import genkobj, gensobj, KERNEL_ID_0, init_plugins, event_register 
from genmake import generate_makefiles
from kgen_tool import KGenTool

class KExtTool(KGenTool):
    def initialize(self):

        myconfig = KExtConfig(KGEN_EXTRACTOR)

        Config.apply(myconfig)

        # create state directories
        if not os.path.exists(Config.path['state']):
            os.makedirs(Config.path['state'])

        # create kernel directories
        if not os.path.exists(Config.path['kernel']):
            os.makedirs(Config.path['kernel'])

    def transform(self):

        # generate kgen_driver.f90 in kernel directory
        self.driver = self.create_tree()
        program = self.create_program(self.driver)
        program.name = self.kernel_name
        self.append_program_in_tree(self.driver, program)

        # init plugin framework
        init_plugins([KERNEL_ID_0])

        # construct a generation tree
        for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
            if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                kfile = genkobj(None, srcobj.tree, KERNEL_ID_0)
                sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                if kfile is None or sfile is None:
                    raise ProgramException('Kernel source file is not generated for %s.'%filepath)
                self.genfiles.append((kfile, sfile, filepath))
                State.used_srcfiles[filepath] = (srcobj, mods_used, units_used)

        # process each nodes in the tree
        for plugin_name in event_register.keys():
            for kfile, sfile, filepath in self.genfiles:
                kfile.created([plugin_name])
                sfile.created([plugin_name])
            for tree in self._trees:
                tree.created([plugin_name])

            for kfile, sfile, filepath in self.genfiles:
                kfile.process([plugin_name])
                sfile.process([plugin_name])
            for tree in self._trees:
                tree.process([plugin_name])

            for kfile, sfile, filepath in self.genfiles:
                kfile.finalize([plugin_name])
                sfile.finalize([plugin_name])
            for tree in self._trees:
                tree.finalize([plugin_name])

            for kfile, sfile, filepath in self.genfiles:
                kfile.flatten(KERNEL_ID_0, [plugin_name])
                sfile.flatten(KERNEL_ID_0, [plugin_name])
            for tree in self._trees:
                tree.flatten(KERNEL_ID_0, [plugin_name])

    def output(self):
                
        # generate source files from each node of the tree
        for kfile, sfile, filepath in self.genfiles:
            filename = os.path.basename(filepath)
            self.set_indent('')
            klines = kfile.tostring()
            if klines is not None:
                with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                    fd.write(klines)

            if sfile.kgen_stmt.used4genstate:
                self.set_indent('')
                slines = sfile.tostring()
                if slines is not None:
                    with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                        fd.write(slines)

        with open('%s/%s.f90'%(Config.path['kernel'], self.kernel_name), 'wb') as fd:
            self.set_indent('')
            lines = self.driver.tostring()
            if lines is not None: fd.write(lines)

        # generate kgen_utils.f90 in kernel directory
        self.generate_kgen_utils()

        generate_makefiles()
        Logger.info('Makefiles are generated', stdout=True)

    def generate_kgen_utils(self):
        from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr, \
            kgen_get_newunit, kgen_error_stop, kgen_utils_file_tostr, kgen_utils_array_sumcheck

        with open('%s/kgen_utils.f90'%Config.path['kernel'], 'wb') as f:
            f.write('MODULE kgen_utils_mod')
            f.write(kgen_utils_file_head)
            f.write('\n')
            f.write('CONTAINS')
            f.write('\n')
            f.write(kgen_utils_array_sumcheck)
            f.write(kgen_utils_file_tostr)
            f.write(kgen_utils_file_checksubr)
            f.write(kgen_get_newunit)
            f.write(kgen_error_stop)
            f.write('END MODULE kgen_utils_mod\n')
