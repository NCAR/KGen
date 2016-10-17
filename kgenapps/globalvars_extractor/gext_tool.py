#!/usr/bin/python
'''
This is the main GExt script.
________________________
Created on Apr 7, 2015

Author: Youngsung Kim <youngsun@ucar.edu>
'''
import sys
import os
import shutil

# Python version check
if sys.hexversion < 0x020700F0:
    print 'ERROR: GExt works with Python Version 2.7 or later.'
    sys.exit(-1)

GVAR_EXTRACTOR = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%GVAR_EXTRACTOR
KGEN_BASE = '%s/base'%KGEN_HOME
TPROF = 'tprof_mod.f90'

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, GVAR_EXTRACTOR)

import block_statements
from gext_config import GExtConfig
from kgen_utils import Config, Logger, UserException, ProgramException, KGGenType 
from kgen_state import State
from kgen_analyze import analyze 
from kgen_genfile import genkobj, gensobj, KERNEL_ID_0, init_plugins, event_register, \
    append_item_in_part, UNIT_PART, Gen_Statement
from kgen_prepost import preprocess, postprocess 
from genmake import generate_makefiles
from kgen_tool import KGenTool

class GExtTool(KGenTool):

    def init(self, argv=None):

        self._trees = []
        self.genfiles = []
        self.kernel_name = State.kernel_driver['name']
        self.config= GExtConfig(argv=argv)

        Config.register(self.config)

    def main(self):

        Logger.info('Starting GExt', stdout=True)

        # create state directories
        if not os.path.exists(Config.path['state']):
            os.makedirs(Config.path['state'])

        # create kernel directories
        if not os.path.exists(Config.path['kernel']):
            os.makedirs(Config.path['kernel'])

        os.system('rm -f %s/kgen_statefile.lst'%Config.path['kernel'])
        os.system('rm -f %s/done.*'%Config.path['kernel'])

        preprocess()
        Logger.info('Pre-processing is done', stdout=True)
    
        analyze()
        Logger.info('Program is analyzed', stdout=True)

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
                sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                if sfile is None:
                    raise ProgramException('Kernel source file is not generated for %s.'%filepath)
                self.genfiles.append((sfile, filepath))
                State.used_srcfiles[filepath] = (srcobj, mods_used, units_used)

        # process each nodes in the tree
        for plugin_name in event_register.keys():
            for sfile, filepath in self.genfiles:
                sfile.created([plugin_name])
            for tree in self._trees:
                tree.created([plugin_name])

            for sfile, filepath in self.genfiles:
                sfile.process([plugin_name])
            for tree in self._trees:
                tree.process([plugin_name])

            for sfile, filepath in self.genfiles:
                sfile.finalize([plugin_name])
            for tree in self._trees:
                tree.finalize([plugin_name])

            for sfile, filepath in self.genfiles:
                sfile.flatten(KERNEL_ID_0, [plugin_name])
            for tree in self._trees:
                tree.flatten(KERNEL_ID_0, [plugin_name])


    def fini(self):
               
        state_files = []

        # generate source files from each node of the tree
        for sfile, filepath in self.genfiles:
            if sfile.kgen_stmt.used4genstate:
                filename = os.path.basename(filepath)
                self.set_indent('')
                slines = sfile.tostring()
                if slines is not None:
                    slines = self.remove_multiblanklines(slines)
                    state_files.append(filename)
                    with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                        fd.write(slines)

        Logger.info('Global variable information generation and instrumentation is completed.', stdout=True)

        state_files.append('Makefile')
        generate_makefiles()
        Logger.info('Makefiles are generated', stdout=True)
 
        postprocess()
        Logger.info('Post-processing is done', stdout=True)

        Logger.info('GExt is finished.', stdout=True)

        return { 'state_files': state_files }

    def create_tree(self):
        tree = genkobj(None, block_statements.BeginSource, KERNEL_ID_0)
        self._trees.append(tree)
        return tree

    def create_program(self, parent):
        return genkobj(parent, block_statements.Program, KERNEL_ID_0)

    def append_program_in_tree(self, driver, program):
        append_item_in_part(driver, UNIT_PART, program)

    def set_indent(self, indent):
        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}

    def remove_multiblanklines(self, text):
        MAXBLANKLINES = 3
        lines = text.split('\n')
        newlines = []
        count = 0
        for line in lines:
            if len(line)>0:
                newlines.append(line)
                count = 0
            else:
                count += 1
                if count < MAXBLANKLINES:
                    newlines.append(line)

        return '\n'.join(newlines)
