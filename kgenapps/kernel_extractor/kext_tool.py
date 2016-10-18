#!/usr/bin/python
'''
This is the main KGEN script.
________________________
Created on Apr 7, 2015

Author: Youngsung Kim <youngsun@ucar.edu>
'''
import sys
import os
import shutil

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
TPROF = 'tprof_mod.f90'

sys.path.insert(0, KGEN_BASE)
sys.path.insert(0, KGEN_EXTRACTOR)

import block_statements
from kext_config import KExtConfig
from kgen_utils import Config, Logger, UserException, ProgramException, KGGenType 
from kgen_state import State
from kgen_analyze import analyze 
from kgen_genfile import genkobj, gensobj, KERNEL_ID_0, init_plugins, event_register, \
    append_item_in_part, UNIT_PART, Gen_Statement
from kgen_prepost import preprocess, postprocess 
from genmake import generate_makefiles
from kgen_tool import KGenTool

class KExtTool(KGenTool):

    def init(self, argv=None):

        self._trees = []
        self.genfiles = []
        self.kernel_name = State.kernel_driver['name']
        self.config= KExtConfig(argv=argv)

        Config.register(self.config)

    def main(self):

        Logger.info('Starting KExt', stdout=True)

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


    def fini(self):
               
        kernel_files = []
        state_files = []

        # generate source files from each node of the tree
        for kfile, sfile, filepath in self.genfiles:
            filename = os.path.basename(filepath)
            self.set_indent('')
            klines = kfile.tostring()
            if klines is not None:
                klines = self.remove_multiblanklines(klines)
                kernel_files.append(filename)
                with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                    fd.write(klines)

            if sfile.kgen_stmt.used4genstate:
                self.set_indent('')
                slines = sfile.tostring()
                if slines is not None:
                    slines = self.remove_multiblanklines(slines)
                    state_files.append(filename)
                    with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                        fd.write(slines)

        kernel_files.append(self.kernel_name)
        with open('%s/%s.f90'%(Config.path['kernel'], self.kernel_name), 'wb') as fd:
            self.set_indent('')
            lines = self.driver.tostring()
            if lines is not None:
                lines = self.remove_multiblanklines(lines)
                fd.write(lines)

        Logger.info('Kernel generation and instrumentation is completed.', stdout=True)

        # generate kgen_utils.f90 in kernel directory
        kernel_files.append('kgen_utils.f90')
        self.generate_kgen_utils()

        kernel_files.append(TPROF)
        shutil.copyfile('%s/%s'%(KGEN_BASE, TPROF), '%s/%s'%(Config.path['kernel'], TPROF))

        kernel_files.append('Makefile')
        state_files.append('Makefile')
        generate_makefiles()
        Logger.info('Makefiles are generated', stdout=True)
 
        postprocess()
        Logger.info('Post-processing is done', stdout=True)

        Logger.info('KExt is finished.', stdout=True)

        return { 'kernel_files': kernel_files, 'state_files': state_files }

    def generate_kgen_utils(self):
        from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr, \
            kgen_get_newunit, kgen_error_stop, kgen_utils_file_tostr, kgen_utils_array_sumcheck, \
            kgen_rankthread

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
            f.write(kgen_rankthread)
            f.write('END MODULE kgen_utils_mod\n')

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
