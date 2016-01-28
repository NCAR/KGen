# kgen_app.py

from kgen_utils import Logger, UserException, ProgramException, KGGenType
from kgen_state import State
from kgen_analyze import analyze
from kgen_prepost import preprocess, postprocess
from kgen_genfile import genkobj, gensobj, KERNEL_ID_0, append_item_in_part, UNIT_PART, init_plugins, \
    event_register, Gen_Statement
import block_statements

class KGenApp(object):
    def execute(self):

        self._trees = []
        self.genfiles = []
        self.kernel_name = State.kernel_driver['name']
          
        try:
            print ''

            self.initialize()

            preprocess()
            Logger.info('Pre-processing is done', stdout=True)

            analyze()
            Logger.info('Program is analyzed', stdout=True)

            self.transform()

            self.output()

            postprocess()
            Logger.info('Post-processing is done', stdout=True)

            Logger.info('Completed.', stdout=True)
        except UserException as e:
            print 'ERROR: %s'%str(e)
            Logger.info(e)
            #Logger.critical(e)
        except ProgramException as e:
            Logger.critical(e)
        except Exception as e:
            Logger.critical(e)
        finally:
            pass
    def apply_plugins(self):

        # setup plugin framework
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


