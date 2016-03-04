# kgen_app.py

from kgen_utils import Logger, UserException, ProgramException, KGGenType
from kgen_state import State
from kgen_analyze import analyze
from kgen_prepost import preprocess, postprocess
from kgen_genfile import genkobj, KERNEL_ID_0, append_item_in_part, UNIT_PART, Gen_Statement
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


