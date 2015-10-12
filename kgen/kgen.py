#!/usr/bin/env python
'''
This is the main KGEN script.
________________________
Created on Apr 7, 2015

Author: Youngsung Kim <youngsun@ucar.edu>
'''

from kgen_utils import Logger, UserException, ProgramException
from kgen_analyze import locate_callsite, collect_kernel_info
from kgen_geninfo import mark_generation_info
from kgen_genkernel import generate_kernel
from kgen_genstate import generate_state
from kgen_genmake import generate_makefiles
from kgen_prepost import preprocess, postprocess

def main():
    preprocess()
    Logger.info('Pre-processing is done', stdout=True)

    locate_callsite()
    Logger.info('Call-site location is found', stdout=True)

    collect_kernel_info()
    Logger.info('Kernel information is collected', stdout=True)

    mark_generation_info()
    Logger.info('Kernel generation information is marked')

    generate_state()
    Logger.info('Instrumented files are generated', stdout=True)

    generate_kernel()
    Logger.info('Kernel files are generated', stdout=True)

    generate_makefiles()
    Logger.info('Makefiles are generated', stdout=True)

    postprocess()
    Logger.info('Post-processing is done', stdout=True)

    Logger.info('Completed.', stdout=True)

if __name__ == "__main__":

    try:
        print ''
        main()
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
