'''
This is the main KGEN script.
________________________
Created on Apr 7, 2015

Author: Youngsung Kim <youngsun@ucar.edu>
'''

# NOTES:
#
# remove kernel driver file. Instead, put program in callsite file
# specifing multiple callsites: command line method) filepath:name1:name2,name3:name4 filepath2:100-200
# directive method) !$kgen callsite_begin <kernel name>
# directive method) !$kgen callsite_end [kernel name]
# collect multiple callsites in Config
# format of geninfo [ kernel number id, state type, ... ]
# kgen analyze steps: 1) resolve with general state type
# 2) find out state type per each name in callsite lines
# 3) update state types in geninfo dict
# directory structure: many kernels in kernels directory
#
# if topblock is module, change it to program and call parent of callsite
# put file io directly in parent of callsite block
# if topblock is program, keep origianl code as much as possible

# Python version check
import sys
if sys.hexversion < 0x020700F0:
    print 'ERROR: KGEN works with Python Version 2.7 or later.'
    sys.exit(-1)

from kgen_utils import Logger, UserException, ProgramException
from kgen_analyze import locate_callsite, collect_kernel_info
from kgen_geninfo import mark_generation_info
#from kgen_genkernel import generate_kernel
#from kgen_genstate import generate_state
from kgen_genfile import generate_srcfiles
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

    generate_srcfiles()
    Logger.info('Source files are generated', stdout=True)

#    generate_state()
#    Logger.info('Instrumented files are generated', stdout=True)
#
#    generate_kernel()
#    Logger.info('Kernel files are generated', stdout=True)

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
