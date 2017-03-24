'''main.py

KGen main function
'''

import os
import sys

from kgconfig import Config
from kgutils import logger, UserException
from kggenfile import init_plugins, KERNEL_ID_0

from compflag.main import CompFlag
from parser.main import Parser
from coverage.main import Coverage
from extractor.main import Extractor

def main():

    Config.parse()

    # compflag
    if 'compflag' in Config.skip:
        logger.info('Skipped compflag.') 
    else:
        compflag = CompFlag()
        compflag.run()

    Config.process_include_option() 
    Config.collect_mpi_params()

    # parse code
    if 'parse' in Config.skip:
        logger.info('Skipped parse.') 
    else:
        parser = Parser()
        parser.run()

    # init plugins
    init_plugins([KERNEL_ID_0])

    # coverage
    if 'coverage' in Config.skip:
        logger.info('Skipped coverage.') 
    else:
        cover = Coverage()
        cover.run()

    # extract
    if 'extract' in Config.skip:
        logger.info('Skipped extract.') 
    else:
        ext = Extractor()
        ext.run()

if __name__ == '__main__':
    try:
        sys.exit(main)
    except UserException as e:
        logger.critical(str(e))
    except:
        raise
