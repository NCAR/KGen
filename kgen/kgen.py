'''main.py

KGen main function
'''

import os
import sys

from kgconfig import Config
from kggenfile import init_plugins, KERNEL_ID_0

from compflag.main import CompFlag
from parser.main import Parser
from coverage.main import Coverage
from extractor.main import Extractor

def main():

    init_plugins([KERNEL_ID_0])

    # compflag
    compflag = CompFlag()
    compflag.run()


    Config.process_include_option() 
    Config.collect_mpi_params()

    # parse code
    parser = Parser()
    parser.run()

    # coverage
    cover = Coverage()
    cover.run()


    # extract
    ext = Extractor()
    ext.run()

if __name__ == '__main__':
    sys.exit(main)
