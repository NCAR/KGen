'''main.py

KGen main function
'''

import os
import sys

from kgconfig import Config
from compflag import CompFlag
from parser import Parser
from coverage import Coverage
from extractor import Extractor

def main():

    # parse arguments
    cfg = Config(os.getcwd())

    # compflag
    compflag = CompFlag(cfg)
    compflag.run()

    cfg.process_include_option() 

    # parse code
    parser = Parser(cfg)
    parser.run()

    # coverage
    cover = Coverage(cfg)
    cover.run()


    # extract
    ext = Extractor(cfg)
    ext.run()

if __name__ == '__main__':
    sys.exit(main)
