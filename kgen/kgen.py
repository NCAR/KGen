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
from elapsedtime.main import ElapsedTime
from papicounter.main import PapiCounter
from extractor.main import Extractor

# increase recursion limit
sys.setrecursionlimit(2000)

def main():

    Config.parse()

    # compflag
    compflag = CompFlag()
    compflag.run()

    Config.process_include_option() 
    Config.collect_mpi_params()

    # parse code
    parser = Parser()
    parser.run()

    # init plugins
    init_plugins([KERNEL_ID_0])

    # create model directory
    model_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['model']))
    if not os.path.exists(model_realpath):
        os.makedirs(model_realpath)
    if not os.path.exists('%s/__data__'%model_realpath):
        os.makedirs('%s/__data__'%model_realpath)
    if not os.path.exists('%s/__data__/__resource__'%model_realpath):
        os.makedirs('%s/__data__/__resource__'%model_realpath)

    # model-coverage
    if not Config.model['types']['code']['enabled']:
        logger.info('Disabled code coverage.') 
    else:
        cover = Coverage()
        cover.run()

    # model-etime
    if not Config.model['types']['etime']['enabled']:
        logger.info('Disabled elapsedtime.') 
    else:
        etime = ElapsedTime()
        etime.run()

    # model-papi
    if not Config.model['types']['papi']['enabled']:
        logger.info('Disabled papi.') 
    else:
        papi = PapiCounter()
        papi.run()

    if len(Config.invocation['triples']) == 0:
        Config.invocation['triples'].append( ( ('0', '0'), ('0', '0'), ('1', '1') ) )

    # extract
    ext = Extractor()
    ext.run()

if __name__ == '__main__':
    try:
        sys.exit(main)
    except UserException as e:
        logger.critical(str(e))
    except:
        raise
