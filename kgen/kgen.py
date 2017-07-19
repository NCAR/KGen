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
#from papicounter.main import PapiCounter
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

    # create model directory
    model_realpath = os.path.realpath('%s/%s'%(Config.path['outdir'], Config.path['model']))
    if not os.path.exists(model_realpath):
        os.makedirs(model_realpath)
    if not os.path.exists('%s/__data__'%model_realpath):
        os.makedirs('%s/__data__'%model_realpath)
    if not os.path.exists('%s/__data__/__resource__'%model_realpath):
        os.makedirs('%s/__data__/__resource__'%model_realpath)

#    # model-coverage
#    if 'coverage' in Config.skip:
#        logger.info('Skipped coverage.') 
#    else:
#        cover = Coverage()
#        cover.run()

    # model-etime
    if 'elapsedtime' in Config.skip:
        logger.info('skipped elapsedtime.') 
    else:
        etime = ElapsedTime()
        etime.run()

#    # model-papi
#    if 'papi' in Config.skip:
#        logger.info('skipped papi.') 
#    else:
#        pass
#        papi = PapiCounter()
#        papi.run()

    if len(Config.invocation['triples']) == 0:
        Config.invocation['triples'].append( ( ('0', '0'), ('0', '0'), ('0', '0') ) )

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
