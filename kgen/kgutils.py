'''KGen utilities
'''

import sys
import logging
import subprocess

##############################################
# Functions
##############################################

def exit(msg):
    print msg
    sys.exit(-1)

def run_shcmd(cmd, input=None, **kwargs):

    show_error_msg = None
    if kwargs.has_key('show_error_msg'):
        show_error_msg = kwargs['show_error_msg']
        del kwargs['show_error_msg']

    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    out, err = proc.communicate(input=input)

    if proc.returncode != 0 and show_error_msg:
        print '>> %s' % cmd
        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(out)+'\n ERROR: '+str(err)+'\n'

    return out, err, proc.returncode



##############################################
# Logging
##############################################

logger = logging.getLogger()
handler = logging.StreamHandler()
formatter = logging.Formatter('%(levelname)-8s [%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s')
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.DEBUG)
