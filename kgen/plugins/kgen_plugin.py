# kgen_plugin.py

from kgen_utils import ProgramException
from ordereddict import OrderedDict

class Kgen_Plugin(object):
    version = 0.1

    #priority = [ 'gencore', 'verification', 'debug' ]
    priority = [ 'gencore', 'verification', 'simple_timing', 'perturb' ]

    plugin_common = OrderedDict()

    def register(self, msg):
        raise ProgramException('Subclass should implement register function')
