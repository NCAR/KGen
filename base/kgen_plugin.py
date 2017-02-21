# kgen_plugin.py

from kgen_utils import ProgramException
from collections import OrderedDict

class Kgen_Plugin(object):
    plugin_common = OrderedDict()

    def register(self, msg):
        raise ProgramException('Subclass should implement register function')
