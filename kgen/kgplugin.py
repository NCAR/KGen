# kgen_plugin.py

from kgutils import ProgramException
import collections

class Kgen_Plugin(object):
    plugin_common = collections.OrderedDict()

    def register(self, msg):
        raise ProgramException('Subclass should implement register function')
