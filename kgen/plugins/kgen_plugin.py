# kgen_plugin.py

from kgen_utils import ProgramException

class Kgen_Plugin(object):
    def register(self, msg):
        raise ProgramException('Subclass should implement register function')
