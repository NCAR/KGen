# kgen_plugin.py

from kgen_utils import ProgramException

class Kgen_Plugin(object):
    version = 0.1
    priority = [ 'gencore' ]

    def register(self, msg):
        raise ProgramException('Subclass should implement register function')
