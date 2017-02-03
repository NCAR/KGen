'''KGen tool base class
'''


class KGTool(object):

    plugins = []

    def __init__(self, cfg, plugins=[]):
        self.cfg = cfg
        self.plugins.extend(plugins)

    def run(self):
        raise Exception('"%s" should implement "run" method.'%self.__class__.__name__)
