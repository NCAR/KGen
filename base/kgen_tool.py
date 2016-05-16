# kgen_tool.py

class KGenTool(object):

    def init(self, standalone=True):
        raise Exception('Subclass should implement "init" function.')

    def main(self):
        raise Exception('Subclass should implement "main" function.')

    def fini(self):
        raise Exception('Subclass should implement "fini" function.')
