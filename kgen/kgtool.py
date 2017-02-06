'''KGen tool base class
'''


class KGTool(object):

    def __init__(self):
        pass

    def run(self):
        raise Exception('"%s" should implement "run" method.'%self.__class__.__name__)
