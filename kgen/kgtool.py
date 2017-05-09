'''KGen tool base class
'''


class KGTool(object):

    def __init__(self):
        pass

    def run(self):
        raise Exception('"%s" should implement "run" method.'%self.__class__.__name__)

class KGModelingTool(object):

    def hasmodel(self, modelfile, modeltype):
        if not os.path.exists(modelfile):
            return False

        cfg = configparser.ConfigParser()
        cfg.optionxform = str
        
        if not cfg.read(modelfile):
            return False

        if not cfg.has_section('general'):
            return False

        if not cfg.has_option('general', modeltype):
            return False

        for subsec in [ s.strip() for s in cfg.get('general', modeltype).split(',') ]:
            if not cfg.has_section('%s.%s'%(modeltype, subsec)):
                return False

        return True

    def addsection(self, 
