'''KGen tool base class
'''

import os
from kgconfig import Config
try:
    import configparser
except:
    import ConfigParser as configparser

GEN = 'general'

class KGTool(object):

    def __init__(self):
        pass

    def run(self):
        raise Exception('"%s" should implement "run" method.'%self.__class__.__name__)

class KGModelingTool(object):

    def hasmodel(self, modeltype):

        modelfile = '%s/%s'%(Config.path['outdir'], Config.modelfile)
 
        if not os.path.exists(modelfile):
            return False

        has_general = False
        has_modeltype = False
        has_modelsection = False
        section = ''
        required_modelsections = []
        model_sections = {}
        with open(modelfile, 'r') as mf:

            for line in mf.readlines():
                if line.startswith('['):
                    pos = line.find(']')
                    if pos > 0:
                        section = line[1:pos].strip()
                        if section == 'general':
                            has_general = True
                        else:
                            mtype, msec = section.split('.')
                            if mtype not in model_sections:
                                model_sections[mtype] = []
                            model_sections[mtype].append(msec)
                elif section == 'general' and line.find('=') > 0:
                    mtype, msections = line.split('=') 
                    if mtype.strip() == modeltype:
                        required_modelsections = [s.strip() for s in msections.split(',')]
                        has_modeltype = True
                if has_modeltype and modeltype in model_sections and all( (msec in model_sections[modeltype]) for msec in required_modelsections):
                    has_modelsection = True

                if has_general and has_modeltype and has_modelsection:
                    break
#        cfg = configparser.ConfigParser()
#        cfg.optionxform = str
#        
#        with open(modelfile, 'r') as mf:
#            if not cfg.read(mf):
#                return False
#
#            if not cfg.has_section(GEN):
#                return False
#
#            if not cfg.has_option(GEN, modeltype):
#                return False
#
#            for subsec in [ s.strip() for s in cfg.get(GEN, modeltype).split(',') ]:
#                if not cfg.has_section('%s.%s'%(modeltype, subsec)):
#                    return False

        return has_general and has_modeltype and has_modelsection

    def addmodel(self, modeltype, sections):
         
        modelfile = '%s/%s'%(Config.path['outdir'], Config.modelfile)

        mode = 'r+'
        if not os.path.exists(modelfile):
            mode = 'w+'

        with open(modelfile, mode) as mf:
            mf.seek(0, os.SEEK_END)
            size = mf.tell()
            if size == 0:
                mf.write('; KGen Model Data File\n')

        cfg = configparser.ConfigParser()
        cfg.optionxform = str
        cfg.read(modelfile)

        if not cfg.has_section(GEN):
            cfg.add_section(GEN)

        if not cfg.has_option(GEN, modeltype):
            cfg.set(GEN, modeltype, ', '.join(sections))
#
#        for sec in sections:
#            secname = '%s.%s'%(modeltype, sec)
#            if not cfg.has_section(secname):
#                cfg.add_section(secname)
       
        with open(modelfile, mode) as mf:
            cfg.write(mf)


    def addsection(self, modeltype, section, options):
         
        modelfile = '%s/%s'%(Config.path['outdir'], Config.modelfile)

        mode = 'r+'
        if not os.path.exists(modelfile):
            raise Exception('Modelfile does not exists: %s'%modelfile)

        cfg = configparser.ConfigParser()
        cfg.optionxform = str
        cfg.read(modelfile)
 
        subsec = '%s.%s'%(modeltype, section)
        if cfg.has_section(subsec):
            raise Exception('Section already exists: %s'%subsec)

        cfg.add_section(subsec)

        for opt, val in options:
            cfg.set(subsec, opt, val)
       
        with open(modelfile, mode) as mf:
            cfg.write(mf)
