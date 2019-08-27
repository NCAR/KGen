
import logging 

class Begin_Source(object):
    pass

logger = logging.getLogger("kgen")

from fparser.two.Fortran2003 import *

__autodoc__ = []
Base_classes = {}
for clsname in dir():
    cls = eval(clsname)
    if isinstance(cls, ClassType) and issubclass(cls, Base) and not cls.__name__.endswith('Base'):
        Base_classes[cls.__name__] = cls
        if len(__autodoc__)<10:
            __autodoc__.append(cls.__name__)

###############################################################################
##################### OPTIMIZE subclass_names tree ############################
###############################################################################

if 1: # Optimize subclass tree:

    def _rpl_list(clsname):
        if clsname not in Base_classes:
            logger.debug('Not implemented: %s' % clsname)
            # print 'Not implemented:',clsname
            return [] # remove this code when all classes are implemented
        cls = Base_classes[clsname]
        if 'match' in cls.__dict__:
            return [clsname]
        l = []
        for n in getattr(cls,'subclass_names',[]):
            l1 = _rpl_list(n)
            for n1 in l1:
                if n1 not in l:
                    l.append(n1)
        return l

    for cls in Base_classes.values():
        if not hasattr(cls, 'subclass_names'): continue
        opt_subclass_names = []
        for n in cls.subclass_names:
            for n1 in _rpl_list(n):
                if n1 not in opt_subclass_names:  opt_subclass_names.append(n1)
        if not opt_subclass_names==cls.subclass_names:
            #print cls.__name__,':',', '.join(cls.subclass_names),'->',', '.join(opt_subclass_names)
            cls.subclass_names[:] = opt_subclass_names
        #else:
        #    print cls.__name__,':',opt_subclass_names


# Initialize Base.subclasses dictionary:
for clsname, cls in Base_classes.items():
    subclass_names = getattr(cls, 'subclass_names', None)
    if subclass_names is None:
        logger.debug('%s class is missing subclass_names list' % (clsname))
        # print '%s class is missing subclass_names list' % (clsname)
        continue
    try:
        l = Base.subclasses[clsname]
    except KeyError:
        Base.subclasses[clsname] = l = []
    for n in subclass_names:
        if n in Base_classes:
            l.append(Base_classes[n])
        else:
            logger.debug('%s not implemented needed by %s' % (n,clsname))
            # print '%s not implemented needed by %s' % (n,clsname)

if 1:
    for cls in Base_classes.values():
        subclasses = Base.subclasses.get(cls.__name__,[])
        subclasses_names = [c.__name__ for c in subclasses]
        subclass_names = getattr(cls,'subclass_names', [])
        use_names = getattr(cls,'use_names',[])
        for n in subclasses_names:
            break
            if n not in subclass_names:
                logger.debug('%s needs to be added to %s subclasses_name list' % (n,cls.__name__))
                # print '%s needs to be added to %s subclasses_name list' % (n,cls.__name__)
        for n in subclass_names:
            break
            if n not in subclasses_names:
                logger.debug('%s needs to be added to %s subclass_name list' % (n,cls.__name__))
                # print '%s needs to be added to %s subclass_name list' % (n,cls.__name__)
        for n in use_names + subclass_names:
            if n not in Base_classes:
                logger.debug('%s not defined used by %s' % (n, cls.__name__))
                # print '%s not defined used by %s' % (n, cls.__name__)


