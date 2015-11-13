"""
-----
Permission to use, modify, and distribute this software is given under the
terms of the NumPy License. See http://scipy.org.

NO WARRANTY IS EXPRESSED OR IMPLIED.  USE AT YOUR OWN RISK.
Author: Pearu Peterson <pearu@cens.ioc.ee>
Created: May 2006
-----
"""

__all__ = ['Statement','BeginStatement','EndStatement', 'Variable',
           'AttributeHolder','ProgramBlock']

import re
import sys
import copy
import logging
from readfortran import Line, Comment
#from numpy.distutils.misc_util import yellow_text, red_text # KGEN deletion
from utils import split_comma, specs_split_comma, is_int_literal_constant
from utils import classes

#logger = logging.getLogger('fparser') # KGEN deletion
logger = logging.getLogger('kgen') # KGEN addition

import Fortran2003 # KGEN addition
from kgen_utils import KGName, Logger, ProgramException # KGEN addition

class AttributeHolder(object):
    # copied from symbolic.base module
    """
    Defines a object with predefined attributes. Only those attributes
    are allowed that are specified as keyword arguments of a constructor.
    When an argument is callable then the corresponding attribute will
    be read-only and set by the value the callable object returns.
    """
    def __init__(self, **kws):
        self._attributes = {}
        self._readonly = []
        for k,v in kws.items():
            self._attributes[k] = v
            if callable(v):
                self._readonly.append(k)
        return

    def __getattr__(self, name):
        if name not in self._attributes:
            raise AttributeError,'%s instance has no attribute %r, '\
                  'expected attributes: %s' \
                  % (self.__class__.__name__,name,
                     ','.join(self._attributes.keys()))
        value = self._attributes[name]
        if callable(value):
            value = value()
            self._attributes[name] = value
        return value

    def __setattr__(self, name, value):
        if name in ['_attributes','_readonly']:
            self.__dict__[name] = value
            return
        if name in self._readonly:
            raise AttributeError,'%s instance attribute %r is readonly' \
                  % (self.__class__.__name__, name)
        if name not in self._attributes:
            raise AttributeError,'%s instance has no attribute %r, '\
                  'expected attributes: %s' \
                  % (self.__class__.__name__,name,','.join(self._attributes.keys()))
        self._attributes[name] = value

    def isempty(self):
        for k in self._attributes.keys():
            v = getattr(self,k)
            if v: return False
        return True

    def __repr__(self): return self.torepr()

    def torepr(self, depth=-1, tab = ''):
        if depth==0: return tab + self.__class__.__name__
        l = [self.__class__.__name__+':']
        ttab = tab + '    '
        for k in self._attributes.keys():
            v = getattr(self,k)
            if v:
                if isinstance(v,list):
                    l.append(ttab + '%s=<%s-list>' % (k,len(v)))
                elif isinstance(v,dict):
                    l.append(ttab + '%s=<dict with keys %s>' % (k,v.keys()))
                else:
                    l.append(ttab + '%s=<%s>' % (k,type(v)))
        return '\n'.join(l)

    def todict(self):
        d = {}
        for k in self._attributes.keys():
            v = getattr(self, k)
            d[k] = v
        return d

def get_base_classes(cls):
    bases = ()
    for c in cls.__bases__:
        bases += get_base_classes(c)
    return bases + cls.__bases__ + (cls,)

class Variable(object):
    """
    Variable instance has attributes:
      name
      typedecl
      dimension
      attributes
      intent
      parent - Statement instances defining the variable
    """

    __metaclass__ = classes
    
    def __init__(self, parent, name):
        self.parent = parent
        self.parents = [parent]
        self.name = name
        self.typedecl = None
        self.dimension = None
        self.bounds = None
        self.length = None
        self.attributes = []
        self.intent = None
        self.bind = []
        self.check = []
        self.init = None

        # after calling analyze the following additional attributes are set:
        # .is_array:
        #    rank
        #    shape
        return

    def __repr__(self):
        l = []
        for a in ['name','typedecl','dimension','bounds','length','attributes','intent','bind','check','init']:
            v = getattr(self,a)
            if v:
                l.append('%s=%r' % (a,v))
        return 'Variable: ' + ', '.join(l)

    def get_bit_size(self):
        typesize = self.typedecl.get_bit_size()
        if self.is_pointer():
            # The size of pointer descriptor is compiler version dependent. Read:
            #   http://www.nersc.gov/vendor_docs/intel/f_ug1/pgwarray.htm
            #   https://www.cca-forum.org/pipermail/cca-fortran/2003-February/000123.html
            #   https://www.cca-forum.org/pipermail/cca-fortran/2003-February/000122.html
            # On sgi descriptor size may be 128+ bits!
            if self.is_array():
                wordsize = 4 # XXX: on a 64-bit system it is 8.
                rank = len(self.bounds or self.dimension)
                return 6 * wordsize + 12 * rank
            return typesize
        if self.is_array():
            size = reduce(lambda x,y:x*y,self.bounds or self.dimension,1)
            if self.length:
                size *= self.length
            return size * typesize
        if self.length:
            return self.length * typesize
        return typesize

    def get_typedecl(self):
        if self.typedecl is None:
            self.set_type(self.parent.get_type(self.name))
        return self.typedecl

    def add_parent(self, parent):
        if id(parent) not in map(id, self.parents):
            self.parents.append(parent)
        self.parent = parent
        return

    def set_type(self, typedecl):
        if self.typedecl is not None:
            if not self.typedecl==typedecl:
                self.parent.warning(\
                    'variable %r already has type %s,'\
                    ' resetting to %s' \
                    % (self.name, self.typedecl.tostr(),typedecl.tostr()))
        assert typedecl is not None
        self.typedecl = typedecl
        return

    def set_init(self, expr):
        if self.init is not None:
            if not self.init==expr:
                self.parent.warning(\
                    'variable %r already has initialization %r, '\
                    ' resetting to %r' % (self.name, self.expr, expr))
        self.init = expr
        return

    def set_dimension(self, dims):
        dims = [tuple(dim.split(':')) for dim in dims]
        dims = [tuple(map(str.strip, dim)) for dim in dims]
        if self.dimension is not None:
            if not self.dimension==dims:
                self.parent.warning(\
                    'variable %r already has dimension %r, '\
                    ' resetting to %r' % (self.name, self.dimension, dims))
        self.dimension = dims
        return

    def set_bounds(self, bounds):
        if self.bounds is not None:
            if not self.bounds==bounds:
                self.parent.warning(\
                    'variable %r already has bounds %r, '\
                    ' resetting to %r' % (self.name, self.bounds, bounds))
        self.bounds = bounds
        return

    def set_length(self, length):
        if self.length is not None:
            if not self.length==length:
                self.parent.warning(\
                    'variable %r already has length %r, '\
                    ' resetting to %r' % (self.name, self.length, length))
        self.length = length
        return

    known_intent_specs = ['IN','OUT','INOUT','CACHE','HIDE', 'COPY',
                          'OVERWRITE', 'CALLBACK', 'AUX', 'C', 'INPLACE',
                          'OUT=']

    def set_intent(self, intent):
        if self.intent is None:
            self.intent = []
        for i in intent:
            if i not in self.intent:
                if i not in self.known_intent_specs:
                    self.parent.warning('unknown intent-spec %r for %r'\
                                        % (i, self.name))
                self.intent.append(i)
        return

    known_attributes = ['PUBLIC', 'PRIVATE', 'ALLOCATABLE', 'ASYNCHRONOUS',
                        'EXTERNAL', 'INTRINSIC', 'OPTIONAL', 'PARAMETER',
                        'POINTER', 'PROTECTED', 'SAVE', 'TARGET', 'VALUE',
                        'VOLATILE', 'REQUIRED']

    def is_intent_in(self):
        #if not self.intent: return True # KGEN deletion
        if not self.intent: return False # KGEN addition
        # start of KGEN    
        #if not self.intent:
        #    anc = self.parent.ancestors()[-1]
        #    if hasattr(anc, 'args') and self.name in anc.args: return True
        #    else: return False
        # end of KGEN
        if 'HIDE' in self.intent: return False
        if 'INPLACE' in self.intent: return False
        if 'IN' in self.intent: return True
        if 'OUT' in self.intent: return False
        if 'INOUT' in self.intent: return False
        if 'OUTIN' in self.intent: return False
        return True

    def is_intent_inout(self):
        #if not self.intent: return False # KGEN deletion
        # start of KGEN    
        if not self.intent:
            anc = self.parent.ancestors()[-1]
            if hasattr(anc, 'args') and self.name in anc.args: return True
            else: return False
        # end of KGEN

        if 'INOUT' in self.intent:
            if 'IN' in self.intent or 'HIDE' in self.intent or 'INPLACE' in self.intent:
                self.warning('INOUT ignored in INPUT(%s)' % (', '.join(self.intent)))
                return False
            return True
        return False

    def is_intent_hide(self):
        if not self.intent: return False
        if 'HIDE' in self.intent: return True
        if 'OUT' in self.intent:
            return 'IN' not in self.intent and 'INPLACE' not in self.intent and 'INOUT' not in self.intent
        return False

    def is_intent_inplace(self): return self.intent and 'INPLACE' in self.intent
    def is_intent_out(self): return  self.intent and 'OUT' in self.intent
    def is_intent_c(self): return  self.intent and 'C' in self.intent
    def is_intent_cache(self): return  self.intent and 'CACHE' in self.intent
    def is_intent_copy(self): return  self.intent and 'COPY' in self.intent
    def is_intent_overwrite(self): return  self.intent and 'OVERWRITE' in self.intent
    def is_intent_callback(self): return  self.intent and 'CALLBACK' in self.intent
    def is_intent_aux(self): return  self.intent and 'AUX' in self.intent

    def is_private(self):
        from block_statements import BeginSource
        if 'PUBLIC' in self.attributes: return False
        if 'PRIVATE' in self.attributes: return True
        # start of KGEN
#        if isinstance(self.parent.parent, BeginSource):
#            return self.parent.check_private(self.name)
#        else:
#            return self.parent.parent.check_private(self.name)
        if hasattr(self.parent.parent, 'check_private'):
            return self.parent.parent.check_private(self.name)
        else:
            return False
        # end of KGEN
        # return self.parent.parent.check_private(self.name) # KGEN deletion
    def is_public(self): return not self.is_private()

    def is_allocatable(self): return 'ALLOCATABLE' in self.attributes
    def is_external(self): return 'EXTERNAL' in self.attributes
    def is_intrinsic(self): return 'INTRINSIC' in self.attributes
    def is_parameter(self): return 'PARAMETER' in self.attributes
    def is_optional(self): return 'OPTIONAL' in self.attributes and 'REQUIRED' not in self.attributes and not self.is_intent_hide()
    def is_required(self): return self.is_optional() and not self.is_intent_hide()
    def is_pointer(self): return 'POINTER' in self.attributes

    def is_array(self): return not not (self.bounds or self.dimension)
    def is_scalar(self): return not self.is_array()

    def update(self, *attrs):
        attributes = self.attributes
        if len(attrs)==1 and isinstance(attrs[0],(tuple,list)):
            attrs = attrs[0]
        for attr in attrs:
            lattr = attr.lower()
            uattr = attr.upper()
            if lattr.startswith('dimension'):
                assert self.dimension is None, `self.dimension,attr`
                l = attr[9:].lstrip()
                assert l[0]+l[-1]=='()',`l`
                self.set_dimension(split_comma(l[1:-1].strip(), self.parent.item))
                continue
            if lattr.startswith('intent'):
                l = attr[6:].lstrip()
                assert l[0]+l[-1]=='()',`l`
                self.set_intent(specs_split_comma(l[1:-1].strip(),
                                                  self.parent.item, upper=True))
                continue
            if lattr.startswith('bind'):
                l = attr[4:].lstrip()
                assert l[0]+l[-1]=='()',`l`
                self.bind = specs_split_comma(l[1:-1].strip(), self.parent.item,
                                              upper = True)
                continue
            if lattr.startswith('check'):
                l = attr[5:].lstrip()
                assert l[0]+l[-1]=='()',`l`
                self.check.extend(split_comma(l[1:-1].strip(), self.parent.item))
                continue
            if uattr not in attributes:
                if uattr not in self.known_attributes:
                    self.parent.warning('unknown attribute %r' % (attr))
                attributes.append(uattr)
        return

    def __str__(self):
        s = ''
        typedecl = self.get_typedecl()
        if typedecl is not None:
            s += typedecl.tostr() + ' '
        a = self.attributes[:]
        if self.dimension is not None:
            a.append('DIMENSION(%s)' % (', '.join([':'.join(spec) for spec in self.dimension])))
        if self.intent is not None:
            a.append('INTENT(%s)' % (', '.join(self.intent)))
        if self.bind:
            a.append('BIND(%s)' % (', '.join(self.bind)))
        if self.check:
            a.append('CHECK(%s)' % (', '.join(self.check)))
        if a:
            s += ', ' + ', '.join(a) + ' :: '
        s += self.name
        if self.bounds:
            s += '(%s)' % (', '.join([':'.join(spec) for spec in self.bounds]))
        if self.length:
            if is_int_literal_constant(self.length):
                s += '*%s' % (self.length)
            else:
                s += '*(%s)' % (self.length)
        if self.init:
            s += ' = ' + self.init
        return s

    def get_array_spec(self):
        assert self.is_array(),'array_spec is available only for arrays'
        if self.bounds:
            if self.dimension:
                self.parent.warning('both bounds=%r and dimension=%r are defined, ignoring dimension.' % (self.bounds, self.dimension))
            array_spec = self.bounds
        else:
            array_spec = self.dimension
        return array_spec

    def is_deferred_shape_array(self):
        if not self.is_array(): return False
        return self.is_allocatable() or self.is_pointer()

    def is_assumed_size_array(self):
        if not self.is_array(): return False
        return self.get_array_spec()[-1][-1]=='*'

    def is_assumed_shape_array(self):
        if not self.is_array(): return False
        if self.is_deferred_shape_array(): return False
        for spec in self.get_array_spec():
            if not spec[-1]: return True
        return False

    def is_explicit_shape_array(self):
        if not self.is_array(): return False
        if self.is_deferred_shape_array(): return False
        for spec in self.get_array_spec():
            if not spec[-1] or spec[-1] == '*': return False
        return True

    def is_allocatable_array(self):
        return self.is_array() and self.is_allocatable()

    def is_array_pointer(self):
        return self.is_array() and self.is_pointer()

    def analyze(self):
        typedecl = self.get_typedecl()
        if self.is_array():
            array_spec = self.get_array_spec()
            self.rank = len(array_spec)
            if self.is_deferred_shape_array(): # a(:,:)
                pass
            elif self.is_explicit_shape_array():
                shape = []
                for spec in array_spec:
                    if len(spec)==1:
                        shape.append(spec[0])
                    else:
                        try:
                            n = int(spec[1]) - int(spec[0])
                        except ValueError:
                            n = '(%s)-(%s)' % (spec[1], spec[0]) 
                        shape.append(str(n))
                self.shape = shape
        return

    def error(self, message):
        return self.parent.error(message)
    def warning(self, message):
        return self.parent.warning(message)
    def info(self, message):
        return self.parent.info(message)

class ProgramBlock(object):

    __metaclass__ = classes

class Statement(object):
    """
    Statement instance has attributes:
      parent  - Parent BeginStatement or FortranParser instance
      item    - Line instance containing the statement line
      isvalid - boolean, when False, the Statement instance will be ignored
    """
    __metaclass__ = classes

    modes = ['free','fix','f77','pyf']
    _repr_attr_names = []

    def __init__(self, parent, item):
        self.parent = parent
        if item is not None:
            self.reader = item.reader
        else:
            self.reader = parent.reader
        self.top = getattr(parent,'top',None) # the top of statement tree
        self.item = item

        if isinstance(parent, ProgramBlock):
            self.programblock = parent
        elif isinstance(self, ProgramBlock):
            self.programblock = self
        elif hasattr(parent,'programblock'):
            self.programblock = parent.programblock
        else:
            #self.warning('%s.programblock attribute not set.' % (self.__class__.__name__))
            pass

        # when a statement instance is constructed by error, set isvalid to False
        self.isvalid = True
        # when a statement should be ignored, set ignore to True
        self.ignore = False

        # attribute a will hold analyze information.
        a_dict = {}
        for cls in get_base_classes(self.__class__):
            if hasattr(cls,'a'):
                a_dict.update(copy.deepcopy(cls.a.todict()))
        self.a = AttributeHolder(**a_dict)
        if hasattr(self.__class__,'a'):
            assert self.a is not self.__class__.a

        self.process_item()

        return

    def __repr__(self):
        return self.torepr()

    def torepr(self, depth=-1,incrtab=''):
        tab = incrtab + self.get_indent_tab()
        clsname = self.__class__.__name__
        #l = [tab + yellow_text(clsname)] # KGEN deletion
        l = [tab + clsname] # KGEN addition
        if depth==0:
            return '\n'.join(l)
        ttab = tab + '  '
        for n in self._repr_attr_names:
            attr = getattr(self, n, None)
            if not attr: continue
            if hasattr(attr, 'torepr'):
                r = attr.torepr(depth-1,incrtab)
            else:
                r = repr(attr)
            l.append(ttab + '%s=%s' % (n, r))
        if self.item is not None: l.append(ttab + 'item=%r' % (self.item))
        if not self.isvalid: l.append(ttab + 'isvalid=%r' % (self.isvalid))
        if self.ignore: l.append(ttab + 'ignore=%r' % (self.ignore))
        if not self.a.isempty():
            l.append(ttab + 'a=' + self.a.torepr(depth-1,incrtab+'  ').lstrip())
        return '\n'.join(l)

    def get_indent_tab(self,deindent=False,isfix=None):
        if isfix is None: isfix = self.reader.isfixed
        if isfix:
            tab = ' '*6
        else:
            tab = ''
        p = self.parent
        while isinstance(p, Statement):
            tab += '  '
            p = p.parent
        if deindent:
            tab = tab[:-2]
        label = getattr(self.item, 'label', None)
        if label is None:
            return tab
        s = str(label)
        if isfix:
            s = ' '+s
        tab = tab[len(s):]
        if not tab: tab = ' '
        tab = s + tab
        return tab

    def __str__(self):
        return self.tofortran()

    def asfix(self):
        lines = []
        for line in self.tofortran(isfix=True).split('\n'):
            if len(line)>72 and line[0]==' ':
                lines.append(line[:72]+'&\n     &')
                line = line[72:]
                while len(line)>66:
                    lines.append(line[:66]+'&\n     &')
                    line = line[66:]
                lines.append(line+'\n')
            else: lines.append(line+'\n')
        return ''.join(lines).replace('\n     &\n','\n')

    def format_message(self, kind, message):
        if self.item is not None:
            message = self.reader.format_message(kind, message,
                                                 self.item.span[0], self.item.span[1])
        else:
            return message
        return message

    # def show_message(self, message, stream=sys.stderr):
    #     print >> stream, message
    #     stream.flush()
    #     return

    def error(self, message):
        #message = self.format_message('ERROR', red_text(message)) # KGEN deletion
        message = self.format_message('ERROR', message) # KGEN addition
        logger.error(message)
        # self.show_message(message)
        return

    def warning(self, message):
        #message = self.format_message('WARNING', yellow_text(message)) # KGEN deletion
        message = self.format_message('WARNING', message) # KGEN addition
        logger.warning(message)
        # self.show_message(message)
        return

    def info(self, message):
        message = self.format_message('INFO', message)
        logger.info(message)
        # self.show_message(message)
        return

    def analyze(self):
        self.warning('nothing analyzed')
        return

    def get_variable(self, name):
        """ Return Variable instance of variable name.
        """
        mth = getattr(self,'get_variable_by_name', self.parent.get_variable)
        return mth(name)

    def get_type(self, name):
        """ Return type declaration using implicit rules
        for name.
        """
        mth = getattr(self,'get_type_by_name', self.parent.get_type)
        return mth(name)

    def get_type_decl(self, kind):
        mth = getattr(self,'get_type_decl_by_kind', self.parent.get_type_decl)
        return mth(kind)

    def get_provides(self):
        """ Returns dictonary containing statements that block provides or None when N/A.
        """
        return

    # start of KGEN

    def remove_label(self, line):
        # remove label
        _label_re = re.compile(r'\s*(?P<label>\d+)\s*(\b|(?=&)|\Z)',re.I)
        m = _label_re.match(line)
        if m:
            label = int(m.group('label'))
            line = line[m.end():]

        return line

    def remove_construct_name(self, line):
        # remove construct name
        _construct_name_re = re.compile(r'\s*(?P<name>\w+)\s*:\s*(\b|(?=&)|\Z)',re.I)
        m = _construct_name_re.match(line)
        if m:
            name = m.group('name')
            line = line[m.end():].lstrip()

        return line

    def tokgen(self, **kwargs):
        raise ProgramException('%s should implement tokgen().'%self.__class__)
#        from statements import Comment, Where
#
#        if isinstance(self, Comment):
#            return self.tofortran().lstrip()
#        # Temporary fix
#        elif isinstance(self, Where):
#            return self.item.line.lstrip()
#        else:
#            return self.item.apply_map(self.tofortran().lstrip())

    def ancestors(self, include_beginsource=False):
        from block_statements import BeginSource, HasUseStmt, Type

        anc = []

        parent = self.parent
        while not isinstance(parent, BeginSource):
            if isinstance(parent, HasUseStmt) or parent.__class__ in [Type]:
                anc.append(parent)
            parent = parent.parent

        if include_beginsource:
            anc.append(parent)

        anc.reverse()
        return anc
 
    def parse_f2003(self):
        from kgen_utils import traverse
        from block_statements import BeginSource, SubProgramStatement
        from statements import Continue

        if not hasattr(self, 'f2003'):
            if hasattr(self, 'f2003_class'):
                if hasattr(self.item, 'line') and self.item.line:
                    line = self.tokgen()
                    is_escape = False
                    cstr = None
                    epos = len(line)
                    for i, c in enumerate(line):
                        if is_escape:
                            is_escape = False
                            continue
                        if c=='\\': is_escape = True
                        elif c=='!':
                            if cstr is None:
                                epos = i
                                break
                        elif c=="'" or c=='"':
                            if cstr is None: cstr=c
                            elif cstr==c: cstr = None

                    line = line[:epos]

                    line = self.remove_label(line)
                    line = self.remove_construct_name(line)
#                    # remove label
#                    _label_re = re.compile(r'\s*(?P<label>\d+)\s*(\b|(?=&)|\Z)',re.I)
#                    m = _label_re.match(line)
#                    if m:
#                        label = int(m.group('label'))
#                        line = line[m.end():]
#                    # remove construct name
#                    _construct_name_re = re.compile(r'\s*(?P<name>\w+)\s*:\s*(\b|(?=&)|\Z)',re.I)
#                    m = _construct_name_re.match(line)
#                    if m:
#                        name = m.group('name')
#                        line = line[m.end():].lstrip()
                    self.f2003 = self.f2003_class(line)
                elif hasattr(self.item, 'comment'):
                    #self.f2003 = self.f2003_class(self.item.comment)
                    self.f2003 = self.f2003_class()
                elif self.__class__ in [ BeginSource ]:
                    self.f2003 = self.f2003_class()
                else:
                    raise Exception('Not either line or comment: ', str(self), str(self.__class__))

                if self.f2003 is None:
                    raise Exception('None f2003 attribute: ', str(self))

                self.f2003.stmtpair = self
                traverse(self.f2003, self.set_parent, None)
            else:
                raise ProgramException('Class %s does not have f2003_class attribute.' % self.__class__)

    def set_parent(self, node, bag, depth):

        if hasattr(node, 'item') and node.item and isinstance(node.item, Fortran2003.Base):
            node.item.parent = node

        if node and hasattr(node, 'items') and node.items:
            for item in node.items:
                if isinstance(item, list) or isinstance(item, tuple):
                    for subitem in item:
                        if subitem and not isinstance(subitem, str) and \
                            not isinstance(item, list) and not isinstance(item, tuple):
                            subitem.parent = node
                            self.set_parent(subitem, bag, depth+1)
                elif item and  not isinstance(item, str):
                    item.parent = node
                    self.set_parent(item, bag, depth+1)

    def expr_by_name(self, name, node=None):

        if not name:
            raise ProgramException("Callsite name is not found. Please check if callsite is specified correctly.")

        expr = None
        if isinstance(name, str):
            name = KGName(name) 

        if isinstance(node, Fortran2003.Name):
            if name.firstpartname().lower()==node.string.lower():
                if len(name.namelist)>1:
                    ancnames = [ a.name.lower() for a in self.ancestors() ]
                    lennl = len(name.namelist[:-1])
                    if ancnames[-1*lennl:]==name.namelist[:-1]:
                        expr = node.parent
                else:
                    expr = node.parent
        elif isinstance(node, list) or isinstance(node, tuple):
            for item in node:
                if isinstance(item, Fortran2003.Name):
                    if name.firstpartname().lower()==item.string.lower():
                        if len(name.namelist)>1:
                            ancnames = [ a.name.lower() for a in self.ancestors() ]
                            lennl = len(name.namelist[:-1])
                            if ancnames[-1*lennl:]==name.namelist[:-1]:
                                expr = item.parent
                        else:
                            expr = item.parent
                else:
                    expr = self.expr_by_name(name, item)
                    if expr is not None: break
        else:
            if node and hasattr(node, 'items') and node.items:
                for item in node.items:
                    expr = self.expr_by_name(name, item)
                    if expr is not None: break

        return expr

    def can_resolve(self, request):
        from typedecl_statements import TypeDeclarationStatement

        if request is None: return False

        # skip if request is maded by this stmt itself
        if self is request.originator:
            if isinstance(self, TypeDeclarationStatement):
                if request.uname.firstpartname() in [ n.split('=')[0].strip() for n in self.entity_decls ]:
                    return True
            else:
                return False

        # check if name is matched and self is in resolver classes
        if hasattr(self, 'name') and self.name and \
            self.name.lower()==request.uname.firstpartname() and \
            self.__class__ in request.resolvers:
            return True

        return False

    # save names that this self resolved
    def add_geninfo(self, uname, request):

        if uname is None or request is None: return

        # Statement
        if not hasattr(self, 'geninfo'):
            self.geninfo = {}
        if not self.geninfo.has_key(request.gentype):
            self.geninfo[request.gentype] = []
        if (uname, request) not in self.geninfo[request.gentype]:
            self.geninfo[request.gentype].append((uname, request))

        # EndStatement
        if isinstance(self, BeginStatement) and isinstance(self.content[-1], EndStatement):
            if not hasattr(self.content[-1], 'geninfo'):
                self.content[-1].geninfo = {}
            if not self.content[-1].geninfo.has_key(request.gentype):
                self.content[-1].geninfo[request.gentype] = []

        # Ancestors
        for anc in self.ancestors(include_beginsource=True):
            if not hasattr(anc, 'geninfo'):
                anc.geninfo = {}
            if not anc.geninfo.has_key(request.gentype):
                anc.geninfo[request.gentype] = []
            if isinstance(anc, BeginStatement) and isinstance(anc.content[-1], EndStatement):
                if not hasattr(anc.content[-1], 'geninfo'):
                    anc.content[-1].geninfo = {}
                if not anc.content[-1].geninfo.has_key(request.gentype):
                    anc.content[-1].geninfo[request.gentype] = []


    def check_spec_stmts(self, uname, request):
        # the last resolver
        res_stmt = request.res_stmts[-1]
        if not hasattr(res_stmt, 'parent') or not hasattr(res_stmt.parent, 'spec_stmts'):
            return

        for spec_stmt in res_stmt.parent.spec_stmts:
            spec_stmt.resolve_uname(uname, request)


    def get_res_stmts(self, uname):
        if not hasattr(self, 'unknowns'):
            return None

        if isinstance(uname, str): strname = uname
        elif isinstance(uname, KGName): strname = uname.firstpartname()

        for kgname, res in self.unknowns.iteritems():
            if res is None:
                return None
            if kgname.firstpartname()==strname:
                return res.res_stmts
        return None

    def resolve(self, request):
        from kgen_state import ResState, State
        from block_statements import BeginSource
        from typedecl_statements import TypeDeclarationStatement
        from kgen_search import f2003_search_unknowns
        from api import walk

        if request is None: return
        Logger.info('%s is being resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)

        # already resolved
        if request.state == ResState.RESOLVED:
            Logger.info('%s is already resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)
            return

        # check if this stmt can resolve the request
        if self.can_resolve(request):
            if isinstance(self, TypeDeclarationStatement):
                typedecl_stmt  = self
                Logger.info('The request is being resolved by a variable', name=request.uname, stmt=self)
                request.res_stmts.append(typedecl_stmt)
                request.state = ResState.RESOLVED
                typedecl_stmt.add_geninfo(request.uname, request)
                self.check_spec_stmts(request.uname, request)
                Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=typedecl_stmt)

                if not hasattr(typedecl_stmt, 'unknowns'):
                    f2003_search_unknowns(typedecl_stmt, typedecl_stmt.f2003)
                if hasattr(typedecl_stmt, 'unknowns'):
                    for unk, req in typedecl_stmt.unknowns.iteritems():
                        if req.state != ResState.RESOLVED:
                            typedecl_stmt.resolve(req) 

            else:
                Logger.info('%s can be resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)
                raise Exception('This request should have been resolved by the stmt shown below.\n%s'%self.tokgen())

        # ask parent for resolution
        if request.state != ResState.RESOLVED:
            Logger.info('%s is not resolved locally and the request is being defered to parent'%request.uname.firstpartname(), \
                name=request.uname, stmt=self)
            if hasattr(self, 'parent') and not isinstance(self.parent, BeginSource):
                self.parent.resolve(request)


        # if not resolved,
        if request.state != ResState.RESOLVED:
            if self is request.originator:
                # check if program units can resolve the request
                for filepath, units in State.program_units.iteritems():
                    for unit in units:
                        if any( isinstance(unit, resolver) for resolver in request.resolvers) and \
                            hasattr(unit, 'name') and request.uname.firstpartname()==unit.name:
                            Logger.info('The request is being resolved by a program unit', name=request.uname, stmt=unit)
                            if unit not in request.originator.ancestors():
                                request.res_stmts.append(unit)
                                request.state = ResState.RESOLVED
                                unit.add_geninfo(request.uname, request)
                                self.check_spec_stmts(request.uname, request)
                                Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=unit)
                                for _stmt, _depth in walk(unit, -1):
                                    if not hasattr(_stmt, 'unknowns'):
                                        f2003_search_unknowns(_stmt, _stmt.f2003)
                                    if hasattr(_stmt, 'unknowns'):
                                        for unk, req in _stmt.unknowns.iteritems():
                                            if req.state != ResState.RESOLVED:
                                                _stmt.resolve(req) 

                                # if newly found program unit is not in srcfiles
                                if not unit in State.srcfiles[self.top.reader.id][2]:
                                    State.srcfiles[self.top.reader.id][2].append(unit)
                    if request.state==ResState.RESOLVED:
                        break

                # tries to apply implicit rules
                if request.state != ResState.RESOLVED:
                    Logger.info('Parent could not resolve %s and the request is being resolved using implicit rules'%request.uname.firstpartname(), \
                        name=request.uname, stmt=self)
                    # TODO: check against implicit_rules of parent
                    # TODO: mark implicit resolution in ResState

                # if still not resolved, mark it
                if request.state != ResState.RESOLVED:
                    Logger.info('%s is not resolved' % request.uname.firstpartname(), name=request.uname, stmt=self)
                    # mark it
        else:
            Logger.info('%s is resolved' % request.uname.firstpartname(), name=request.uname)
 
    # end of KGEN

class BeginStatement(Statement):
    """[ construct_name : ] <blocktype> [ <name> ]

    BeginStatement instances have additional attributes:
      name
      blocktype

    Block instance has attributes:
      content - list of Line or Statement instances
      name    - name of the block, unnamed blocks are named
                with the line label
      construct_name - name of a construct
      parent  - Block or FortranParser instance
      item    - Line instance containing the block start statement
      get_item, put_item - methods to retrive/submit Line instances
                from/to Fortran reader.
      isvalid - boolean, when False, the Block instance will be ignored.

      stmt_cls, end_stmt_cls

    """
    _repr_attr_names = ['blocktype','name','construct_name'] + Statement._repr_attr_names
    def __init__(self, parent, item=None):

        self.content = []
        self.get_item = parent.get_item # get line function
        self.put_item = parent.put_item # put line function
        if not hasattr(self, 'blocktype'):
            self.blocktype = self.__class__.__name__.lower()
        if not hasattr(self, 'name'):
            # process_item may change this
            self.name = '__'+self.blocktype.upper()+'__'
        self.construct_name = getattr(item,'name',None)
        Statement.__init__(self, parent, item)
        return

    # start of KGEN
    def resolve(self, request):
        from kgen_state import ResState
        from kgen_search import f2003_search_unknowns
        from kgen_utils import pack_exnamepath
        from block_statements import HasUseStmt, Type, TypeDecl, Function, Subroutine, Interface
        from typedecl_statements import TypeDeclarationStatement
        from statements import External, Use, SpecificBinding
        from api import walk

        if request is None: return

        Logger.info('%s is being resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)

        # if resolved, return
        if request.state == ResState.RESOLVED:
            Logger.info('%s is already resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)
            return

        # check if stmt is either subprogram, module, interface
        if isinstance(self, HasUseStmt):
            # NOTE: check if the resolver is in the file or in other file
            # check if internal a subprogram can resolve
            if request.state != ResState.RESOLVED:
                subp = None
                if hasattr(self.a, 'internal_subprogram') and request.uname.firstpartname() in self.a.internal_subprogram.keys():
                    subp = self.a.internal_subprogram[request.uname.firstpartname()]
                elif hasattr(self.a, 'module_subprogram') and request.uname.firstpartname() in self.a.module_subprogram.keys():
                    subp = self.a.module_subprogram[request.uname.firstpartname()]
                elif hasattr(self.a, 'module_interface') and request.uname.firstpartname() in self.a.module_interface.keys():
                    subp = self.a.module_interface[request.uname.firstpartname()]

                if subp and any( isinstance(subp, resolver) for resolver in request.resolvers ):
                    # skip recursive call - NEED TO CHECK
                    Logger.info('The request is being resolved by a subprogram or interface', name=request.uname, stmt=self)
                    if subp not in request.originator.ancestors():
                        request.res_stmts.append(subp)
                        request.state = ResState.RESOLVED
                        subp.add_geninfo(request.uname, request)
                        self.check_spec_stmts(request.uname, request)
                        Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=subp)
                        for _stmt, _depth in walk(subp, -1):
                            if not hasattr(_stmt, 'unknowns'):
                                f2003_search_unknowns(_stmt, _stmt.f2003)
                            if hasattr(_stmt, 'unknowns'):
                                for unk, req in _stmt.unknowns.iteritems():
                                    if req.state != ResState.RESOLVED:
                                        _stmt.resolve(req) 

            # check if subprogram stmt in Interface block can resolve
            if request.state != ResState.RESOLVED and isinstance(request.originator, SpecificBinding) and \
                hasattr(self.a, 'module_interface') and Interface in request.resolvers:
                for if_name, if_obj in self.a.module_interface.iteritems():
                    if if_obj.isabstract:
                        for item in if_obj.content:
                            if item.__class__ in [ Function, Subroutine ] and request.uname.firstpartname()==item.name:
                                Logger.info('The request is being resolved by a Subprogram in abstract interface', \
                                    name=request.uname, stmt=self)
                                request.res_stmts.append(item)
                                request.state = ResState.RESOLVED
                                item.add_geninfo(request.uname, request)
                                self.check_spec_stmts(request.uname, request)
                                Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=item)

                                for _stmt, _depth in walk(item, -1):
                                    if not hasattr(_stmt, 'unknowns'):
                                        f2003_search_unknowns(_stmt, _stmt.f2003)
                                    if hasattr(_stmt, 'unknowns'):
                                        for unk, req in _stmt.unknowns.iteritems():
                                            if req.state != ResState.RESOLVED:
                                                _stmt.resolve(req) 

            # check if a type can resolve
            if request.state != ResState.RESOLVED and hasattr(self.a, 'type_decls') and \
                request.uname.firstpartname() in self.a.type_decls.keys():
                type_stmt  = self.a.type_decls[request.uname.firstpartname()]
                if any( isinstance(type_stmt, resolver) for resolver in request.resolvers ):
                    Logger.info('The request is being resolved by a typedecl', name=request.uname, stmt=self)
                    request.res_stmts.append(type_stmt)
                    request.state = ResState.RESOLVED
                    type_stmt.add_geninfo(request.uname, request)
                    self.check_spec_stmts(request.uname, request)
                    Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=type_stmt)

                    for _stmt, _depth in walk(type_stmt, -1):
                        if not hasattr(_stmt, 'unknowns'):
                            f2003_search_unknowns(_stmt, _stmt.f2003)
                        if hasattr(_stmt, 'unknowns'):
                            for unk, req in _stmt.unknowns.iteritems():
                                if req.state != ResState.RESOLVED:
                                    _stmt.resolve(req) 

            # check if a module variable can resolve
            # NOTE: check if the resolver is in the file or in other file
            if request.state != ResState.RESOLVED and hasattr(self.a, 'variables') and \
                request.uname.firstpartname() in self.a.variables.keys():
                #self.a.variables[request.uname.firstpartname()].is_public():

                typedecl_stmt  = self.a.variables[request.uname.firstpartname()].parent

                #if ((isinstance(res_stmt, TypeDeclarationStatement) and request.uname.firstpartname() in res_stmt.entity_decls) or \
                #    res_stmt is not request.originator) and any( isinstance(res_stmt, resolver) for resolver in request.resolvers ):
                if any( isinstance(typedecl_stmt, resolver) for resolver in request.resolvers ):
                    Logger.info('The request is being resolved by a variable', name=request.uname, stmt=self)
                    request.res_stmts.append(typedecl_stmt)
                    request.state = ResState.RESOLVED
                    typedecl_stmt.add_geninfo(request.uname, request)
                    self.check_spec_stmts(request.uname, request)
                    Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=typedecl_stmt)

                    if not hasattr(typedecl_stmt, 'unknowns'):
                        f2003_search_unknowns(typedecl_stmt, typedecl_stmt.f2003)
                    if hasattr(typedecl_stmt, 'unknowns'):
                        for unk, req in typedecl_stmt.unknowns.iteritems():
                            if req.state != ResState.RESOLVED:
                                typedecl_stmt.resolve(req) 

            # check if a common statement can resolve
            #if request.state != ResState.RESOLVED and hasattr(self.a, 'common_stmts') and \
            #    len(self.a.common_stmts)>0:
            if request.state != ResState.RESOLVED and hasattr(self.a, 'common_stmts') and \
                request.uname.firstpartname() in self.a.common_stmts.keys():
                common_stmt  = self.a.common_stmts[request.uname.firstpartname()]
                if any( isinstance(common_stmt, resolver) for resolver in request.resolvers ):
                    Logger.info('The request is being resolved by a common stmt', name=request.uname, stmt=self)
                    request.res_stmts.append(common_stmt)
                    request.state = ResState.RESOLVED
                    common_stmt.add_geninfo(request.uname, request)
                    self.check_spec_stmts(request.uname, request)
                    Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=common_stmt)

                    if not hasattr(common_stmt, 'unknowns'):
                        f2003_search_unknowns(common_stmt, common_stmt.f2003)
                    if hasattr(common_stmt, 'unknowns'):
                        for unk, req in common_stmt.unknowns.iteritems():
                            if req.state != ResState.RESOLVED:
                                common_stmt.resolve(req) 

            # handle external
            if request.state != ResState.RESOLVED and isinstance(request.originator, External) and \
                self.__class__ in [ Function, Subroutine ] and request.uname.firstpartname() in self.args:
                external_stmt = self
                request.res_stmts.append(external_stmt)
                request.state = ResState.RESOLVED
                external_stmt.add_geninfo(request.uname, request)
                self.check_spec_stmts(request.uname, request)
                Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=external_stmt)

            # check if use stmt can resolve
            if request.state != ResState.RESOLVED:
                Logger.info('%s is not resolved on this block'%request.uname.firstpartname(), name=request.uname, stmt=self)
                if hasattr(self, 'use_stmts') and len(self.use_stmts)>0:
                    Logger.info('The request is being deferred to use stmt', name=request.uname, stmt=self)
                    uname = request.uname.firstpartname()

                    # first, try with use stmts having only keyword
                    for mod_name, use_stmts in self.use_stmts.iteritems():
                        for use_stmt in use_stmts:
                            if use_stmt.isonly:
                                if uname in use_stmt.norenames:
                                    Logger.info('%s is found in norenames'%uname, name=request.uname, stmt=self)
                                    use_stmt.resolve(request)
                                    if request.state == ResState.RESOLVED:
                                        Logger.info('%s is resolved in norenames'%uname, name=request.uname, stmt=self)
                                        request.res_stmts.append(use_stmt)
                                        use_stmt.add_geninfo(request.uname, request)
                                        self.check_spec_stmts(request.uname, request)
                                        break

                                rename = [r for r in use_stmt.renames if r[0]==uname]
                                if len(rename)>1:
                                    raise ProgramException('More than one result: %s'%str(rename))
                                elif len(rename)==1:
                                    newname = KGName(pack_exnamepath(request.originator, rename[0][1]), node=use_stmt.f2003, stmt=use_stmt)
                                    request.push_uname(newname)
                                    use_stmt.resolve(request)
                                    if request.state == ResState.RESOLVED:
                                        request.pop_uname(reset_uname=True)
                                        request.res_stmts.append(use_stmt)
                                        use_stmt.add_geninfo(request.uname, request)
                                        self.check_spec_stmts(request.uname, request)
                                        break
                                    else:
                                        request.pop_uname()
                        if request.state==ResState.RESOLVED: break

                    # and then, try with use stmts not having only keyword
                    # but still has renames or norenames
                    if request.state != ResState.RESOLVED:
                        for mod_name, use_stmts in self.use_stmts.iteritems():
                            for use_stmt in use_stmts:
                                if not use_stmt.isonly:
                                    if uname in use_stmt.norenames:
                                        use_stmt.resolve(request)
                                        if request.state == ResState.RESOLVED:
                                            request.res_stmts.append(use_stmt)
                                            use_stmt.add_geninfo(request.uname, request)
                                            self.check_spec_stmts(request.uname, request)
                                            break

                                    rename = [r for r in use_stmt.renames if r[0]==uname]
                                    if len(rename)>1:
                                        raise ProgramException('More than one result: %s'%str(rename))
                                    elif len(rename)==1:
                                        newname = KGName(pack_exnamepath(request.originator, rename[0][1]), node=use_stmt.f2003, stmt=use_stmt)
                                        request.push_uname(newname)
                                        use_stmt.resolve(request)

                                        if request.state == ResState.RESOLVED:
                                            request.pop_uname(reset_uname=True)
                                            request.res_stmts.append(use_stmt)
                                            use_stmt.add_geninfo(request.uname, request)
                                            self.check_spec_stmts(request.uname, request)
                                            break
                                        else:
                                            request.pop_uname()
                            if request.state==ResState.RESOLVED: break

#                    # and then, try with use stmts that has an analyzed-module
#                    if request.state != ResState.RESOLVED:
#                        for mod_name, use_stmts in self.use_stmts.iteritems():
#                            for use_stmt in use_stmts:
#                                if use_stmt.module:
#                                    if uname in use_stmt.module.a.module_provides:
#                                        use_stmt.resolve(request)
#                                        if request.state == ResState.RESOLVED:
#                                            request.res_stmt = use_stmt
#                                            request.res_stmt.add_geninfo(request.uname, request)
#                                            self.check_spec_stmts(request.uname, request)
#                                            break
#
#                                    rename = [r for r in use_stmt.renames if r[0]==uname]
#                                    if len(rename)>1:
#                                        raise ProgramException('More than one result: %s'%str(rename))
#                                    elif len(rename)==1 and uname in use_stmt.module.a.module_provides:
#                                        request.uname.set_name(rename[0][1])
#                                        use_stmt.resolve(request)
#                                        request.uname.reset_name()
#                                        if request.state == ResState.RESOLVED:
#                                            request.res_stmt = use_stmt
#                                            request.res_stmt.add_geninfo(request.uname, request)
#                                            self.check_spec_stmts(request.uname, request)
#                                            break
#                            if request.state==ResState.RESOLVED: break

                    # and then, try with use stmts not having only keyword
                    # and not has renames or norenames
                    if request.state != ResState.RESOLVED:
                        for mod_name, use_stmts in self.use_stmts.iteritems():
                            for use_stmt in use_stmts:
                                if not use_stmt.isonly:
                                    if len(use_stmt.norenames)==0 and len(use_stmt.renames)==0:
                                        use_stmt.resolve(request)
                                        if request.state == ResState.RESOLVED:
                                            request.res_stmts.append(use_stmt)
                                            use_stmt.add_geninfo(request.uname, request)
                                            self.check_spec_stmts(request.uname, request)
                                            break
                            if request.state==ResState.RESOLVED: break

        elif self.__class__ in [ Type, TypeDecl ]:
            # check if a type can resolve
            if request.state != ResState.RESOLVED and request.uname.firstpartname()==self.name:
                type_stmt  = self
                if any( isinstance(type_stmt, resolver) for resolver in request.resolvers ):
                    Logger.info('The request is being resolved by a typedecl', name=request.uname, stmt=self)
                    request.res_stmts.append(type_stmt)
                    request.state = ResState.RESOLVED
                    type_stmt.add_geninfo(request.uname, request)
                    self.check_spec_stmts(request.uname, request)
                    Logger.info('%s is resolved'%request.uname.firstpartname(), name=request.uname, stmt=type_stmt)

                    #if self not in request.originator.ancestors():
                    if self is request.originator.parent:
                        pass
                    else:
                        for _stmt, _depth in walk(type_stmt, -1):
                            if not hasattr(_stmt, 'unknowns'):
                                f2003_search_unknowns(_stmt, _stmt.f2003)
                            if hasattr(_stmt, 'unknowns'):
                                for unk, req in _stmt.unknowns.iteritems():
                                    if req.state != ResState.RESOLVED:
                                        _stmt.resolve(req) 
        # defer to super
        if request.state != ResState.RESOLVED:
            super(BeginStatement, self).resolve(request)

    def tokgen(self, **kwargs):
        construct_name = self.construct_name
        construct_name = construct_name + ': ' if construct_name else ''
        return self.get_indent_tab(isfix=False).lstrip() + construct_name + self.item.apply_map(self.tostr())

    # end of KGEN

    def tostr(self):
        return self.blocktype.upper() + ' '+ self.name

    def tofortran(self, isfix=None):
        construct_name = self.construct_name
        construct_name = construct_name + ': ' if construct_name else ''
        l=[self.get_indent_tab(isfix=isfix) + construct_name + self.tostr()]
        for c in self.content:
            l.append(c.tofortran(isfix=isfix))
        return '\n'.join(l)

    def torepr(self, depth=-1, incrtab=''):
        tab = incrtab + self.get_indent_tab()
        ttab = tab + '  '
        l=[Statement.torepr(self, depth=depth,incrtab=incrtab)]
        if depth==0 or not self.content:
            return '\n'.join(l)
        l.append(ttab+'content:')
        for c in self.content:
            if isinstance(c,EndStatement):
                l.append(c.torepr(depth-1,incrtab))
            else:
                l.append(c.torepr(depth-1,incrtab + '  '))
        return '\n'.join(l)

    def process_item(self):
        """ Process the line
        """
        item = self.item
        if item is None: return
        self.fill()
        return

    def fill(self, end_flag = False):
        """
        Fills blocks content until the end of block statement.
        """

        mode = self.reader.mode
        class_list = self.get_classes()
        self.classes = [cls for cls in class_list if mode in cls.modes]
        self.pyf_classes = [cls for cls in class_list if 'pyf' in cls.modes]

        item = self.get_item()
        while item is not None:
            if isinstance(item, Line):
                if self.process_subitem(item):
                    end_flag = True
                    break
            elif isinstance(item, Comment):
                # TODO: FIX ME, Comment content is a string
                self.content.append(classes.Comment(self, item))
            else:
                raise NotImplementedError(`item`)
            item = self.get_item()

        if not end_flag:
            self.warning('failed to find the end of block')
        return

    def process_subitem(self, item):
        """
        Check is item is blocks start statement, if it is, read the block.

        Return True to stop adding items to given block.
        """
        line = item.get_line()

        # First check for the end of block
        cls = self.end_stmt_cls
        if cls.match(line):
            stmt = cls(self, item)
            if stmt.isvalid:
                self.content.append(stmt)
                return True

        if item.is_f2py_directive:
            classes = self.pyf_classes
        else:
            classes = self.classes

        # Look for statement match
        for cls in classes:
            if cls.match(line):
                stmt = cls(self, item)
                if stmt.isvalid:
                    if not stmt.ignore:
                        self.content.append(stmt)
                    return False
                # item may be cloned that changes the items line:
                line = item.get_line()

        # Check if f77 code contains inline comments or other f90
        # constructs that got undetected by get_source_info.
        if item.reader.isf77:
            i = line.find('!')
            if i != -1:
                message = item.reader.format_message(\
                        'WARNING',
                        'no parse pattern found for "%s" in %r block,'\
                        ' trying to remove inline comment (not in Fortran 77).'\
                        % (item.get_line(),self.__class__.__name__),
                        item.span[0], item.span[1])
                # .. but at the expense of loosing the comment.
                logger.warning(message)
                # self.show_message(message)
                if line[:i]:
                    newitem = item.copy(line[:i].rstrip())
                    return self.process_subitem(newitem)
                else:
                    return True

            # try fix statement classes
            f77_classes = self.classes
            classes = []
            for cls in self.get_classes():
                if 'f77' in cls.modes and cls not in f77_classes:
                    classes.append(cls)
            if classes:
                message = item.reader.format_message(\
                        'WARNING',
                        'no parse pattern found for "%s" in %r block'\
                        ' maybe due to strict f77 mode.'\
                        ' Trying f90 fix mode patterns..'\
                        % (item.get_line(),self.__class__.__name__),
                        item.span[0], item.span[1])
                logger.warning(message)
                # self.show_message(message)

                item.reader.set_mode(False, False)
                self.classes = classes

                r = BeginStatement.process_subitem(self, item)
                if r is None:
                    # restore f77 fix mode
                    self.classes = f77_classes
                    item.reader.set_mode(False, True)
                else:
                    message = item.reader.format_message(\
                        'INFORMATION',
                        'The f90 fix mode resolved the parse pattern issue.'\
                        ' Setting reader to f90 fix mode.',
                        item.span[0], item.span[1])
                    logger.info(message)
                    # self.show_message(message)
                    # set f90 fix mode
                    self.classes = f77_classes + classes
                    self.reader.set_mode(False, False)
                return r

        self.handle_unknown_item(item)
        return

    def handle_unknown_item(self, item):
        message = item.reader.format_message(\
                        'WARNING',
                        'no parse pattern found for "%s" in %r block.'\
                        % (item.get_line(),self.__class__.__name__),
                        item.span[0], item.span[1])
        logger.warning(message)
        # self.show_message(message)
        self.content.append(item)
        #sys.exit()
        return

    def analyze(self):
        for stmt in self.content:
            stmt.analyze()
        return

class EndStatement(Statement):
    """
    END [<blocktype> [<name>]]

    EndStatement instances have additional attributes:
      name
      blocktype
    """
    _repr_attr_names = ['blocktype','name'] + Statement._repr_attr_names

    def __init__(self, parent, item):
        if not hasattr(self, 'blocktype'):
            self.blocktype = self.__class__.__name__.lower()[3:]
        Statement.__init__(self, parent, item)

    def process_item(self):
        item = self.item
        line = item.get_line().replace(' ','')[3:]
        line = item.apply_map(line)
        blocktype = self.blocktype
        if line.lower().startswith(blocktype):
            line = line[len(blocktype):].strip()
        else:
            if line:
                # not the end of expected block
                line = ''
                self.isvalid = False
        if self.parent.construct_name:
            name = self.parent.construct_name
        else:
            name = self.parent.name

        if line:
            # line variable is already cast to lower case so would fail if any
            # upper case letters exist in the label. Also, fortran is case
            # insensitive anyway so we should assume labels may have a
            # different case and therefore cast both to the same case in our
            # equivalence test.
            if line.lower()!=name.lower():
                self.warning(\
                    'expected the end of %r block but got the end of %r, skipping.'\
                    % (name, line))
                self.isvalid = False
        # start of KGEN addition
            else:
                self.name = line.lower()
        else:
            self.name = None
        # end of KGEN addition
        #self.name = name # KGEN deletion

    def analyze(self):
        return

    def get_indent_tab(self,deindent=False,isfix=None):
        return Statement.get_indent_tab(self, deindent=True,isfix=isfix)

    def tofortran(self, isfix=None):
        return self.get_indent_tab(isfix=isfix) + 'END %s %s'\
               % (self.blocktype.upper(),self.name or '')

    # start of KGEN addition
    def tokgen(self, **kwargs):
        return 'END %s %s' % (self.blocktype.upper(),self.name or '')
    # end of KGEN addition
