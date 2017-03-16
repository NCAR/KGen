'''KGen utilities
'''

import sys
import logging
import subprocess

##############################################
# KGName
##############################################

EXTERNAL_NAMELEVEL_SEPERATOR = ':'
INTERNAL_NAMELEVEL_SEPERATOR = '__kgen__' # lower-case only

def encode_NS(namepath):
    return namepath.replace(EXTERNAL_NAMELEVEL_SEPERATOR, INTERNAL_NAMELEVEL_SEPERATOR)

def decode_NS(namepath):
    return namepath.replace(INTERNAL_NAMELEVEL_SEPERATOR, EXTERNAL_NAMELEVEL_SEPERATOR)

class KGName(object):
    def __init__(self, name, node=None, stmt=None):
        from parser.Fortran2003 import Data_Ref
        if not name: raise ProgramException('Name can not be none or blank')
        if name[0].isdigit(): raise ProgramException('Name can not have digit as its first character')

        self.namepath = encode_NS(name).strip().lower() # lower case
        self.namelist = self.namepath.split(INTERNAL_NAMELEVEL_SEPERATOR)
        self.dataref = Data_Ref(self.namelist[-1])
        self.node = node
        self.stmt = stmt
        #self.rename = []

    def path(self):
        return decode_NS(self.namepath)

    def list(self):
        return self.namelist

    def dataref(self):
        return self.dataref

    def last(self):
        return self.namelist[-1]

    def first(self):
        return self.namelist[0]

    def firstpartname(self):
        from parser.Fortran2003 import Name
        if isinstance(self.dataref, Name):
            return self.dataref.string
        else:
            return self.dataref.items[0].string

    def __eq__(self, other):
        return self.namepath==other.namepath

    def __str__(self):
        raise Exception('KGName')

def _get_namepath(stmt, external):
    if external:
        return EXTERNAL_NAMELEVEL_SEPERATOR.join([ a.name.lower() for a in stmt.ancestors() ])
    else:
        return INTERNAL_NAMELEVEL_SEPERATOR.join([ a.name.lower() for a in stmt.ancestors() ])

def _pack_namepath(stmt, lastname, external):
    if external:
        return '%s%s%s'%(_get_namepath(stmt, True), EXTERNAL_NAMELEVEL_SEPERATOR, lastname)
    else:
        return '%s%s%s'%(_get_namepath(stmt, False), INTERNAL_NAMELEVEL_SEPERATOR, lastname)

def pack_innamepath(stmt, name):
    return _pack_namepath(stmt, name, False)

def pack_exnamepath(stmt, name):
    return _pack_namepath(stmt, name, True)

def get_innamepath(stmt):
    return _get_namepath(stmt, False)

def get_exnamepath(stmt):
    return _get_namepath(stmt, True)

def match_namepath(pattern, namepath, internal=True):

#name -> name in the beginning and the end
#:name: -> name in any location
#name: -> name at the beginning
#:name -> name at the end
#name1:name2 -> two level name
#name1:name2: -> more than two level name starts with the two names
#:name1:name2 -> more than two level name ends with the two names
#:name1:name2: -> more than two level name that the two name locates in the middle
#eventually data slicing

    if not pattern or not namepath: return False

    if internal:
        split_pattern = pattern.split(INTERNAL_NAMELEVEL_SEPERATOR)
        split_namepath = namepath.split(INTERNAL_NAMELEVEL_SEPERATOR)
    else:
        split_pattern = pattern.split(EXTERNAL_NAMELEVEL_SEPERATOR)
        split_namepath = namepath.split(EXTERNAL_NAMELEVEL_SEPERATOR)

    p = list(split_pattern)

    leading_mark = False
    if len(p[0])==0:
        leading_mark = True
        p = p[1:]

    ending_mark = False
    if len(p[-1])==0:
        ending_mark = True
        p = p[:-1]
        if len(p)==0:
            raise UserException('Wrong namepath format: %s'%split_pattern)

    n = list(split_namepath)
    while len(p)>0 and len(n)>0:
        if p[0]==n[0]:
            p = p[1:]
            n = n[1:]
        elif leading_mark:
            n = n[1:]
        elif len(p[0])==0:
            leading_mark = True
            p = p[1:]
        else:
            return False

    if len(p)==0:
        if len(n)>0:
            if ending_mark: return True
            return False
        else:
            return True
    else:
        if len(n)>0:
            raise ProgramException('Incorrect namepath match: (%s, %s)'%(split_pattern, split_namepath))
        else:
            return False

#############################################################################
## EXCEPTION
#############################################################################

class KGException(Exception):
    pass

class UserException(KGException):
    pass

class ProgramException(KGException):
    pass

##############################################
# Functions
##############################################

def exit(msg):
    print msg
    sys.exit(-1)

def run_shcmd(cmd, input=None, **kwargs):

    show_error_msg = None
    if kwargs.has_key('show_error_msg'):
        show_error_msg = kwargs['show_error_msg']
        del kwargs['show_error_msg']

    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
    out, err = proc.communicate(input=input)

    if proc.returncode != 0 and show_error_msg:
        print '>> %s' % cmd
        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(out)+'\n ERROR: '+str(err)+'\n'

    return out, err, proc.returncode

# traverse f2003 nodes
# traverse and func will return None if to continue processing
# traverse and func will return return code if to stop processing
# The return code will be forwarded to initial caller
# func will collect anything in bag during processing
def traverse(node, func, bag, subnode='items', prerun=True, depth=0):
    ret = None

    if prerun and func is not None:
        ret = func(node, bag, depth)
        if ret is not None: return ret

    if node and hasattr(node, subnode) and getattr(node, subnode) is not None:
        for child in getattr(node, subnode):
            ret = traverse(child, func, bag, subnode=subnode, prerun=prerun, depth=depth+1)

    if not prerun and func is not None:
        ret = func(node, bag, depth)
        if ret is not None: return ret

    return ret

def get_subtree(obj, tree, prefix='top', depth=0):
    tab = '    '
    postfix = ''
    if isinstance(obj, str): postfix = ' => ' + obj
    elif isinstance(obj, type): postfix = ' => ' + str(obj)
    elif obj.__class__.__name__=='Name': postfix = ' => ' + obj.string

    #tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix, depth ) ]
    if hasattr(obj, 'parent'):
        pcls = str(obj.parent.__class__)
    else:
        pcls = 'None'
    tree += [ ( tab*depth + prefix + ': ' + str(obj.__class__) + postfix + ': parent => ' + pcls , depth ) ]
    if hasattr(obj, 'items'):
        for item in obj.items:
            get_subtree(item, tree, prefix='item', depth=depth+1)

    if hasattr(obj, 'content'):
        for elem in obj.content:
            get_subtree(elem, tree, prefix='content', depth=depth+1)

def show_obj(obj):
    print 'CLS: ', obj.__class__
    print 'STR: ', str(obj)
    print 'DIR: ', dir(obj)

def show_tree(node, prevent_print=False):
    tree = []
    get_subtree(node, tree)
    lines = []
    for elem, depth in tree:
        line = '    '*depth + elem
        if not prevent_print:
            print line
        lines.append(line+'\n')
    return lines

def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]

def remove_multiblanklines(text):
    MAXBLANKLINES = 3
    lines = text.split('\n')
    newlines = []
    count = 0
    for line in lines:
        if len(line)>0:
            newlines.append(line)
            count = 0
        else:
            count += 1
            if count < MAXBLANKLINES:
                newlines.append(line)

    return '\n'.join(newlines)

def dequote(s):
    s = s.strip()
    if len(s) == 0: return s
    if (s[0] == s[-1]) and s.startswith(("'", '"')):
        return s[1:-1]
    return s

##############################################
# Logging
##############################################

# create logger with 'spam_application'
logger = logging.getLogger('kgen')
logger.setLevel(logging.DEBUG)

# create file handler which logs even debug messages
fh = logging.FileHandler('kgen.log', mode='w')
fh.setLevel(logging.DEBUG)

# create console handler with a higher log level
ch = logging.StreamHandler()
ch.setLevel(logging.INFO)

# create formatter and add it to the handlers
cf = logging.Formatter('%(message)s')
ff = logging.Formatter('%(levelname)-8s [%(filename)s:%(lineno)s] %(message)s')
fh.setFormatter(ff)
ch.setFormatter(cf)

# add the handlers to the logger
logger.addHandler(fh)
logger.addHandler(ch)

