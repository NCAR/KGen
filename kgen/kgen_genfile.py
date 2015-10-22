# kgen_genfile.py
#
# NOTES:
# 1. Gen_<class name> classes construct backbone of source file generation
# 2. Gen[K|S]_<class_name> classes add kernel or state specific codes
# 3. Gen[K|S]_<class_name> are default classes if there is no particular class
# 4. __init__ construct tree structure and prepare processing by setting attributes
# 5. process analyze stmt and generates specific attributes to be forwarded to tokgen and creates dummy classes if 
#    new stmt is required
# 6. tostr eventually call tokgen with the specific attributes generated in process

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType
from kgen_state import State

########### Common ############
def _genobj(node, gentype):
    import inspect

    obj = None
    for cls in inspect.getmro(node.__class__):    
        try:
            exec('obj = Gen%s_%s(node)'%(gentype, cls.__name__))  
            break
        except:
            pass
    return obj

def genkobj(node):
    return _genobj(node, 'K')

def gensobj(node):
    return _genobj(node, 'S')

########### Statement ############
class Gen_Statement(object):
    def __init__(self, node):
        self.stmt = node
        self.parent = None
        self.isvalid = True

    def tostr(self):
        from statements import Comment

        if isinstance(self.stmt, Comment):
            if not self.stmt.item.comment.startswith('!KGEN#'):
                start = self.stmt.item.span[0]-1
                end = self.stmt.item.span[1]
                lines = self.stmt.top.prep[start:end]
                lines_str = '\n'.join(lines)
                if lines_str.strip().startswith(self.stmt.item.comment.strip()):
                    return lines_str
        elif self.isvalid:
            if self.stmt.item and hasattr(self.stmt.item, 'span'):
                start = self.stmt.item.span[0]-1
                end = self.stmt.item.span[1]
                lines = self.stmt.top.prep[start:end]
                return '\n'.join(lines)

class GenK_Statement(Gen_Statement):
    gentype = 'K'

    def genobj(self, node):
        return genkobj(node)

    def process(self):
        if hasattr(self.stmt, 'geninfo') and self.stmt.geninfo.has_key(KGGenType.KERNEL):
            pass
        elif hasattr(self.stmt, 'unknowns') and any(res.gentype==KGGenType.KERNEL for res in self.stmt.unknowns.values()):
            pass
        else:
            self.isvalid = False


class GenS_Statement(Gen_Statement):
    gentype = 'S'

    def genobj(self, node):
        return gensobj(node)

    def process(self):
        pass

########### BeginStatement ############
class Gen_BeginStatement(Gen_Statement):
    def __init__(self, node):
        self.items = []

        super(Gen_BeginStatement, self).__init__(node)

        for item in node.content:
            childnode = self.genobj(item)
            childnode.parent = self
            self.items.append(childnode)

    def tostr_children(self):
        lines = []
        for item in self.items:
            l = item.tostr()
            if l is not None: lines.append(l)
        return lines

class GenK_BeginStatement(Gen_BeginStatement, GenK_Statement):
    def process(self):
        # process first line
        super(GenK_BeginStatement, self).process()

        if self.isvalid:
            # process remained lines
            for item in self.items:
                item.process()
            
    def tostr(self):
        if self.isvalid:
            lines = []
            l = super(GenK_BeginStatement, self).tostr()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_children())
            return '\n'.join(lines)
        else:
            return

class GenS_BeginStatement(Gen_BeginStatement, GenS_Statement):
    def process(self):
        # process first line
        super(GenS_BeginStatement, self).process()

        # process remained lines
        for item in self.items:
            item.process()
            
    def tostr(self):
        lines = []
        l = super(GenS_BeginStatement, self).tostr()
        if l is not None: lines.append(l)
        lines.extend(self.tostr_children())
        return '\n'.join(lines)

########### BeginSource ############
class Gen_BeginSource(Gen_BeginStatement):
    def __init__(self, node):
        super(Gen_BeginSource, self).__init__(node)

class GenK_BeginSource(Gen_BeginSource, GenK_BeginStatement):
    def gensrc(self, fd):
        if hasattr(self.stmt, 'geninfo') and self.stmt.geninfo.has_key(KGGenType.KERNEL):
            self.process()
            lines = self.tostr()
            if lines is not None: fd.write(lines)

class GenS_BeginSource(Gen_BeginSource, GenS_BeginStatement):
    def gensrc(self, fd):
        self.process()
        # create file only if it is used for state generation
        lines = self.tostr()
        if lines is not None: fd.write(lines)

def generate_srcfiles():
    """Generate source files."""
    from block_statements import Program, Module

    # create state and kernel directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

    if isinstance(State.topblock['stmt'], Program):
        Logger.major("Callsite statement can not be in Program unit.", stdout=True)
        sys.exit(-1)
    elif isinstance(State.topblock['stmt'], Module):
        pass
        #generate_callsite_module()
    else:
        raise ProgramException('Unknown parent type: %s' % State.topblock['stmt'].__class__)

    # generate instrumented files except callsite file
    #generate_state_files()

    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
        filename = os.path.basename(filepath)
        if hasattr(srcobj.tree, 'geninfo') and srcobj.tree.geninfo.has_key(KGGenType.KERNEL):
            kfile = genkobj(srcobj.tree)
            if kfile is None:
                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
            else:
                with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as f:
                    kfile.gensrc(f)

        if hasattr(srcobj.tree, 'geninfo') and srcobj.tree.geninfo.has_key(KGGenType.STATE):
            sfile = gensobj(srcobj.tree)
            if sfile is None:
                raise ProgramException('Instrumented source file is not generated for %s.'%filepath)
            else:
                with open('%s/%s'%(Config.path['state'], filename), 'wb') as f:
                    sfile.gensrc(f)

    State.state = State.STATE_GENERATED

