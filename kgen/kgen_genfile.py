# kgen_genfile.py
#

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType
from kgen_state import State
from statements import Comment

# KERNEL ID ...

########### Common ############

TAB = '    ' 

def get_indent(line):
    import re
    return re.match(r"\s*", line).group()

def _genobj(node, k_id, gentype):
    import inspect

    obj = None
    for cls in inspect.getmro(node.__class__):    
        try:
            exec('obj = Gen%s_%s(node, k_id)'%(gentype, cls.__name__))  
            break
        except NameError:
            pass
        except:
            raise
    return obj

def genkobj(node, k_id):
    return _genobj(node, k_id, 'K')

def gensobj(node, k_id):
    return _genobj(node, k_id, 'S')

class Gen_HasUseStmts(object):
    from statements import Use
    classes = [ Use ]
    def __init__(self):
        super(Gen_HasUseStmts, self).__init__()
        self.use_stmts = []

    def insert_in_use_stmts(self, item):
        self.use_stmts.append(item)

class Gen_HasImportStmts(object):
    from statements import Import
    classes = [ Import ]
    def __init__(self):
        super(Gen_HasImportStmts, self).__init__()
        self.import_stmts = []

    def insert_in_import_stmts(self, item):
        self.import_stmts.append(item)

class Gen_HasImplicitPart(object):
    from block_statements import implicit_part
    classes = implicit_part
    def __init__(self):
        super(Gen_HasImplicitPart, self).__init__()
        self.implicit_part = []

    def insert_in_implicit_part(self, item):
        self.implicit_part.append(item)

class Gen_HasDeclConstruct(object):
    from block_statements import declaration_construct
    classes = declaration_construct
    def __init__(self):
        super(Gen_HasDeclConstruct, self).__init__()
        self.decl_construct = []

    def insert_in_decl_construct(self, item):
        self.decl_construct.append(item)

class Gen_HasExecutionPart(object):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        super(Gen_HasExecutionPart, self).__init__()
        self.exe_part = []

    def insert_in_exe_part(self, item):
        self.exe_part.append(item)

class Gen_HasContainsStmt(object):
    from statements import Contains
    classes = [ Contains ]
    def __init__(self):
        super(Gen_HasContainsStmt, self).__init__()
        self.contains_stmt = []

    def insert_in_contains_stmt(self, item):
        self.contains_stmt.append(item)

class Gen_HasSubprograms(object):
    from block_statements import internal_subprogram
    classes =  internal_subprogram
    def __init__(self):
        super(Gen_HasSubprograms, self).__init__()
        self.subprograms = []

    def insert_in_subprograms(self, item):
        self.subprograms.append(item)


########### Statement ############
class Gen_Statement(object):

    def __init__(self, node, k_id):
        super(Gen_Statement, self).__init__()

        self.isvalid = True
        self.k_id = k_id
        self.stmt = node
        self.parent = None
        self.tokgen_attrs = {}
        self.indent = ''

    def tostr(self):
        from statements import Comment
        from block_statements import BeginSource


        if isinstance(self.stmt, Comment):
            if not self.stmt.item.comment.startswith('!KGEN#'):
                start = self.stmt.item.span[0]-1
                end = self.stmt.item.span[1]
                lines = self.stmt.top.prep[start:end]
                lines_str = '\n'.join(lines)
                if lines_str.strip().startswith(self.stmt.item.comment.strip()):
                    self.indent = get_indent(lines_str)
                    return lines_str
        elif self.isvalid:
            if self.stmt:
                if len(self.tokgen_attrs)>0:
                    return self.indent + self.stmt.tokgen(**self.tokgen_attrs)
                else:
                    if hasattr(self.stmt.item, 'span'):
                        start = self.stmt.item.span[0]-1
                        end = self.stmt.item.span[1]
                        lines = self.stmt.top.prep[start:end]
                        lines_str = '\n'.join(lines)
                        self.indent = get_indent(lines_str)
                        return lines_str
                    elif self.stmt.__class__ in [ BeginSource ]:
                        pass
                    else:
                        raise ProgramException('Wrong path for tostr')
            else:
                return self.indent + self.tokgen(**self.tokgen_attrs)

    def tokgen(self, **kwargs):
        raise ProgramException('Inherited class should implement tokgen().')

    def add_attr(self, key, value):
        self.tokgen_attrs[key] = value

    def get_dtype_subpname(self, stmt):
        return '%s_%s'%(stmt.ancestors()[0].name, stmt.name)

    def get_nondtype_subpname(self, stmt):
        import pdb; pdb.set_trace()
        pass

    def get_subpname(self, stmt):
        from block_statements import Type

        if isinstance(stmt, Type):
            return self.get_dtype_subpname(stmt)
        else:
            return self.get_nondtype_subpname(stmt)

    def process(self):
        pass

class GenK_Statement(Gen_Statement):
    gentype = 'K'

    def __init__(self, node, k_id):
        super(GenK_Statement, self).__init__(node, k_id)

        if self.stmt is None:
            pass
        #elif hasattr(self.stmt, 'geninfo') and self.stmt.geninfo.has_key(KGGenType.KERNEL):
        elif hasattr(self.stmt, 'geninfo'):
            pass
        #elif hasattr(self.stmt, 'unknowns') and any(res.gentype==KGGenType.KERNEL for res in self.stmt.unknowns.values()):
        elif hasattr(self.stmt, 'unknowns'):
            pass
        else:
            self.isvalid = False

    def genobj(self, node, k_id):
        return genkobj(node, k_id)

    def get_readname(self, stmt):
        return 'kr_%s'%self.get_subpname(stmt)

    def get_verifyname(self, stmt):
        return 'kv_%s'%self.get_subpname(stmt)

class GenS_Statement(Gen_Statement):
    gentype = 'S'

    def __init__(self, node, k_id):
        super(GenS_Statement, self).__init__(node, k_id)

    def genobj(self, node, k_id):
        return gensobj(node, k_id)

    def get_writename(self, stmt):
        return 'kw_%s'%self.get_subpname(stmt)

########### Use ############
class GenK_Use(GenK_Statement):
    def process(self):
        super(GenK_Use, self).process()
        if self.isvalid:
            if self.stmt and self.stmt.isonly:
                # limit use items for KERNEL
                items = []
                for (uname, req) in self.stmt.geninfo[KGGenType.KERNEL]:
                    items.append(uname.firstpartname())
                self.add_attr('items', items)

                # add read verify perturb for STATE
                newitems = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    newitems.append(self.get_readname(req.res_stmts[0]))
                    newitems.append(self.get_verifyname(req.res_stmts[0]))
                useobj = GenK_Use(self.stmt, self.k_id)
                useobj.parent = self.parent
                useobj.add_attr('items', newitems)
                self.parent.insert_in_use_stmts(useobj)

                pubobj = GenK_Public(None, self.k_id)
                pubobj.parent = self.parent
                pubobj.add_attr('items', newitems)
                self.parent.insert_in_decl_construct(pubobj)

class GenS_Use(GenS_Statement):
    def process(self):
        super(GenS_Use, self).process()
        if self.stmt and self.stmt.isonly:

            # add read verify perturb for STATE
            newitems = []
            for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                newitems.append(self.get_writename(req.res_stmts[0]))
            useobj = GenS_Use(self.stmt, self.k_id)
            useobj.parent = self.parent
            useobj.add_attr('items', newitems)
            self.parent.insert_in_use_stmts(useobj)

            pubobj = GenS_Public(None, self.k_id)
            pubobj.parent = self.parent
            pubobj.add_attr('items', newitems)
            self.parent.insert_in_decl_construct(pubobj)

########### Public ############
class GenK_Public(GenK_Statement):

    def process(self):
        super(GenK_Public, self).process()

    def tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)

class GenS_Public(GenS_Statement):

    def process(self):
        super(GenS_Public, self).process()
 
    def tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)

########### TypeDeclarationStatement ############

class GenK_TypeDeclarationStatement(GenK_Statement):
    def process(self):
        super(GenK_TypeDeclarationStatement, self).process()
        if self.isvalid:
            if self.stmt and hasattr(self.stmt, 'geninfo'):

                # limit typedecl items for IN items
                items = []
                out_items = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    items.append(uname.firstpartname())
                    if KGGenType.is_state_out_inout(req.gentype):
                        out_items.append('ref_'+uname.firstpartname())
                self.add_attr('items', items)
                if self.stmt.parent is State.parentblock['stmt']:
                    self.add_attr('remove_intent', None)

                # add typedecl items for OUT items
                if out_items:
                    typedecleobj = GenK_TypeDeclarationStatement(self.stmt, self.k_id)
                    typedecleobj.add_attr('items', out_items)
                    if self.stmt.parent is State.parentblock['stmt']:
                        typedecleobj.add_attr('remove_intent', None)
                    self.parent.insert_in_decl_construct(typedecleobj)

class GenS_TypeDeclarationStatement(GenS_Statement):
    def process(self):
        super(GenS_TypeDeclarationStatement, self).process()
        if self.stmt:
            pass

########### BeginStatement ############
class Gen_BeginStatement(Gen_Statement):
    def __init__(self, node, k_id):
        self.items = []
        self.insert_blist = []
        self.insert_alist = []
        self.end_stmt = None

        super(Gen_BeginStatement, self).__init__(node, k_id)

        for item in node.content:
            childnode = self.genobj(item, k_id)
            childnode.parent = self
            self.items.append(childnode)

    def tostr_list(self, items, indent):
        lines = []
        for item in items:
            item.indent = indent
            l = item.tostr()
            if l is not None:
                lines.append(l)
                indent = item.indent
        return lines

    def insert_before(self, posobj, item):
        if posobj in self.items:
            self.insert_blist.append((posobj, item))
        else:
            raise ProgramException('Following statement is not in items list:\n%s'%str(posobj))

    def insert_after(self, posobj, item):
        if posobj in self.items:
            self.insert_alist.append((posobj, item))
        else:
            raise ProgramException('Following statement is not in items list:\n%s'%str(posobj))

    def process_items(self):
        for item in self.items:
            item.process()

    def process_insertlists(self):
        for (posobj, item) in self.insert_blist:
            self.items.insert(self.items.index(posobj), item)
        self.insert_blist = []
        for (posobj, item) in self.insert_alist:
            self.items.insert(self.items.index(posobj)+1, item)
        self.insert_alist = []

class GenK_BeginStatement(Gen_BeginStatement, GenK_Statement):
    def process(self):
        # process first line
        super(GenK_BeginStatement, self).process()

        if self.isvalid:
            # process remained lines
            self.process_items()

            # process insert lists
            self.process_insertlists()

    def tostr(self):
        if self.isvalid:
            lines = []
            l = super(GenK_BeginStatement, self).tostr()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_list(self.items, self.indent+TAB))
            return '\n'.join(lines)
        else:
            return

class GenS_BeginStatement(Gen_BeginStatement, GenS_Statement):
    def process(self):
        # process first line
        super(GenS_BeginStatement, self).process()

        # process remained lines
        self.process_items()
            
        # process insert lists
        self.process_insertlists()

    def tostr(self):
        lines = []
        l = super(GenS_BeginStatement, self).tostr()
        if l is not None: lines.append(l)

        lines.extend(self.tostr_list(self.items, self.indent+TAB))
        return '\n'.join(lines)

class Gen_EndStatement(Gen_Statement):
    def tostr(self):
        self.indent = self.parent.indent
        return super(Gen_EndStatement, self).tostr()
 
class GenK_EndStatement(Gen_EndStatement):
    pass

class GenS_EndStatement(Gen_EndStatement):
    pass

########### Module ############
class Gen_Module(object):
    def process_module_items(self):
        from block_statements import EndModule

        cur_list = self.use_stmts
        for item in self.items:
            if item.stmt.__class__ in Gen_HasUseStmts.classes:
                self.insert_in_use_stmts(item)
                if cur_list is not self.use_stmts:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
            elif item.stmt.__class__ in Gen_HasImportStmts.classes:
                self.insert_in_import_stmts(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.import_stmts
            elif item.stmt.__class__ in Gen_HasImplicitPart.classes:
                self.insert_in_implicit_part(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and cur_list is not self.implicit_part:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.implicit_part
            elif item.stmt.__class__ in Gen_HasDeclConstruct.classes:
                self.insert_in_decl_construct(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and \
                    cur_list is not self.implicit_part and cur_list is not self.decl_construct:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.decl_construct
            elif item.stmt.__class__ in Gen_HasContainsStmt.classes:
                self.insert_in_contains_stmt(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and \
                    cur_list is not self.implicit_part and cur_list is not self.decl_construct:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.contains_stmt
            elif item.stmt.__class__ in Gen_HasSubprograms.classes:
                self.insert_in_subprograms(item)
                if cur_list is not self.contains_stmt and cur_list is not self.subprograms:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.stmt.__class__)
                else:
                    cur_list = self.subprograms
            elif item.stmt.__class__ is EndModule:
                self.end_stmt = item
                cur_list = None
            elif item.stmt.__class__ is Comment:
                cur_list.append(item)
            else:
                raise ProgramException('Unknown stmt type: %s'%item.stmt.__class__)

            item.process()
    def tostr_module(self, indent):
        lines = []
        lines.extend(self.tostr_list(self.use_stmts, indent))
        lines.extend(self.tostr_list(self.import_stmts, indent))
        lines.extend(self.tostr_list(self.implicit_part, indent))
        lines.extend(self.tostr_list(self.decl_construct, indent))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt, indent))
        elif len(self.subprograms)>0:
            lines.append('CONTAINS')
        lines.extend(self.tostr_list(self.subprograms, indent))
        if self.end_stmt:
            lines.append(self.end_stmt.tostr())
        return lines

class GenK_Module(GenK_BeginStatement, Gen_Module, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasContainsStmt, Gen_HasSubprograms):
    def __init__(self, node, k_id):
        super(GenK_Module, self).__init__(node, k_id)

    def process(self):

        # process first line
        super(GenK_BeginStatement, self).process()

        if self.isvalid:
            self.process_module_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = super(GenK_BeginStatement, self).tostr()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_module(self.indent+TAB))
            return '\n'.join(lines)
        else:
            return

class GenS_Module(GenS_BeginStatement, Gen_Module, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasContainsStmt, Gen_HasSubprograms):
    def __init__(self, node, k_id):
        super(GenS_Module, self).__init__(node, k_id)

    def process(self):

        # process first line
        super(GenS_BeginStatement, self).process()

        if self.isvalid:
            self.process_module_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = super(GenS_BeginStatement, self).tostr()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_module(self.indent+TAB))
        
            return '\n'.join(lines)
        else:
            return


########### SubProgramStatement ############
class Gen_SubProgramStatement(object):
    def process_subp_items(self):
        from block_statements import EndSubroutine, EndFunction

        cur_list = self.use_stmts
        for item in self.items:
            if item.stmt.__class__ in Gen_HasUseStmts.classes:
                self.insert_in_use_stmts(item)
                if cur_list is not self.use_stmts:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
            elif item.stmt.__class__ in Gen_HasImportStmts.classes:
                self.insert_in_import_stmts(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.import_stmts
            elif item.stmt.__class__ in Gen_HasImplicitPart.classes:
                self.insert_in_implicit_part(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and cur_list is not self.implicit_part:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.implicit_part
            elif item.stmt.__class__ in Gen_HasDeclConstruct.classes:
                self.insert_in_decl_construct(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and \
                    cur_list is not self.implicit_part and cur_list is not self.decl_construct:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.decl_construct
            elif item.stmt.__class__ in Gen_HasExecutionPart.classes:
                self.insert_in_exe_part(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and \
                    cur_list is not self.implicit_part and cur_list is not self.decl_construct and \
                    cur_list is not self.exe_part:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.exe_part
            elif item.stmt.__class__ in Gen_HasContainsStmt.classes:
                self.insert_in_contains_stmt(item)
                if cur_list is not self.use_stmts and cur_list is not self.import_stmts and \
                    cur_list is not self.implicit_part and cur_list is not self.decl_construct and \
                    cur_list is not self.exe_part:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.tostr())
                else:
                    cur_list = self.contains_stmt
            elif item.stmt.__class__ in Gen_HasSubprograms.classes:
                self.insert_in_subprograms(item)
                if cur_list is not self.contains_stmt and cur_list is not self.subprograms:
                    raise ProgramException('Wrong sequence of stmt type: %s'%item.stmt.__class__)
                else:
                    cur_list = self.subprograms
            elif item.stmt.__class__ in [ EndSubroutine, EndFunction ]:
                self.end_stmt = item
                cur_list = None
            elif item.stmt.__class__ is Comment:
                cur_list.append(item)
            else:
                raise ProgramException('Unknown stmt type: %s'%item.stmt.__class__)

            item.process()
    def tostr_subp(self, indent):
        lines = []
        lines.extend(self.tostr_list(self.use_stmts, indent))
        lines.extend(self.tostr_list(self.import_stmts, indent))
        lines.extend(self.tostr_list(self.implicit_part, indent))
        lines.extend(self.tostr_list(self.decl_construct, indent))
        lines.extend(self.tostr_list(self.exe_part, indent))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt, indent))
        elif len(self.subprograms)>0:
            lines.append('CONTAINS')
        lines.extend(self.tostr_list(self.subprograms, indent))
        if self.end_stmt:
            lines.append(self.end_stmt.tostr())
        return lines

class GenK_SubProgramStatement(GenK_BeginStatement, Gen_SubProgramStatement, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):
    def __init__(self, node, k_id):
        super(GenK_SubProgramStatement, self).__init__(node, k_id)

    def process(self):

        # process first line
        super(GenK_BeginStatement, self).process()

        if self.isvalid:
            self.process_subp_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = super(GenK_BeginStatement, self).tostr()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_subp(self.indent+TAB))
            return '\n'.join(lines)
        else:
            return

class GenS_SubProgramStatement(GenS_BeginStatement, Gen_SubProgramStatement, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):
    def __init__(self, node, k_id):
        super(GenS_SubProgramStatement, self).__init__(node, k_id)

    def process(self):

        # process first line
        super(GenS_BeginStatement, self).process()

        if self.isvalid:
            self.process_subp_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = super(GenS_BeginStatement, self).tostr()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_subp(self.indent+TAB))
        
            return '\n'.join(lines)
        else:
            return


########### BeginSource ############
class Gen_BeginSource(Gen_BeginStatement):
    def __init__(self, node, k_id):
        super(Gen_BeginSource, self).__init__(node, k_id)

    def gensrc(self, fd):
        self.process()
        lines = self.tostr()
        if lines is not None: fd.write(lines)

class GenK_BeginSource(Gen_BeginSource, GenK_BeginStatement):
    pass

class GenS_BeginSource(Gen_BeginSource, GenS_BeginStatement):
    pass

def generate_srcfiles():
    """Generate source files."""
    from block_statements import Program, Module

    # create state directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

#    if isinstance(State.topblock['stmt'], Program):
#        Logger.major("Callsite statement can not be in Program unit.", stdout=True)
#        sys.exit(-1)
#    elif not isinstance(State.topblock['stmt'], Module):
#        raise ProgramException('Unknown parent type: %s' % State.topblock['stmt'].__class__)

    # generate source files
    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
        filename = os.path.basename(filepath)
        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_kernel(srcobj.tree.geninfo):
            kfile = genkobj(srcobj.tree, 0)
            if kfile is None:
                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
            else:
                with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as f:
                    kfile.gensrc(f)

        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
            sfile = gensobj(srcobj.tree, 0)
            if sfile is None:
                raise ProgramException('Instrumented source file is not generated for %s.'%filepath)
            else:
                with open('%s/%s'%(Config.path['state'], filename), 'wb') as f:
                    sfile.gensrc(f)

    State.state = State.STATE_GENERATED

