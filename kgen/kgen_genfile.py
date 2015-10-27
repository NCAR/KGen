# kgen_genfile.py
#

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType
from kgen_state import State
from statements import Comment

#  TRY dict for class variable

########### Common ############

TAB = '    ' 

def get_indent(line):
    import re
    return re.match(r"\s*", line).group()

def _genobj(node, k_id, gentype):
    import inspect
    from typedecl_statements import TypeStmt
    from block_statements import TypeDecl

    obj = None
    for cls in inspect.getmro(node.__class__):    
        try:
            clsname = cls.__name__
            if cls.__name__=='Type':
                if cls is TypeDecl: clsname = 'TypeDecl'
                elif cls is TypeStmt: clsname = 'TypeStmt'
            exec('obj = Gen%s_%s(node, k_id)'%(gentype, clsname))  
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

class GenHasTypeParamDefStmts(object):
    from typedecl_statements import Integer
    classes =  [ Integer ]
    def __init__(self):
        super(GenHasTypeParamDefStmts, self).__init__()
        self.type_param_def_stmts = []

    def insert_in_type_param_def_stmts(self, item):
        self.type_param_def_stmts.append(item)

class GenHasPrivateOrSequence(object):
    from block_statements import private_or_sequence
    classes =  private_or_sequence
    def __init__(self):
        super(GenHasPrivateOrSequence, self).__init__()
        self.private_or_sequence = []

    def insert_in_private_or_sequence(self, item):
        self.private_or_sequence.append(item)

class GenHasComponentPart(object):
    from block_statements import component_part
    classes =  component_part
    def __init__(self):
        super(GenHasComponentPart, self).__init__()
        self.comp_part = []

    def insert_in_comp_part(self, item):
        self.comp_part.append(item)


class GenHasTypeBoundProcedurePart(object):
    from block_statements import type_bound_procedure_part
    classes =  type_bound_procedure_part
    def __init__(self):
        super(GenHasTypeBoundProcedurePart, self).__init__()
        self.type_bound_proc_part = []

    def insert_in_type_bound_proc_part(self, item):
        self.type_bound_proc_part.append(item)

########### Statement ############
class Gen_Statement(object):
    indent = ['']

    def __init__(self, node, k_id):
        super(Gen_Statement, self).__init__()

        self.isvalid = True
        self.k_id = k_id
        self.stmt = node
        self.parent = None
        self.tokgen_attrs = {}

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
                    return lines_str
        elif self.isvalid:
            if self.stmt:
                if len(self.tokgen_attrs)>0:
                    return self.indent[-1] + self.stmt.tokgen(**self.tokgen_attrs)
                else:
                    if hasattr(self.stmt.item, 'span'):
                        start = self.stmt.item.span[0]-1
                        end = self.stmt.item.span[1]
                        lines = self.stmt.top.prep[start:end]
                        lines_str = '\n'.join(lines)
                        self.indent[-1] = get_indent(lines_str)
                        return lines_str
                    elif isinstance(self.stmt, BeginSource):
                        pass
                    else:
                        raise ProgramException('Wrong path for tostr')
            else:
                return self.indent[-1] + self.tokgen(**self.tokgen_attrs)

    def tokgen(self, **kwargs):
        raise ProgramException('Inherited class should implement tokgen().')

    def add_attr(self, key, value):
        self.tokgen_attrs[key] = value

    def _get_topname(self, stmt):
        return stmt.ancestors()[0].name

    def get_dtype_subpname(self, stmt):
        return '%s_%s'%(self._get_topname(stmt), stmt.name)

    def get_nondtype_subpname(self, stmt):
        var = stmt.get_variable(stmt.entity_decls[0].split('=')[0])
        if var.is_parameter():
            return
        else:
            # kind, len, dim, pointer, dim, ..
            import pdb; pdb.set_trace()
            return 'test_name'
        pass

    def get_subpname(self, stmt):
        from block_statements import Type
        from typedecl_statements import TypeDeclarationStatement

        if isinstance(stmt, Type):
            return self.get_dtype_subpname(stmt)
        elif isinstance(stmt, TypeDeclarationStatement):
            return self.get_nondtype_subpname(stmt)
        else:
            raise ProgramException('Wrong stmt type: %s'%stmt.__class__)

    def process(self):
        if self.isvalid:
            pass

    def add_comment(self, comment, items):
        comobj = Gen_Comment(None, self.k_id)
        comobj.parent = self.parent
        comobj.add_attr('comment', comment)
        items(comobj)

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
        subpname = self.get_subpname(stmt)
        if subpname:
            return 'kr_%s'%subpname
        else:
            return

    def get_verifyname(self, stmt):
        subpname = self.get_subpname(stmt)
        if subpname:
            return 'kv_%s'%subpname
        else:
            return

class GenS_Statement(Gen_Statement):
    gentype = 'S'

    def __init__(self, node, k_id):
        super(GenS_Statement, self).__init__(node, k_id)

    def genobj(self, node, k_id):
        return gensobj(node, k_id)

    def get_writename(self, stmt):
        subpname = self.get_subpname(stmt)
        if subpname:
            return 'kw_%s'%subpname
        else:
            return

class Gen_Comment(Gen_Statement):
    def tokgen(self, **kwargs):
        comment = None
        if kwargs.has_key('comment'):
            comment = kwargs['comment']
        if comment is None: raise ProgramException('No comment is provided.')

        return '! %s'%comment


########### Use ############
class GenK_Use(GenK_Statement):
    def process(self):
        if self.isvalid:
            if self.stmt and self.stmt.isonly:
                # limit use items for KERNEL
                items = []
                for (uname, req) in self.stmt.geninfo[KGGenType.KERNEL]:
                    if not uname.firstpartname() in items:
                        items.append(uname.firstpartname())
                if items!=self.stmt.items:
                    self.add_attr('items', items)

                # add read verify perturb for STATE
                newitems = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    rname = self.get_readname(req.res_stmts[0])
                    if rname and not rname in newitems:
                        newitems.append(rname)
                    vname = self.get_verifyname(req.res_stmts[0])
                    if vname and not vname in newitems:
                        newitems.append(vname)
                if newitems:
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
        if self.isvalid:
            if self.stmt and self.stmt.isonly:

                # add read verify perturb for STATE
                newitems = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    wname = self.get_writename(req.res_stmts[0])
                    if wname and not wname in newitems:
                        newitems.append(wname)

                if newitems:
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

    def tostr(self):
        if self.isvalid:
            if self.stmt and self.stmt.parent is State.topblock['stmt']:
                return 
            else:
                return super(GenK_Public, self).tostr()

    def tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)
        else:
            return 'PUBLIC'

class GenS_Public(GenS_Statement):

    def tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)
        else:
            return 'PUBLIC'

########### Public ############
class GenK_Call(GenK_Statement):

    def tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        if name is None: raise ProgramException('No subroutine name is provided in call stmt.')
        
        args = ''
        if kwargs.has_key('args'):
            args = ', '.join(kwargs['args'])

        return 'CALL %s( %s )'%(name, args)

########### TypeDeclarationStatement ############

class GenK_TypeDeclarationStatement(GenK_Statement):
    def process(self):
        if self.isvalid:
            if self.stmt and hasattr(self.stmt, 'geninfo'):
                # limit typedecl items for IN items
                items = []
                out_items = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    if not uname.firstpartname() in items:
                        items.append(uname.firstpartname())
                        if KGGenType.is_state_out_inout(req.gentype):
                            out_items.append('ref_'+uname.firstpartname())
                if items and (len(items)!=len(self.stmt.entity_decls) or \
                    any(not self.stmt.entity_decls[i].startswith(item) for i, item in enumerate(items))):
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
        if self.isvalid:
            if self.stmt:
                pass

########### BeginStatement ############
class Gen_BeginStatement(Gen_Statement):
    def __init__(self, node, k_id):
        self.items = []
        self.end_stmt = None
#        self.insert_blist = []
#        self.insert_alist = []

        super(Gen_BeginStatement, self).__init__(node, k_id)
        if hasattr(node, 'content'):
            for item in node.content:
                childnode = self.genobj(item, k_id)
                childnode.parent = self
                self.items.append(childnode)

    def process_blockhead(self):
        super(Gen_BeginStatement, self).process()

    def process_items(self):
        for item in self.items:
            item.process()

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_items()

    def tostr_blockhead(self):
        return super(Gen_BeginStatement, self).tostr()

    def tostr_list(self, items=None):
        if items is None: items = self.items

        lines = []
        for item in items:
            l = item.tostr()
            if l is not None:
                lines.append(l)
        return lines

    def tostr(self):
        if self.isvalid:
            lines = []
            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_list())
            return '\n'.join(lines)

    def insert_in_order(self, item, class_order, end_classes):
        def insert_in_list(cls, item):
            from types import FunctionType
            for attrname, attrobj in cls.__dict__.iteritems():
                if type(attrobj) == FunctionType and attrname.startswith('insert_in_'):
                    func = getattr(self, attrname)
                    func(item)
                    break

        if item.stmt.__class__ in end_classes:
            self.end_stmt = item
            return class_order
        elif item.stmt.__class__ is Comment:
            insert_in_list(class_order[0], item)
            return class_order

        classes = []
        matched = False
        for blockcls in class_order:
            if item.stmt.__class__ in blockcls.classes:
                matched = True
                insert_in_list(blockcls, item)
            if matched:
                classes.append(blockcls)

        if not matched:
            raise ProgramException('Wrong sequence of stmt type: %s'%item.stmt.__class__)

        return classes

class GenK_BeginStatement(Gen_BeginStatement, GenK_Statement):
    pass

class GenS_BeginStatement(Gen_BeginStatement, GenS_Statement):
    pass

class Gen_EndStatement(Gen_Statement):
    def pop_indent(self):
        last_indent = self.indent[-1]
        self.indent = self.indent[:-1]
        return last_indent

    def tostr(self):
        if self.isvalid:
            self.pop_indent()
            return super(Gen_EndStatement, self).tostr()

    def tokgen(self, **kwargs):
        blockname = None
        if kwargs.has_key('blockname'):
            blockname = kwargs['blockname']
        if blockname is None: raise ProgramException('No block name is provided.')
        
        name = ''
        if kwargs.has_key('name'):
            name = kwargs['name']

        self.pop_indent()
        return 'END %s %s'%(blockname.upper(), name)


class GenK_EndStatement(Gen_EndStatement):
    pass

class GenS_EndStatement(Gen_EndStatement):
    pass

########### Module ############
class Gen_Module(object):
    def process_module_items(self):
        from block_statements import EndModule

        class_order = [Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, Gen_HasDeclConstruct, Gen_HasExecutionPart, \
            Gen_HasContainsStmt, Gen_HasSubprograms]
        end_classes = [ EndModule ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_module(self):
        lines = []
        lines.extend(self.tostr_list(self.use_stmts))
        lines.extend(self.tostr_list(self.import_stmts))
        lines.extend(self.tostr_list(self.implicit_part))
        lines.extend(self.tostr_list(self.decl_construct))
        lines.extend(self.tostr_list(self.exe_part))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        elif len(self.subprograms)>0:
            lines.append('CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        if self.end_stmt:
            lines.append(self.end_stmt.tostr())
        return lines

class GenK_Module(GenK_BeginStatement, Gen_Module, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_module_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            if self.stmt is State.topblock['stmt']:
                l = 'PROGRAM kernel_%s'%Config.callsite['subpname'].firstpartname()
            else:
                l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_module())
            return '\n'.join(lines)

class GenK_EndModule(GenK_EndStatement):
    def tostr(self):
        if self.isvalid:
            if self.stmt.parent is State.topblock['stmt']:
                self.pop_indent()
                return 'END PROGRAM kernel_%s'%Config.callsite['subpname'].firstpartname()
            else:
                return super(GenK_EndModule, self).tostr()

class GenS_Module(GenS_BeginStatement, Gen_Module, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_module_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_module())
        
            return '\n'.join(lines)


########### SubProgramStatement ############
class Gen_SubProgramStatement(object):
    def process_subp_items(self):
        from block_statements import EndSubroutine, EndFunction

        class_order = [Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, Gen_HasDeclConstruct, Gen_HasExecutionPart, \
            Gen_HasContainsStmt, Gen_HasSubprograms]
        end_classes = [ EndSubroutine, EndFunction ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_subp(self):
        lines = []
        lines.extend(self.tostr_list(self.use_stmts))
        lines.extend(self.tostr_list(self.import_stmts))
        lines.extend(self.tostr_list(self.implicit_part))
        lines.extend(self.tostr_list(self.decl_construct))
        lines.extend(self.tostr_list(self.exe_part))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        elif len(self.subprograms)>0:
            lines.append('CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        if self.end_stmt:
            lines.append(self.end_stmt.tostr())
        return lines

class GenK_SubProgramStatement(GenK_BeginStatement, Gen_SubProgramStatement, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            # process first line
            self.process_blockhead()

            if self.stmt and hasattr(self.stmt, 'ancestor_callsite') and self.stmt.ancestor_callsite:
                callobj = GenK_Call(None, self.k_id)
                callobj.add_attr('name', self.stmt.name)
                self.parent.insert_in_exe_part(callobj)
   
            self.process_subp_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_subp())
            return '\n'.join(lines)

class GenS_SubProgramStatement(GenS_BeginStatement, Gen_SubProgramStatement, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_subp_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_subp())
        
            return '\n'.join(lines)


########### Type ############
class GenK_Subroutine(GenK_SubProgramStatement):

    def tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        if name is None: raise ProgramException('No subroutine name is provided in call stmt.')
        
        args = ''
        if kwargs.has_key('args'):
            args = ', '.join(kwargs['args'])

        return 'SUBROUTINE %s( %s )'%(name, args)

class GenS_Subroutine(GenS_SubProgramStatement):
    pass


########### Type ############
class Gen_TypeDecl(object):
    def process_type_comps(self):
        from block_statements import EndType
        from typedecl_statements import Integer

        class_order = [GenHasTypeParamDefStmts, GenHasPrivateOrSequence, GenHasComponentPart, GenHasTypeBoundProcedurePart]
        end_classes = [ EndType ]

        for item in self.items:
            if item.stmt.__class__ is Integer:
                if class_order[0]==GenHasTypeParamDefStmts and (any(attr in ['kind', 'len'] for attr in item.stmt.attrspec) and \
                    all(decl.find('=')>0 for decl in item.stmt.entity_decls)):
                    class_order = self.insert_in_order(item, class_order, end_classes)
                else:
                    class_order = self.insert_in_order(item, class_order[1:], end_classes)
            else:
                class_order = self.insert_in_order(item, class_order, end_classes)

            item.process()

    def tostr_typedecl(self):
        lines = []
        lines.extend(self.tostr_list(self.type_param_def_stmts))
        lines.extend(self.tostr_list(self.private_or_sequence))
        lines.extend(self.tostr_list(self.comp_part))
        lines.extend(self.tostr_list(self.type_bound_proc_part))
        if self.end_stmt:
            lines.append(self.end_stmt.tostr())
        return lines

class GenK_TypeDecl(GenK_BeginStatement, Gen_TypeDecl, GenHasTypeParamDefStmts, GenHasPrivateOrSequence, GenHasComponentPart, \
    GenHasTypeBoundProcedurePart):

    def gen_read_subr(self):
        self.add_comment('reading type variable', self.parent.insert_in_subprograms)

        subrname = self.get_readname(self.stmt)
        subrobj = GenK_Subroutine(None, self.k_id)
        subrobj.parent = self.parent
        subrobj.add_attr('name', subrname)
        subrobj.add_attr('args', ['a', 'b', 'c'])
        self.parent.insert_in_subprograms(subrobj)

        endsubrobj = GenK_EndStatement(None, self.k_id)
        endsubrobj.parent = subrobj
        endsubrobj.add_attr('blockname', 'SUBROUTINE')
        endsubrobj.add_attr('name', subrname)
        subrobj.end_stmt = endsubrobj

        pubobj = GenK_Public(None, self.k_id)
        pubobj.parent = subrobj
        pubobj.add_attr('items', [subrname])
        self.parent.insert_in_decl_construct(pubobj)


    def gen_verify_subr(self):
        self.add_comment('verifying type variable', self.parent.insert_in_subprograms)

        subrname = self.get_verifyname(self.stmt)
        subrobj = GenK_Subroutine(None, self.k_id)
        subrobj.parent = self.parent
        subrobj.add_attr('name', subrname)
        subrobj.add_attr('args', ['a', 'b', 'c'])
        self.parent.insert_in_subprograms(subrobj)

        endsubrobj = GenK_EndStatement(None, self.k_id)
        endsubrobj.parent = subrobj
        endsubrobj.add_attr('blockname', 'SUBROUTINE')
        endsubrobj.add_attr('name', subrname)
        subrobj.end_stmt = endsubrobj

        pubobj = GenK_Public(None, self.k_id)
        pubobj.parent = subrobj
        pubobj.add_attr('items', [subrname])
        self.parent.insert_in_decl_construct(pubobj)

    def process(self):
        if self.isvalid:
            if KGGenType.has_state(self.stmt.geninfo):
                self.gen_read_subr()
                self.gen_verify_subr()

            self.process_blockhead()
            self.process_type_comps()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_typedecl())
            return '\n'.join(lines)

class GenS_TypeDecl(GenS_BeginStatement, Gen_TypeDecl, GenHasTypeParamDefStmts, GenHasPrivateOrSequence, GenHasComponentPart, \
    GenHasTypeBoundProcedurePart):

    def gen_write_subr(self):
        pass

    def process(self):
        if self.isvalid:
            if KGGenType.has_state(self.stmt.geninfo):
                self.gen_write_subr()

            self.process_blockhead()
            self.process_type_comps()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            self.indent.append(self.indent[-1]+TAB)
            lines.extend(self.tostr_typedecl())
            return '\n'.join(lines)


########### BeginSource ############
class Gen_BeginSource(Gen_BeginStatement):

    def gensrc(self, fd):
        self.process()
        lines = self.tostr()
        if lines is not None: fd.write(lines)

class GenK_BeginSource(Gen_BeginSource, GenK_BeginStatement):
    pass

class GenS_BeginSource(Gen_BeginSource, GenS_BeginStatement):
    pass

########### functions ############
def generate_kgen_utils():
    from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr
    with open('%s/kgen_utils.f90'%Config.path['kernel'], 'wb') as f:
        f.write('MODULE kgen_utils_mod')
        f.write(kgen_utils_file_head)
        f.write('CONTAINS\n')
        f.write(kgen_utils_file_checksubr)
        f.write('END MODULE kgen_utils_mod\n')

def generate_srcfiles():
    """Generate files."""
    from block_statements import Program, Module

    # create state directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

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

    # generate kgen_utils.f90 in kernel directory
    generate_kgen_utils()

    State.state = State.STATE_GENERATED

