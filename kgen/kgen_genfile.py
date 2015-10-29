# kgen_genfile.py
#

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType
from kgen_state import State
from statements import Comment

#  TRY dict for class variable

########### Common ############

TAB = ' '*4

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

def get_entity_name(entity):
    from Fortran2003 import Entity_Decl
 
    edecl = Entity_Decl(entity)
    return edecl.items[0].string

class Gen_Has(object):
    def _hasname_in_list(self, items, name, cls):
        for item in items:
            if isinstance(item, cls):
                if item.name==name: return True
                elif item.tokgen_attrs.has_key('name') and item.tokgen_attrs['name']==name: return True
        return False

class Gen_HasUseStmts(Gen_Has):
    from statements import Use
    classes = [ Use ]
    def __init__(self):
        super(Gen_HasUseStmts, self).__init__()
        self.use_stmts = []

    def insert_in_use_stmts(self, item):
        self.use_stmts.append(item)

    def hasname_in_use_stmts(self, name, cls):
        return self._hasname_in_list(self.use_stmts, name, cls)

class Gen_HasImportStmts(Gen_Has):
    from statements import Import
    classes = [ Import ]
    def __init__(self):
        super(Gen_HasImportStmts, self).__init__()
        self.import_stmts = []

    def insert_in_import_stmts(self, item):
        self.import_stmts.append(item)

    def hasname_in_import_stmts(self, name, cls):
        return self._hasname_in_list(self.import_stmts, name, cls)

class Gen_HasImplicitPart(Gen_Has):
    from block_statements import implicit_part
    classes = implicit_part
    def __init__(self):
        super(Gen_HasImplicitPart, self).__init__()
        self.implicit_part = []

    def insert_in_implicit_part(self, item):
        self.implicit_part.append(item)

    def hasname_in_implicit_part(self, name, cls):
        return self._hasname_in_list(self.implicit_part, name, cls)

class Gen_HasDeclConstruct(Gen_Has):
    from block_statements import declaration_construct
    classes = declaration_construct
    def __init__(self):
        super(Gen_HasDeclConstruct, self).__init__()
        self.decl_construct = []

    def insert_in_decl_construct(self, item):
        self.decl_construct.append(item)

    def hasname_in_decl_construct(self, name, cls):
        return self._hasname_in_list(self.decl_construct, name, cls)

class Gen_HasExecutionPart(Gen_Has):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        super(Gen_HasExecutionPart, self).__init__()
        self.exe_part = []

    def insert_in_exe_part(self, item):
        self.exe_part.append(item)

    def hasname_in_exe_part(self, name, cls):
        return self._hasname_in_list(self.exe_part, name, cls)

class Gen_HasContainsStmt(Gen_Has):
    from statements import Contains
    classes = [ Contains ]
    def __init__(self):
        super(Gen_HasContainsStmt, self).__init__()
        self.contains_stmt = []

    def insert_in_contains_stmt(self, item):
        self.contains_stmt.append(item)

    def hasname_in_contains_stmt(self, name, cls):
        return self._hasname_in_list(self.contains_stmt, name, cls)

class Gen_HasSubprograms(Gen_Has):
    from block_statements import internal_subprogram
    classes =  internal_subprogram
    def __init__(self):
        super(Gen_HasSubprograms, self).__init__()
        self.subprograms = []

    def insert_in_subprograms(self, item):
        self.subprograms.append(item)

    def hasname_in_subprograms(self, name, cls):
        return self._hasname_in_list(self.subprograms, name, cls)

class GenHasTypeParamDefStmts(Gen_Has):
    from typedecl_statements import Integer
    classes =  [ Integer ]
    def __init__(self):
        super(GenHasTypeParamDefStmts, self).__init__()
        self.type_param_def_stmts = []

    def insert_in_type_param_def_stmts(self, item):
        self.type_param_def_stmts.append(item)

    def hasname_in_type_param_def_stmts(self, name, cls):
        return self._hasname_in_list(self.type_param_def_stmts, name, cls)

class GenHasPrivateOrSequence(Gen_Has):
    from block_statements import private_or_sequence
    classes =  private_or_sequence
    def __init__(self):
        super(GenHasPrivateOrSequence, self).__init__()
        self.private_or_sequence = []

    def insert_in_private_or_sequence(self, item):
        self.private_or_sequence.append(item)

    def hasname_in_private_or_sequence(self, name, cls):
        return self._hasname_in_list(self.private_or_sequence, name, cls)

class GenHasComponentPart(Gen_Has):
    from block_statements import component_part
    classes =  component_part
    def __init__(self):
        super(GenHasComponentPart, self).__init__()
        self.comp_part = []

    def insert_in_comp_part(self, item):
        self.comp_part.append(item)

    def hasname_in_comp_part(self, name, cls):
        return self._hasname_in_list(self.comp_part, name, cls)


class GenHasTypeBoundProcedurePart(Gen_Has):
    from block_statements import type_bound_procedure_part
    classes =  type_bound_procedure_part
    def __init__(self):
        super(GenHasTypeBoundProcedurePart, self).__init__()
        self.type_bound_proc_part = []

    def insert_in_type_bound_proc_part(self, item):
        self.type_bound_proc_part.append(item)

    def hasname_in_type_bound_proc_part(self, name, cls):
        return self._hasname_in_list(self.type_bound_proc_part, name, cls)

########### Statement ############
class Gen_Statement(object):
    gen_attrs = {'indent': ''}

    def __init__(self, node, k_id):
        super(Gen_Statement, self).__init__()

        self.isvalid = True
        self.k_id = k_id
        self.stmt = node
        self.name = getattr(node, 'name', None)
        self.parent = None
        self.tokgen_attrs = {}

    def tostr(self):
        from statements import Comment, ElseIf
        from block_statements import BeginSource, BeginStatement
        from base_classes import EndStatement

        if isinstance(self.stmt, Comment):
            if not self.stmt.item.comment.startswith('!KGEN#'):
                start = self.stmt.item.span[0]-1
                end = self.stmt.item.span[1]
                lines = self.stmt.top.prep[start:end]
                lines_str = '\n'.join(lines)
                if lines_str.strip().startswith(self.stmt.item.comment.strip()):
                    return lines_str
        elif self.isvalid:
            cur_indent = self.gen_attrs['indent']
            if self.stmt:
                lines_str = None
                if hasattr(self.stmt.item, 'span'):
                    start = self.stmt.item.span[0]-1
                    end = self.stmt.item.span[1]
                    lines = self.stmt.top.prep[start:end]
                    lines_str = '\n'.join(lines)

                if len(self.tokgen_attrs)>0:
                    if isinstance(self.stmt, BeginStatement):
                        self.indent = cur_indent
                        self.gen_attrs['indent'] = cur_indent + TAB
                    elif isinstance(self.stmt, EndStatement):
                        self.gen_attrs['indent'] = self.parent.indent
                    return cur_indent + self.stmt.tokgen(**self.tokgen_attrs)
                else:
                    if lines_str:
                        cur_indent = get_indent(lines_str)
                        if isinstance(self.stmt, BeginStatement):
                            self.indent = cur_indent
                            self.gen_attrs['indent'] = cur_indent + TAB
                        else:
                            self.gen_attrs['indent'] = cur_indent
                        return lines_str
                    elif isinstance(self.stmt, BeginSource):
                        pass
                    else:
                        raise ProgramException('Wrong path for tostr')
            else:
                if isinstance(self, Gen_BeginStatement):
                    self.indent = cur_indent
                    self.gen_attrs['indent'] = cur_indent + TAB
                elif isinstance(self, Gen_EndStatement):
                    self.gen_attrs['indent'] = self.parent.indent
                    cur_indent = self.parent.indent

                if self.__class__ in [GenK_ElseIf]:
                    return self.parent.indent + self.tokgen(**self.tokgen_attrs)
                else:
                    return cur_indent + self.tokgen(**self.tokgen_attrs)

    def tokgen(self, **kwargs):
        import pdb; pdb.set_trace()
        raise ProgramException('Inherited class should implement tokgen().')

    def has_attr(self, key):
        return self.tokgen_attrs.has_key(key)

    def set_attr(self, key, value):
        self.tokgen_attrs[key] = value

    def get_attr(self, key):
        return self.tokgen_attrs[key]

    def append_attr(self, key, value):
        if self.tokgen_attrs.has_key(key):
            self.tokgen_attrs[key].append(value)
        else:
            self.tokgen_attrs[key] = [ value ]

    def extend_attr(self, key, value):
        if self.tokgen_attrs.has_key(key):
            self.tokgen_attrs[key].extend(value)
        else:
            self.tokgen_attrs[key] = value

    def _get_topname(self, stmt):
        return stmt.ancestors()[0].name

    def get_dtype_subpname(self, stmt):
        return '%s_%s'%(self._get_topname(stmt), stmt.name)

    def get_nondtype_subpnames(self, stmt):
        names = []
        for entity in stmt.entity_decls:
            var = stmt.get_variable(get_entity_name(entity))
            l = [ stmt.name ]
            l.extend(list(stmt.selector))
            if var.is_array():
                l.append('dim%d'%var.rank)
            if var.is_pointer():
                l.append('ptr')

            name = '_'.join(l)
            if name in names: names.append(name)
        return names

    def get_subpnames(self, stmt):
        from block_statements import Type
        from typedecl_statements import TypeDeclarationStatement

        if isinstance(stmt, Type):
            return [ self.get_dtype_subpname(stmt) ]
        elif isinstance(stmt, TypeDeclarationStatement):
            return self.get_nondtype_subpnames(stmt)
        else:
            raise ProgramException('Wrong stmt type: %s'%stmt.__class__)

    def process(self):
        if self.isvalid:
            pass

    def add_comment(self, comment, items):
        comobj = Gen_Comment(None, self.k_id)
        comobj.parent = self.parent
        comobj.set_attr('comment', comment)
        items(comobj)

    def add_line(self, insert_in, nlines=1):
        for nline in range(nlines):
            comobj = Gen_Comment(None, self.k_id)
            comobj.parent = self.parent
            insert_in(comobj)

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

    def get_readnames(self, stmt):
        subpnames = self.get_subpnames(stmt)
        if subpnames:
            return [ 'kr_%s'%sname for sname in subpnames ]
        else:
            return

    def get_verifynames(self, stmt):
        subpnames = self.get_subpnames(stmt)
        if subpnames:
            return [ 'kv_%s'%sname for sname in subpnames ]
        else:
            return

class GenS_Statement(Gen_Statement):
    gentype = 'S'

    def __init__(self, node, k_id):
        super(GenS_Statement, self).__init__(node, k_id)

    def genobj(self, node, k_id):
        return gensobj(node, k_id)

    def get_writenames(self, stmt):
        subpnames = self.get_subpnames(stmt)
        if subpnames:
            return [ 'kw_%s'%sname for sname in subpnames ]
        else:
            return

class Gen_Comment(Gen_Statement):
    def tokgen(self, **kwargs):
        comment = None
        if kwargs.has_key('comment'):
            comment = kwargs['comment']
        if comment is None:
            return ''
        else:
            return '! %s'%comment

class GenK_Comment(Gen_Comment):
    pass

class GenS_Comment(Gen_Comment):
    pass

########### Use ############
class GenK_Use(GenK_Statement):
    def process(self):
        from typedecl_statements import TypeDeclarationStatement

        if self.isvalid:
            if self.stmt and self.stmt.isonly:
                # limit use items for KERNEL
                items = []
                for (uname, req) in self.stmt.geninfo[KGGenType.KERNEL]:
                    if not uname.firstpartname() in items:
                        items.append(uname.firstpartname())
                if items!=self.stmt.items:
                    self.set_attr('items', items)

                # add read verify perturb for STATE
                newitems = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    resstmt = req.res_stmts[0]
                    if isinstance(resstmt, TypeDeclarationStatement):
                        resvar = resstmt.get_variable(get_entity_name(resstmt.entity_decls[0]))
                        if resvar.is_parameter() or resvar.is_explicit_shape_array():
                            continue

                        for entity in resstmt.entity_decls:
                            ename = get_entity_name(entity)
                            rname = self.get_readnames(req.res_stmts[0])
                            if rname and not rname in newitems:
                                newitems.append(rname)
                            vname = self.get_verifynames(req.res_stmts[0])
                            if vname and not vname in newitems:
                                newitems.append(vname)
                    else:
                        rname = self.get_readnames(req.res_stmts[0])
                        if rname and not rname in newitems:
                            newitems.append(rname)
                        vname = self.get_verifynames(req.res_stmts[0])
                        if vname and not vname in newitems:
                            newitems.append(vname)
                if newitems:
                    useobj = self.parent.create_use(stmt=self.stmt)
                    useobj.set_attr('items', newitems)

                    pubobj = self.parent.create_public()
                    pubobj.set_attr('items', newitems)

    def tokgen(self, **kwargs):
        assert kwargs.has_key('name')

        if kwargs.has_key('items'):
            return 'USE %s, ONLY : %s'%(kwargs['name'], ', '.join(kwargs['items']))
        else:
            return 'USE %s'%kwargs['name']

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
                    useobj = self.parent.create_use(stmt=self.stmt)
                    useobj.set_attr('items', newitems)

                    pubobj = self.parent.create_public()
                    pubobj.set_attr('items', newitems)

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
                    self.set_attr('items', items)
                    if self.stmt.parent is State.parentblock['stmt']:
                        self.set_attr('remove_intent', None)

                # add typedecl items for OUT items
                if out_items:
                    typedecleobj = GenK_TypeDeclarationStatement(self.stmt, self.k_id)
                    typedecleobj.set_attr('items', out_items)
                    if self.stmt.parent is State.parentblock['stmt']:
                        typedecleobj.set_attr('remove_intent', None)
                    self.parent.insert_in_decl_construct(typedecleobj)

    def tokgen(self, **kwargs):
        typespec = None
        if kwargs.has_key('typespec'):
            typespec = kwargs['typespec']
        if typespec is None: raise ProgramException('No typespecis provided.')
        
        selector = ''
        if kwargs.has_key('selector'):
            selector = '( %s )'%kwargs['selector']

        attrspec = ''
        if kwargs.has_key('attrspec'):
            attrspec = ', ' + ', '.join(kwargs['attrspec'])

        entity_decl = ''
        if kwargs.has_key('entity_decl'):
            entity_decl = ' ' + ', '.join(kwargs['entity_decl'])


        return '%s%s%s ::%s'%(typespec, selector, attrspec, entity_decl)

    def gen_verify_subr(self, parent, entity):
        vname = self.get_verifynames(item.stmt)
        if not parent.has_name(vname, GenK_Subroutine):
            parent.add_line(parent.insert_in_subprograms)

            subrobj = parent.create_subroutine()
            subrobj.set_attr('name', vname)
            subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

            # create end subroutine
            endsubrobj = subrobj.create_endobj()
            endsubrobj.set_attr('name', vname)
            endsubrobj.set_attr('blockname', 'SUBROUTINE')

class GenS_TypeDeclarationStatement(GenS_Statement):
    def process(self):
        if self.isvalid:
            if self.stmt:
                pass

########### Assignment ############
class GenK_Assignment(GenK_Statement):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('lhs')
        assert kwargs.has_key('rhs')

        return '%s = %s'%(kwargs['lhs'], kwargs['rhs'])

########### ElseIf ############
class GenK_ElseIf(GenK_Statement):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'ELSE IF( %s ) THEN'%kwargs['expr']

########### BeginStatement ############
class Gen_BeginStatement(Gen_Statement):
    def __init__(self, node, k_id):
        self.items = []
        self.end_obj = None

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

            lines.extend(self.tostr_list())
            if self.end_obj: lines.append(self.end_obj.tostr())

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
            self.end_obj = item
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

    def create_endobj(self):
        assert hasattr(self, 'end_obj')

        obj = Gen_EndStatement(None, self.k_id)
        obj.parent = self
        self.end_obj = obj
        return obj

    def create_subroutine(self):
        assert hasattr(self, 'insert_in_subprograms')

        exec('obj = Gen%s_Subroutine(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_subprograms(obj)
        return obj

    def create_typedeclstmt(self):
        assert hasattr(self, 'insert_in_decl_construct')

        exec('obj = Gen%s_TypeDeclarationStatement(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_decl_construct(obj)
        return obj

    def create_assignstmt(self):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Assignment(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

        # initialize dtype check status
        checkobj = subrobj.create_callstmt()
        checkobj.set_attr('name', 'kgen_init_check')
        checkobj.set_attr('args', 'dtype_check_status')

    def create_callstmt(self):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Call(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_ifthen(self):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_IfThen(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_elseif(self):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_ElseIf(None, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_use(self, stmt=None):
        assert hasattr(self, 'insert_in_use_stmts')

        exec('obj = Gen%s_Use(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_use_stmts(obj)
        return obj

    def create_public(self, stmt=None):
        assert hasattr(self, 'insert_in_decl_construct')

        exec('obj = Gen%s_Public(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_decl_construct(obj)
        return obj

    def has_name(self, name, cls):
        for func in [getattr(self, m) for m in dir(self) if callable(getattr(self, m)) and m.startswith('hasname_in')]:
            if func(name, cls): return True
        return False

class GenK_BeginStatement(Gen_BeginStatement, GenK_Statement):
    pass

class GenS_BeginStatement(Gen_BeginStatement, GenS_Statement):
    pass

class Gen_EndStatement(Gen_Statement):

    def tostr(self):
        if self.isvalid:
            return super(Gen_EndStatement, self).tostr()

    def tokgen(self, **kwargs):
        blockname = None
        if kwargs.has_key('blockname'):
            blockname = kwargs['blockname']
        if blockname is None: raise ProgramException('No block name is provided.')
        
        name = ''
        if kwargs.has_key('name'):
            name = kwargs['name']

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
        return lines

class GenK_Module(GenK_BeginStatement, Gen_Module, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_module_items()
            useobj = self.create_use()
            useobj.set_attr('name', 'kgen_utils_mod')
            useobj.extend_attr('items', ['check_t', 'kgen_init_check'])
   

    def tostr(self):
        if self.isvalid:
            lines = []

            if self.stmt is State.topblock['stmt']:
                l = 'PROGRAM kernel_%s'%Config.callsite['subpname'].firstpartname()
            else:
                l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_module())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

class GenK_EndModule(Gen_EndStatement):
    def tostr(self):
        if self.isvalid:
            if self.stmt.parent is State.topblock['stmt']:
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
            lines.extend(self.tostr_module())
            if self.end_obj: lines.append(self.end_obj.tostr())
        
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
        return lines

class GenK_SubProgramStatement(GenK_BeginStatement, Gen_SubProgramStatement, Gen_HasUseStmts, Gen_HasImportStmts, Gen_HasImplicitPart, \
    Gen_HasDeclConstruct, Gen_HasExecutionPart, Gen_HasContainsStmt, Gen_HasSubprograms):

    def process(self):
        if self.isvalid:
            # process first line
            self.process_blockhead()

            if self.stmt and hasattr(self.stmt, 'ancestor_callsite') and self.stmt.ancestor_callsite:
                callobj = self.parent.create_callstmt()
                callobj.set_attr('name', self.stmt.name)
   
            self.process_subp_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_subp())
            if self.end_obj: lines.append(self.end_obj.tostr())

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

            lines.extend(self.tostr_subp())
            if self.end_obj: lines.append(self.end_obj.tostr())
        
            return '\n'.join(lines)

########### Subroutine ############
class GenK_Subroutine(GenK_SubProgramStatement):

    def tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        if name is None: raise ProgramException('No subroutine name is provided in call stmt.')
        
        args = ''
        if kwargs.has_key('args'):
            args = ', '.join(kwargs['args'])

        prefix = ''
        if kwargs.has_key('prefix'):
            prefix = ', '.join(kwargs['prefix']) + ' '

        suffix = ''
        if kwargs.has_key('suffix'):
            suffix = ' ' + ', '.join(kwargs['suffix'])

        return '%sSUBROUTINE %s( %s )%s'%(prefix, name, args, suffix)

class GenS_Subroutine(GenS_SubProgramStatement):
    pass

########### IfThen ############
class GenK_IfThen(GenK_BeginStatement, Gen_HasExecutionPart,):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'IF ( %s ) THEN'%kwargs['expr']

    def tostr_ifthen(self):
        lines = []
        lines.extend(self.tostr_list(self.exe_part))
        return lines

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_ifthen())
            if self.end_obj: lines.append(self.end_obj.tostr())
            return '\n'.join(lines)


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
        return lines

class GenK_TypeDecl(GenK_BeginStatement, Gen_TypeDecl, GenHasTypeParamDefStmts, GenHasPrivateOrSequence, GenHasComponentPart, \
    GenHasTypeBoundProcedurePart):

    def gen_read_subr(self, parent, pubobj):
        self.add_comment('reading type variable', parent.insert_in_subprograms)

        subrname = self.get_readnames(self.stmt)
        if subrname is None: return

        # create subroutine
        subrobj = parent.create_subroutine()
        subrobj.set_attr('name', subrname)
        subrobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])
        subrobj.append_attr('prefix', 'RECURSIVE')

        # create end subroutine
        endsubrobj = subrobj.create_endobj()
        endsubrobj.set_attr('name', subrname)
        endsubrobj.set_attr('blockname', 'SUBROUTINE')

        return subrobj

    def verify_components(self, parent):
        from base_classes import EndStatement
        from block_statements import EndType

        stmtvar = self.stmt.get_variable(self.stmt.name)
        if stmtvar.is_array():
            pass
        else:
            for item in self.items:
                if isinstance(item, GenK_TypeDeclarationStatement):
                    for entity in item.stmt.entity_decls:
                        ename = get_entity_name(entity)
                        vnames = self.get_verifynames(item.stmt)
                        if not vnames: continue

                        for vname in vnames:
                            callobj = parent.create_callstmt()
                            callobj.set_attr('name', vname)
                            callobj.set_attr('args', ['"%s"'%ename, 'dtype_check_status', 'var%%%s'%ename, 'ref_var%%%s'%ename])

                            item.gen_verify_subr(self.parent, ename)

        TODO: separate get vname wname rname for typedecl and type..., typedelc needs entity ...

            self.add_line(parent.insert_in_exe_part)

                    # create call stmt
                    # defer subroutine generation if typedecl is stmt or through use stmt?
                    # isintrinsic???
                    # array pointer??

#                DO idx1=LBOUND(var,1), UBOUND(var,1)
#                    CALL kgen_verify_mod10(varname, dtype_check_status, var(idx1), ref_var(idx1))
#                END DO

#        CALL kgen_verify_real_real_kind_dim2("dvv", dtype_check_status, var%dvv, ref_var%dvv)
#        CALL kgen_verify_real_real_kind_dim2("dvv_diag", dtype_check_status, var%dvv_diag, ref_var%dvv_diag)
#        CALL kgen_verify_real_real_kind_dim2("dvv_twt", dtype_check_status, var%dvv_twt, ref_var%dvv_twt)
#        CALL kgen_verify_real_real_kind_dim2("mvv_twt", dtype_check_status, var%mvv_twt, ref_var%mvv_twt)
#        CALL kgen_verify_real_real_kind_dim2("mfvm", dtype_check_status, var%mfvm, ref_var%mfvm)
#        CALL kgen_verify_real_real_kind_dim2("cfvm", dtype_check_status, var%cfvm, ref_var%cfvm)
#        CALL kgen_verify_real_real_kind_dim2("sfvm", dtype_check_status, var%sfvm, ref_var%sfvm)
#        CALL kgen_verify_real_real_kind_dim2("legdg", dtype_check_status, var%legdg, ref_var%legdg)

    def gen_verify_subr(self, parent, pubobj):

        self.add_comment('verifying type variable', parent.insert_in_subprograms)

        subrnames = self.get_verifynames(self.stmt)
        if not subrnames: return
        elif len(subrnames)>1: raise ProgramException('More than one subroutine names: %s'%subrnames)
        subrname = subrnames[0]

        # subroutine
        subrobj = parent.create_subroutine()
        subrobj.set_attr('name', subrname)
        subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

        # varname
        vnameobj = subrobj.create_typedeclstmt()
        vnameobj.set_attr('typespec', 'CHARACTER')
        vnameobj.set_attr('selector', '*')
        vnameobj.append_attr('attrspec', 'INTENT(IN)')
        vnameobj.append_attr('entity_decl', 'varname')

        # check_status
        checkobj = subrobj.create_typedeclstmt()
        checkobj.set_attr('typespec', 'TYPE')
        checkobj.set_attr('selector', 'check_t')
        checkobj.append_attr('attrspec', 'INTENT(INOUT)')
        checkobj.append_attr('entity_decl', 'check_status')

        # dtype ckeck status
        dcheckobj = subrobj.create_typedeclstmt()
        dcheckobj.set_attr('typespec', 'TYPE')
        dcheckobj.set_attr('selector', 'check_t')
        dcheckobj.append_attr('entity_decl', 'dtype_check_status')

        # variable
        varobj = subrobj.create_typedeclstmt()
        varobj.set_attr('typespec', 'TYPE')
        varobj.set_attr('selector', self.stmt.name)
        varobj.append_attr('attrspec', 'INTENT(IN)')
        varobj.extend_attr('entity_decl', [ 'var', 'ref_var'])
        subrobj.add_line(subrobj.insert_in_decl_construct)

        # increment total check count
        incobj = subrobj.create_assignstmt()
        incobj.set_attr('lhs', 'check_status%numTotal')
        incobj.set_attr('rhs', 'check_status%numTotal + 1')

        # initialize dtype check status
        callobj = subrobj.create_callstmt()
        callobj.set_attr('name', 'kgen_init_check')
        callobj.set_attr('args', ['dtype_check_status'])
        subrobj.add_line(subrobj.insert_in_exe_part)

        # call to verify components
        self.verify_components(subrobj)

        # create if stmt for dtype check result
        ifthenobj = subrobj.create_ifthen()
        ifthenobj.set_attr('expr', 'dtype_check_status%numTotal == dtype_check_status%numIdentical')

        # increment identical check count
        incobj = ifthenobj.create_assignstmt()
        incobj.set_attr('lhs', 'check_status%numIdentical')
        incobj.set_attr('rhs', 'check_status%numIdentical + 1')

        # create else if stmt
        elifobj = ifthenobj.create_elseif()
        elifobj.set_attr('expr', 'dtype_check_status%numFatal > 0')

        # increment fatal check count
        incobj = ifthenobj.create_assignstmt()
        incobj.set_attr('lhs', 'check_status%numFatal')
        incobj.set_attr('rhs', 'check_status%numFatal + 1')

        # create else if stmt
        elifobj = ifthenobj.create_elseif()
        elifobj.set_attr('expr', 'dtype_check_status%numWarning > 0')

        # increment warning check count
        incobj = ifthenobj.create_assignstmt()
        incobj.set_attr('lhs', 'check_status%numWarning')
        incobj.set_attr('rhs', 'check_status%numWarning + 1')

        # create end subroutine
        endifobj = ifthenobj.create_endobj()
        endifobj.set_attr('blockname', 'IF')

        # create end subroutine
        endsubrobj = subrobj.create_endobj()
        endsubrobj.set_attr('blockname', 'SUBROUTINE')
        endsubrobj.set_attr('name', subrname)

        return subrobj

    def process(self):
        if self.isvalid:
            if KGGenType.has_state(self.stmt.geninfo):

                pubobj = self.parent.create_public()

                krsubr = self.gen_read_subr(self.parent, pubobj)
                self.add_line(self.parent.insert_in_subprograms)
                pubobj.append_attr('items', krsubr.get_attr('name'))

                kvsubr = self.gen_verify_subr(self.parent, pubobj)
                self.add_line(self.parent.insert_in_subprograms)
                pubobj.append_attr('items', kvsubr.get_attr('name'))

            self.process_blockhead()
            self.process_type_comps()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_typedecl())
            if self.end_obj: lines.append(self.end_obj.tostr())
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
            lines.extend(self.tostr_typedecl())
            if self.end_obj: lines.append(self.end_obj.tostr())
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

