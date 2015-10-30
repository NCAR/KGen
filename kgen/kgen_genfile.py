# kgen_genfile.py
#

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType
from kgen_state import State
from statements import Comment

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
        except NameError as e:
            #print node.__class__, cls.__class__,  e
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

class Gen_Has_(object):

    def _hasname_in_list(self, items, name, cls):
        for item in items:
            if isinstance(item, cls):
                if item.name==name: return True
                elif item.tokgen_attrs.has_key('name') and item.tokgen_attrs['name']==name: return True
        return False

class Gen_Has_UseStmts(Gen_Has_):
    from statements import Use
    classes = [ Use ]
    def __init__(self):
        self.use_stmts = []

    def insert_in_use_stmts(self, item):
        self.use_stmts.append(item)

    def hasname_in_use_stmts(self, name, cls):
        return self._hasname_in_list(self.use_stmts, name, cls)

class Gen_Has_ImportStmts(Gen_Has_):
    from statements import Import
    classes = [ Import ]
    def __init__(self):
        self.import_stmts = []

    def insert_in_import_stmts(self, item):
        self.import_stmts.append(item)

    def hasname_in_import_stmts(self, name, cls):
        return self._hasname_in_list(self.import_stmts, name, cls)

class Gen_Has_ImplicitPart(Gen_Has_):
    from block_statements import implicit_part
    classes = implicit_part
    def __init__(self):
        self.implicit_part = []

    def insert_in_implicit_part(self, item):
        self.implicit_part.append(item)

    def hasname_in_implicit_part(self, name, cls):
        return self._hasname_in_list(self.implicit_part, name, cls)

class Gen_Has_DeclConstruct(Gen_Has_):
    from block_statements import declaration_construct
    classes = declaration_construct
    def __init__(self):
        self.decl_construct = []

    def insert_in_decl_construct(self, item):
        self.decl_construct.append(item)

    def hasname_in_decl_construct(self, name, cls):
        return self._hasname_in_list(self.decl_construct, name, cls)

class Gen_Has_ExecutionPart(Gen_Has_):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        self.exe_part = []

    def insert_in_exe_part(self, item):
        self.exe_part.append(item)

    def hasname_in_exe_part(self, name, cls):
        return self._hasname_in_list(self.exe_part, name, cls)

class Gen_Has_ContainsStmt(Gen_Has_):
    from statements import Contains
    classes = [ Contains ]
    def __init__(self):
        self.contains_stmt = []

    def insert_in_contains_stmt(self, item):
        self.contains_stmt.append(item)

    def hasname_in_contains_stmt(self, name, cls):
        return self._hasname_in_list(self.contains_stmt, name, cls)

class Gen_Has_Subprograms(Gen_Has_):
    from block_statements import internal_subprogram
    classes =  internal_subprogram
    def __init__(self):
        self.subprograms = []

    def insert_in_subprograms(self, item):
        self.subprograms.append(item)

    def hasname_in_subprograms(self, name, cls):
        return self._hasname_in_list(self.subprograms, name, cls)

class Gen_Has_TypeParamDefStmts(Gen_Has_):
    from typedecl_statements import Integer
    classes =  [ Integer ]
    def __init__(self):
        self.type_param_def_stmts = []

    def insert_in_type_param_def_stmts(self, item):
        self.type_param_def_stmts.append(item)

    def hasname_in_type_param_def_stmts(self, name, cls):
        return self._hasname_in_list(self.type_param_def_stmts, name, cls)

class Gen_Has_PrivateOrSequence(Gen_Has_):
    from block_statements import private_or_sequence
    classes =  private_or_sequence
    def __init__(self):
        self.private_or_sequence = []

    def insert_in_private_or_sequence(self, item):
        self.private_or_sequence.append(item)

    def hasname_in_private_or_sequence(self, name, cls):
        return self._hasname_in_list(self.private_or_sequence, name, cls)

class Gen_Has_ComponentPart(Gen_Has_):
    from block_statements import component_part
    classes =  component_part
    def __init__(self):
        self.comp_part = []

    def insert_in_comp_part(self, item):
        self.comp_part.append(item)

    def hasname_in_comp_part(self, name, cls):
        return self._hasname_in_list(self.comp_part, name, cls)


class Gen_Has_TypeBoundProcedurePart(Gen_Has_):
    from block_statements import type_bound_procedure_part
    classes =  type_bound_procedure_part
    def __init__(self):
        self.type_bound_proc_part = []

    def insert_in_type_bound_proc_part(self, item):
        self.type_bound_proc_part.append(item)

    def hasname_in_type_bound_proc_part(self, name, cls):
        return self._hasname_in_list(self.type_bound_proc_part, name, cls)

########### Statement ############
class Gen_Statement(object):
    gen_attrs = {'indent': ''}

    def __init__(self, node, k_id):
        from base_classes import Statement

        self.isvalid = True
        self.k_id = k_id
        self.stmt = node
        self.name = getattr(node, 'name', None)
        if isinstance(self.stmt, Statement):
            self.stmt.genpair = self
        self.parent = None
        self.tokgen_attrs = {}

    def tostr(self):
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

                if self.__class__ in [GenK_ElseIf, GenK_Else]:
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
        assert stmt
        return stmt.ancestors()[0].name

    def get_dtype_subpname(self, stmt):
        assert stmt
        return '%s_%s'%(self._get_topname(stmt), stmt.name)

    def get_typedecl_subpname(self, stmt):
        assert stmt

        names = []
        for entity in stmt.entity_decls:
            var = stmt.get_variable(get_entity_name(entity))
            if var is None: return

            l = [ stmt.name ]
            l.extend(list(stmt.selector))
            if var.is_array():
                l.append('dim%d'%var.rank)
            if var.is_pointer():
                l.append('ptr')

            names.append('_'.join(l))
        return names

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

    def process(self):
        super(GenK_Statement, self).process()

    def add_comment(self, comment, items):
        comobj = GenK_Comment(None, self.k_id)
        comobj.parent = self.parent
        comobj.set_attr('comment', comment)
        items(comobj)

    def add_line(self, insert_in, nlines=1):
        for nline in range(nlines):
            comobj = GenK_Comment(None, self.k_id)
            comobj.parent = self.parent
            insert_in(comobj)

class GenS_Statement(Gen_Statement):
    gentype = 'S'

    def __init__(self, node, k_id):
        super(GenS_Statement, self).__init__(node, k_id)

    def genobj(self, node, k_id):
        return gensobj(node, k_id)

    def process(self):
        super(GenS_Statement, self).process()

    def add_comment(self, comment, items):
        comobj = GenS_Comment(None, self.k_id)
        comobj.parent = self.parent
        comobj.set_attr('comment', comment)
        items(comobj)

    def add_line(self, insert_in, nlines=1):
        for nline in range(nlines):
            comobj = GenS_Comment(None, self.k_id)
            comobj.parent = self.parent
            insert_in(comobj)

########### Comment ############
class Gen_Comment(object):
    pass

class GenK_Comment(GenK_Statement, Gen_Comment):
    def tokgen(self, **kwargs):
        if kwargs.has_key('comment'):
            return '! %s'%kwargs['comment']
        else: return ''

class GenS_Comment(GenS_Statement, Gen_Comment):
    def tokgen(self, **kwargs):
        if kwargs.has_key('comment'):
            return '! %s'%kwargs['comment']
        else: return ''

########### Use ############
class Gen_Use(object):
    pass

class GenK_Use(GenK_Statement, Gen_Use):
    def process(self):
        from typedecl_statements import TypeDeclarationStatement
        from block_statements import TypeDecl

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
                    genpair = resstmt.genpair
                    if isinstance(resstmt, TypeDeclarationStatement):
                        resvar = resstmt.get_variable(get_entity_name(resstmt.entity_decls[0]))
                        if resvar.is_parameter() or resvar.is_explicit_shape_array():
                            continue

                        for entity in resstmt.entity_decls:
                            rnames = genpair.get_readnames()
                            for rname in rnames:
                                if not rname in newitems:
                                    newitems.append(rname)
                            vnames = self.get_verifynames()
                            for vname in vnames:
                                if not vname in newitems:
                                    newitems.append(vname)
                    elif isinstance(resstmt, TypeDecl):
                        rname = genpair.get_readname()
                        if rname and not rname in newitems:
                            newitems.append(rname)
                        vname = genpair.get_verifyname()
                        if vname and not vname in newitems:
                            newitems.append(vname)
                    else: raise ProgramException('Wrong stmt type: %s'%resstmt.__class__)

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

class GenS_Use(GenS_Statement, Gen_Use):
    def process(self):
        from typedecl_statements import TypeDeclarationStatement
        from block_statements import TypeDecl

        if self.isvalid:
            if self.stmt and self.stmt.isonly:

                # add read verify perturb for STATE
                newitems = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    resstmt = req.res_stmts[0]
                    genpair = resstmt.genpair
                    if isinstance(genpair, Gen_TypeDeclarationStatement):
                        resvar = resstmt.get_variable(get_entity_name(resstmt.entity_decls[0]))
                        if resvar.is_parameter() or resvar.is_explicit_shape_array():
                            continue

                        for entity in resstmt.entity_decls:
                            rnames = genpair.get_readnames()
                            for rname in rnames:
                                if not rname in newitems:
                                    newitems.append(rname)
                            vnames = self.get_verifynames()
                            for vname in vnames:
                                if not vname in newitems:
                                    newitems.append(vname)
                    elif isinstance(genpair, Gen_TypeDecl):
                        rname = genpair.get_readname()
                        if rname and not rname in newitems:
                            newitems.append(rname)
                        vname = genpair.get_verifyname()
                        if vname and not vname in newitems:
                            newitems.append(vname)
                    else: raise ProgramException('Wrong stmt type: %s'%resstmt.__class__)

                if newitems:
                    useobj = self.parent.create_use(stmt=self.stmt)
                    useobj.set_attr('items', newitems)

                    pubobj = self.parent.create_public()
                    pubobj.set_attr('items', newitems)

########### Public ############
class Gen_Public(object):
    pass

class GenK_Public(GenK_Statement, Gen_Public):

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

class GenS_Public(GenS_Statement, Gen_Public):

    def tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)
        else:
            return 'PUBLIC'

########### Call ############
class Gen_Call(object):
    pass

class GenK_Call(GenK_Statement, Gen_Call):

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
class Gen_TypeDeclarationStatement(object):
    pass

class GenK_TypeDeclarationStatement(GenK_Statement, Gen_TypeDeclarationStatement):

                var = self.stmt.get_variable(get_entity_name(self.stmt.entity_decls[0]))

                parent.add_line(parent.insert_in_subprograms)

                subrobj = parent.create_subroutine()
                subrobj.set_attr('name', vname)
                subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

                #parent.add_line(subrobj.insert_in_use_stmts)

                varnameobj = subrobj.create_typedeclstmt()
                varnameobj.set_attr('typespec', 'CHARACTER')
                varnameobj.set_attr('selector', '*')
                varnameobj.append_attr('attrspec', 'INTENT(IN)')
                varnameobj.append_attr('entity_decl', 'varname')

                checkobj = subrobj.create_typedeclstmt()
                checkobj.set_attr('typespec', 'TYPE')
                checkobj.set_attr('selector', 'check_t')
                checkobj.append_attr('attrspec', 'INTENT(INOUT)')
                checkobj.append_attr('entity_decl', 'check_status')

                varobj = subrobj.create_typedeclstmt(stmt=self.stmt)
                for attrspec in self.stmt.attrspec:
                    if attrspec in ['pointer', 'allocatable']:
                        varobj.append_attr('attrspec', attrspec.upper())
                if var.is_array():
                    varobj.append_attr('attrspec', 'DIMENSION(%s)'% ','.join(':'*var.rank))
                varobj.append_attr('attrspec', 'INTENT(IN)')
                varobj.set_attr('entity_decl', ['var', 'ref_var'])

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
                    self.set_attr('entity_decl', items)
                    if self.stmt.parent is State.parentblock['stmt']:
                        self.set_attr('remove_intent', None)

                # add typedecl items for OUT items
                if out_items:
                    typedecleobj = GenK_TypeDeclarationStatement(self.stmt, self.k_id)
                    typedecleobj.set_attr('entity_decl', out_items)
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

    def gen_verify_subr(self, parent):
        vnames = self.get_verifynames()
        for vname in vnames:
            if not parent.has_name(vname, GenK_Subroutine):

                var = self.stmt.get_variable(get_entity_name(self.stmt.entity_decls[0]))

                parent.add_line(parent.insert_in_subprograms)

                subrobj = parent.create_subroutine()
                subrobj.set_attr('name', vname)
                subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

                #parent.add_line(subrobj.insert_in_use_stmts)

                varnameobj = subrobj.create_typedeclstmt()
                varnameobj.set_attr('typespec', 'CHARACTER')
                varnameobj.set_attr('selector', '*')
                varnameobj.append_attr('attrspec', 'INTENT(IN)')
                varnameobj.append_attr('entity_decl', 'varname')

                checkobj = subrobj.create_typedeclstmt()
                checkobj.set_attr('typespec', 'TYPE')
                checkobj.set_attr('selector', 'check_t')
                checkobj.append_attr('attrspec', 'INTENT(INOUT)')
                checkobj.append_attr('entity_decl', 'check_status')

                varobj = subrobj.create_typedeclstmt(stmt=self.stmt)
                for attrspec in self.stmt.attrspec:
                    if attrspec in ['pointer', 'allocatable']:
                        varobj.append_attr('attrspec', attrspec.upper())
                if var.is_array():
                    varobj.append_attr('attrspec', 'DIMENSION(%s)'% ','.join(':'*var.rank))
                varobj.append_attr('attrspec', 'INTENT(IN)')
                varobj.set_attr('entity_decl', ['var', 'ref_var'])

                subrobj.add_line(subrobj.insert_in_exe_part)

                ifallocobj = None
                if var.is_allocatable():
                    ifallocobj = subrobj.create_ifthen()
                    ifallocobj.set_attr('expr', 'ALLOCATED(var)')

                ifassocobj = None
                if var.is_pointer():
                    ifassocobj = subrobj.create_ifthen()
                    ifassocobj.set_attr('expr', 'ASSOCIATED(var)')

                assert not (ifallocobj and ifassocobj)
                ifcheckobj = ifallocobj if ifallocobj else ifassocobj

                if ifcheckobj: topobj = ifcheckobj
                else: topobj = subrobj

                incobj = topobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numTotal')
                incobj.set_attr('rhs', 'check_status%numTotal + 1')

                ifcmpobj = topobj.create_ifthen()
                ifcmpobj.set_attr('expr', 'ALL( var == ref_var )')

                incobj = ifcmpobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numIdentical')
                incobj.set_attr('rhs', 'check_status%numIdentical + 1')

                ifverbose1obj = ifcmpobj.create_ifthen()
                ifverbose1obj.set_attr('expr', 'check_status%verboseLevel > 1')

                write1obj = ifverbose1obj.create_write()

                write2obj = ifverbose1obj.create_write()
                write2obj.set_attr('item_list', ['"All elements of "','trim(adjustl(varname))','" are IDENTICAL."'])

                ifverbose2obj = ifverbose1obj.create_ifthen()
                ifverbose2obj.set_attr('expr', 'ALL( var == 0 )')

                ifverbose3obj = ifverbose2obj.create_ifthen()
                ifverbose3obj.set_attr('expr', 'check_status%verboseLevel > 2')

                write3obj = ifverbose3obj.create_write()
                write3obj.set_attr('item_list', ['"All values are zero."'])

                endobj = ifverbose3obj.create_endobj()
                endobj.set_attr('blockname', 'IF')

                endobj = ifverbose2obj.create_endobj()
                endobj.set_attr('blockname', 'IF')

                endobj = ifverbose1obj.create_endobj()
                endobj.set_attr('blockname', 'IF')

                # top else
                ifcmpobj.create_else()

                ifverbose4obj = ifcmpobj.create_ifthen()
                ifverbose4obj.set_attr('expr', 'check_status%verboseLevel > 0')

                write4obj = ifverbose4obj.create_write()

                write5obj = ifverbose4obj.create_write()
                write5obj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL."'])

                write6obj = ifverbose4obj.create_write()
                write6obj.set_attr('item_list', ['count( var /= ref_var)', '" of "', 'size( var )', '" elements are different."'])

                endobj = ifverbose4obj.create_endobj()
                endobj.set_attr('blockname', 'IF')

                incobj = ifcmpobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numFatal')
                incobj.set_attr('rhs', 'check_status%numFatal + 1')

                endobj = ifcmpobj.create_endobj()
                endobj.set_attr('blockname', 'IF')


                if ifassocobj:
                    endobj = ifassocobj.create_endobj()
                    endobj.set_attr('blockname', 'IF')

                if ifallocobj:
                    endobj = ifallocobj.create_endobj()
                    endobj.set_attr('blockname', 'IF')

                # create end subroutine
                endsubrobj = subrobj.create_endobj()
                endsubrobj.set_attr('name', vname)
                endsubrobj.set_attr('blockname', 'SUBROUTINE')

    def get_readnames(self):
        subpnames = self.get_typedecl_subpname(self.stmt)
        if subpnames: return [ 'kr_%s'%sname for sname in subpnames ]
        else: return []

    def get_verifynames(self):
        subpnames = self.get_typedecl_subpname(self.stmt)
        if subpnames: return [ 'kv_%s'%sname for sname in subpnames ]
        else: return []

class GenS_TypeDeclarationStatement(GenS_Statement, Gen_TypeDeclarationStatement):
    def process(self):
        if self.isvalid:
            if self.stmt:
                pass

    def get_writenames(self):
        subpnames = self.get_typedecl_subpname(stmt)
        if subpnames: return [ 'kw_%s'%sname for sname in subpnames ]
        else: return []

########### Assignment ############
class Gen_Assignment(object):
    pass

class GenK_Assignment(GenK_Statement, Gen_Assignment):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('lhs')
        assert kwargs.has_key('rhs')

        return '%s = %s'%(kwargs['lhs'], kwargs['rhs'])

########### ElseIf ############
class Gen_ElseIf(object):
    pass

class GenK_ElseIf(GenK_Statement, Gen_ElseIf):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'ELSE IF( %s ) THEN'%kwargs['expr']

########### ElseIf ############
class Gen_Else(object):
    pass

class GenK_Else(GenK_Statement, Gen_Else):

    def tokgen(self, **kwargs):
        return 'ELSE'

########### Write ############
class Gen_Write(object):
    pass

class GenK_Write(GenK_Statement, Gen_Write):

    def tokgen(self, **kwargs):
        if kwargs.has_key('ctrl_list'): ctrls = kwargs['ctrl_list']
        else: ctrls = ['*','*']
        if kwargs.has_key('item_list'): items = kwargs['item_list']
        else: items = []

        return 'WRITE(%s) %s'%(','.join(ctrls), ', '.join(items))

class GenS_Write(GenS_Statement, Gen_Write):

    def tokgen(self, **kwargs):
        if kwargs.has_key('ctrl_list'): ctrls = kwargs['ctrl_list']
        else: ctrls = ['*','*']
        if kwargs.has_key('item_list'): items = kwargs['item_list']
        else: items = []

        return 'WRITE(%s) %s'%(','.join(ctrls), ', '.join(items))

########### BeginStatement ############
class Gen_BeginStatement(object):

    def gen_subnodes(self, node):
        if hasattr(node, 'content'):
            for item in node.content:
                childnode = self.genobj(item, self.k_id)
                childnode.parent = self
                self.items.append(childnode)

    def process_items(self):
        for item in self.items:
            item.process()

    def tostr_list(self, items=None):
        if items is None: items = self.items

        lines = []
        for item in items:
            l = item.tostr()
            if l is not None:
                lines.append(l)
        return lines

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

    def create_endobj(self, stmt=None):
        assert hasattr(self, 'end_obj')

        exec('obj = Gen%s_EndStatement(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.end_obj = obj
        return obj

    def create_subroutine(self, stmt=None):
        assert hasattr(self, 'insert_in_subprograms')

        exec('obj = Gen%s_Subroutine(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_subprograms(obj)
        return obj

    def create_typedeclstmt(self, stmt=None):
        assert hasattr(self, 'insert_in_decl_construct')

        exec('obj = Gen%s_TypeDeclarationStatement(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_decl_construct(obj)
        return obj

    def create_assignstmt(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Assignment(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

        # initialize dtype check status
        checkobj = subrobj.create_callstmt()
        checkobj.set_attr('name', 'kgen_init_check')
        checkobj.set_attr('args', 'dtype_check_status')

    def create_callstmt(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Call(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_ifthen(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_IfThen(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_elseif(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_ElseIf(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def create_else(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Else(stmt, self.k_id)'%self.gentype)
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

    def create_write(self, stmt=None):
        assert hasattr(self, 'insert_in_exe_part')

        exec('obj = Gen%s_Write(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        self.insert_in_exe_part(obj)
        return obj

    def has_name(self, name, cls):
        for func in [getattr(self, m) for m in dir(self) if callable(getattr(self, m)) and m.startswith('hasname_in')]:
            if func(name, cls): return True
        return False

class GenK_BeginStatement(GenK_Statement, Gen_BeginStatement):
    def __init__(self, node, k_id):
        import inspect

        self.items = []
        self.end_obj = None

        super(GenK_BeginStatement, self).__init__(node, k_id)
        for cls in inspect.getmro(self.__class__):
            if cls.__name__.startswith('Gen_Has_'):
                cls.__init__(self)

        self.gen_subnodes(node)

    def process_blockhead(self):
        super(GenK_BeginStatement, self).process()

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_items()

    def tostr_blockhead(self):
        return super(GenK_BeginStatement, self).tostr()

    def tostr(self):
        if self.isvalid:
            lines = []
            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_list())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

class GenS_BeginStatement(GenS_Statement, Gen_BeginStatement):
    def __init__(self, node, k_id):
        import inspect

        self.items = []
        self.end_obj = None

        super(GenS_BeginStatement, self).__init__(node, k_id)
        for cls in inspect.getmro(self.__class__):
            if cls.__name__.startswith('Gen_Has_'):
                cls.__init__(self)

        self.gen_subnodes(node)

    def process_blockhead(self):
        super(GenS_BeginStatement, self).process()

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_items()

    def tostr_blockhead(self):
        return super(GenS_BeginStatement, self).tostr()

    def tostr(self):
        if self.isvalid:
            lines = []
            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_list())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

########### EndStatement ############
class Gen_EndStatement(object):
    pass

class GenK_EndStatement(GenK_Statement, Gen_EndStatement):
    def tostr(self):
        if self.isvalid:
            return super(GenK_EndStatement, self).tostr()

    def tokgen(self, **kwargs):
        blockname = None
        if kwargs.has_key('blockname'):
            blockname = kwargs['blockname']
        if blockname is None: raise ProgramException('No block name is provided.')
        
        name = ''
        if kwargs.has_key('name'):
            name = kwargs['name']

        return 'END %s %s'%(blockname.upper(), name)

class GenS_EndStatement(GenS_Statement, Gen_EndStatement):
    def tostr(self):
        if self.isvalid:
            return super(GenS_EndStatement, self).tostr()

    def tokgen(self, **kwargs):
        blockname = None
        if kwargs.has_key('blockname'):
            blockname = kwargs['blockname']
        if blockname is None: raise ProgramException('No block name is provided.')
        
        name = ''
        if kwargs.has_key('name'):
            name = kwargs['name']

        return 'END %s %s'%(blockname.upper(), name)

########### Module ############
class Gen_Module(object):
    def process_module_items(self):
        from block_statements import EndModule

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, \
            Gen_Has_ContainsStmt, Gen_Has_Subprograms]
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

class GenK_Module(GenK_BeginStatement, Gen_Module, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

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

class GenS_Module(GenS_BeginStatement, Gen_Module, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

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

class Gen_EndModule(object):
    pass

class GenK_EndModule(GenK_EndStatement, Gen_EndModule):
    def tostr(self):
        if self.isvalid:
            if self.stmt.parent is State.topblock['stmt']:
                return 'END PROGRAM kernel_%s'%Config.callsite['subpname'].firstpartname()
            else:
                return super(GenK_EndModule, self).tostr()

########### SubProgramStatement ############
class Gen_SubProgramStatement(object):
    def process_subp_items(self):
        from block_statements import EndSubroutine, EndFunction

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, \
            Gen_Has_ContainsStmt, Gen_Has_Subprograms]
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

class GenK_SubProgramStatement(GenK_BeginStatement, Gen_SubProgramStatement, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

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

class GenS_SubProgramStatement(GenS_BeginStatement, Gen_SubProgramStatement, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

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
class Gen_Subroutine(object):
    pass

class GenK_Subroutine(GenK_SubProgramStatement, Gen_Subroutine):

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

class GenS_Subroutine(GenS_SubProgramStatement, Gen_Subroutine):
    pass

########### IfThen ############
class Gen_IfThen(object):
    pass

class GenK_IfThen(GenK_BeginStatement, Gen_IfThen, Gen_Has_ExecutionPart,):

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


########### TypeDecl ############
class Gen_TypeDecl(object):
    def process_type_comps(self):
        from block_statements import EndType
        from typedecl_statements import Integer

        class_order = [Gen_Has_TypeParamDefStmts, Gen_Has_PrivateOrSequence, Gen_Has_ComponentPart, Gen_Has_TypeBoundProcedurePart]
        end_classes = [ EndType ]

        for item in self.items:
            if item.stmt.__class__ is Integer:
                if class_order[0]==Gen_Has_TypeParamDefStmts and (any(attr in ['kind', 'len'] for attr in item.stmt.attrspec) and \
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

    def get_readname(self):
        if self.stmt is None: return
        subpname = self.get_dtype_subpname(self.stmt)
        if subpname: return 'kr_%s'%subpname

    def get_verifyname(self):
        if self.stmt is None: return
        subpname = self.get_dtype_subpname(self.stmt)
        if subpname: return 'kv_%s'%subpname

class GenK_TypeDecl(GenK_BeginStatement, Gen_TypeDecl, Gen_Has_TypeParamDefStmts, Gen_Has_PrivateOrSequence, Gen_Has_ComponentPart, \
    Gen_Has_TypeBoundProcedurePart):

    def gen_read_subr(self, parent, pubobj):
        self.add_comment('reading type variable', parent.insert_in_subprograms)

        subrname = self.get_readname()
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

        for item in self.items:
            if isinstance(item, GenK_TypeDeclarationStatement):
                vnames = item.get_verifynames()
                if not vnames: continue


                enames = [ get_entity_name(e) for e in item.stmt.entity_decls ]
                for vname, ename in zip(vnames, enames):
                    callobj = parent.create_callstmt()
                    callobj.set_attr('name', vname)
                    callobj.set_attr('args', ['"%s"'%ename, 'dtype_check_status', 'var%%%s'%ename, 'ref_var%%%s'%ename])

                    var = item.stmt.get_variable(ename)
                    if var.is_parameter() or var.is_explicit_shape_array():
                        pass
                    else:
                        item.gen_verify_subr(self.parent)

        self.add_line(parent.insert_in_exe_part)

    def gen_verify_subr(self, parent, pubobj):

        self.add_comment('verifying type variable', parent.insert_in_subprograms)

        subrname = self.get_verifyname()
        if not subrname: return

        if not parent.has_name(subrname, Gen_Subroutine):
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

class GenS_TypeDecl(GenS_BeginStatement, Gen_TypeDecl, Gen_Has_TypeParamDefStmts, Gen_Has_PrivateOrSequence, Gen_Has_ComponentPart, \
    Gen_Has_TypeBoundProcedurePart):

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

    def get_writenames(self, stmt):
        subpname = self.get_dtype_subpname(stmt)
        if subpname: return 'kw_%s'%subpname

########### BeginSource ############
class Gen_BeginSource(object):

    def gensrc(self, fd):
        self.process()
        lines = self.tostr()
        if lines is not None: fd.write(lines)

class GenK_BeginSource(GenK_BeginStatement, Gen_BeginSource):
    pass

class GenS_BeginSource(GenS_BeginStatement, Gen_BeginSource):
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

