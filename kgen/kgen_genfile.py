#' kgen_genfile.py
#

import os
import sys
from kgen_utils import Config, ProgramException, KGGenType, pack_exnamepath, match_namepath
from kgen_state import State
from statements import Comment

########### Common ############

#TODO: dummy argument not having intent should be handled with appropriate implicit rules
# parameter var
# add kw_externs_out.. subroutine

TAB = ' '*4
KTYPE = 'K'
STYPE = 'S'

def get_indent(line):
    import re
    return re.match(r"\s*", line).group()

def _genobj(node, k_id, gentype):
    import inspect
    from typedecl_statements import TypeStmt
    from block_statements import TypeDecl

    obj = None
    for i, cls in enumerate(inspect.getmro(node.__class__)):
        try:
            clsname = cls.__name__
            if cls.__name__=='Type':
                if cls is TypeDecl: clsname = 'TypeDecl'
                elif cls is TypeStmt: clsname = 'TypeStmt'
            if i==0 and node is State.parentblock['stmt']: clsname += 'P'
            exec('obj = Gen%s_%s(node, k_id)'%(gentype, clsname))  
            break
        except NameError as e:
            #print node.__class__, cls.__class__,  e
            pass
        except:
            raise
    return obj

def genkobj(node, k_id):
    return _genobj(node, k_id, KTYPE)

def gensobj(node, k_id):
    return _genobj(node, k_id, STYPE)

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

class Gen_Has_ExecutionPart1(Gen_Has_):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        self.exe_part1 = []

    def insert_in_exe_part1(self, item):
        self.exe_part1.append(item)

    def hasname_in_exe_part1(self, name, cls):
        return self._hasname_in_list(self.exe_part1, name, cls)

class Gen_Has_ExecutionPart2(Gen_Has_):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        self.exe_part2 = []

    def insert_in_exe_part2(self, item):
        self.exe_part2.append(item)

    def hasname_in_exe_part2(self, name, cls):
        return self._hasname_in_list(self.exe_part2, name, cls)

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

class Gen_Has_InputModuleState(Gen_Has_):
    from statements import Call
    classes =  [ Call ]
    def __init__(self):
        self.input_module_state = []

    def insert_in_input_module_state(self, item):
        self.input_module_state.append(item)

    def hasname_in_input_module_state(self, name, cls):
        return self._hasname_in_list(self.input_module_state, name, cls)

class Gen_Has_OutputModuleState(Gen_Has_):
    from statements import Call
    classes =  [ Call ]
    def __init__(self):
        self.output_module_state = []

    def insert_in_output_module_state(self, item):
        self.output_module_state.append(item)

    def hasname_in_output_module_state(self, name, cls):
        return self._hasname_in_list(self.output_module_state, name, cls)

class Gen_Has_InputLocalState(Gen_Has_):
    from statements import Read, Write, Call
    classes =  [ Read, Write, Call ]
    def __init__(self):
        self.input_local_state = []

    def insert_in_input_local_state(self, item):
        self.input_local_state.append(item)

    def hasname_in_input_local_state(self, name, cls):
        return self._hasname_in_list(self.input_local_state, name, cls)

class Gen_Has_OutputLocalState(Gen_Has_):
    from statements import Read, Write, Call
    classes =  [ Read, Write, Call ]
    def __init__(self):
        self.output_local_state = []

    def insert_in_output_local_state(self, item):
        self.output_local_state.append(item)

    def hasname_in_output_local_state(self, name, cls):
        return self._hasname_in_list(self.output_local_state, name, cls)

class Gen_Has_CallsiteStmts(Gen_Has_):
    from block_statements import execution_part
    classes = execution_part
    def __init__(self):
        self.callsite_stmts = []

    def insert_in_callsite_stmts(self, item):
        self.callsite_stmts.append(item)

    def hasname_in_callsite_stmts(self, name, cls):
        return self._hasname_in_list(self.callsite_stmts, name, cls)

class Gen_Has_VerifyModuleState(Gen_Has_):
    from statements import Call
    classes =  [ Call ]
    def __init__(self):
        self.verify_module_state = []

    def insert_in_verify_module_state(self, item):
        self.verify_module_state.append(item)

    def hasname_in_verify_module_state(self, name, cls):
        return self._hasname_in_list(self.verify_module_state, name, cls)

class Gen_Has_VerifyLocalState(Gen_Has_):
    from statements import Call
    classes =  [ Call ]
    def __init__(self):
        self.verify_local_state = []

    def insert_in_verify_local_state(self, item):
        self.verify_local_state.append(item)

    def hasname_in_verify_local_state(self, name, cls):
        return self._hasname_in_list(self.verify_local_state, name, cls)

class Gen_Has_MeasureTiming(Gen_Has_):
    from statements import Call, Write, Assignment
    from block_statements import Do
    classes =  [ Call, Do, Assignment, Write ]
    def __init__(self):
        self.measure_timing = []

    def insert_in_measure_timing(self, item):
        self.measure_timing.append(item)

    def hasname_in_measure_timing(self, name, cls):
        return self._hasname_in_list(self.measure_timing, name, cls)

########### Statement ############
class Gen_Statement(object):
    gen_attrs = {'indent': ''}

    def __init__(self, node, k_id):

        self.isvalid = True
        self.k_id = k_id
        self.stmt = node
        self.name = getattr(node, 'name', None)
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

                if self.__class__ in [GenS_ElseIf, GenS_Else, GenS_ElseWhere, GenK_ElseIf, GenK_Else, GenK_ElseWhere]:
                    if not hasattr(self.parent, 'indent'): import pdb; pdb.set_trace()
                    kgenstr = self.tokgen(**self.tokgen_attrs)
                    if not kgenstr is None: return self.parent.indent + kgenstr
                else:
                    kgenstr = self.tokgen(**self.tokgen_attrs)
                    if not kgenstr is None: return cur_indent + kgenstr

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
        assert stmt and hasattr(stmt, 'parent')
        topstmt = stmt.ancestors()[0]
        if hasattr(topstmt, 'name'):
            return topstmt.name 
        else: return ''

    def _get_parentname(self, stmt):
        assert stmt and hasattr(stmt, 'parent')
        parentstmt = stmt.parent
        if hasattr(parentstmt, 'name'):
            return parentstmt.name 
        else: return ''

    def get_dtype_subpname(self, stmt):
        assert stmt
        return '%s_%s'%(self._get_topname(stmt), stmt.name)

    def get_typedecl_subpname(self, stmt, enames=None):
        assert stmt

        if enames is None:
            enames = [ get_entity_name(ename) for ename in stmt.entity_decls]

        prefix = [ self._get_parentname(stmt), stmt.name ] + list(stmt.selector)

        subpnames = []
        for ename in enames:
            var = stmt.get_variable(ename)
            if var is None: break

            l = []
            if var.is_array():
                l.append('dim%d'%var.rank)
            if var.is_pointer():
                l.append('ptr')

            subpnames.append('_'.join(prefix+l))

        return subpnames

    def process(self):
        pass

    def add_comment(self, comment, insert_in, style=None):
        try:
            exec('comobj = Gen%s_Comment(None, self.k_id)'%self.gentype)
        except: raise
        comobj.parent = self.parent
        comobj.set_attr('comment', comment)
        if style: comobj.set_attr('style', style)
        insert_in(comobj)

    def add_openmp(self, directive, insert_in):
        self.add_comment(directive, insert_in, style='openmp')

    def add_line(self, insert_in, nlines=1):
        for nline in range(nlines):
            self.add_comment(None, insert_in)

class GenK_Statement(Gen_Statement):
    gentype = KTYPE

    def __init__(self, node, k_id):
        from base_classes import Statement

        super(GenK_Statement, self).__init__(node, k_id)

        if node: node.genkpair = self

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

class GenS_Statement(Gen_Statement):
    gentype = STYPE

    def __init__(self, node, k_id):
        from base_classes import Statement

        super(GenS_Statement, self).__init__(node, k_id)

        if node: node.genspair = self

    def genobj(self, node, k_id):
        return gensobj(node, k_id)

    def process(self):
        super(GenS_Statement, self).process()

########### Comment ############
class Gen_Comment(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('comment'):
            if kwargs.has_key('style'):
                if kwargs['style']=='openmp':
                    return '!$OMP %s'%kwargs['comment']
                else:
                    return '! %s'%kwargs['comment']
            else:
                if kwargs['comment'] is None:
                    return ''
                else:
                    return '! %s'%kwargs['comment']
        else: return ''

class GenK_Comment(GenK_Statement, Gen_Comment):
    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Comment(GenS_Statement, Gen_Comment):
    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Use ############
class Gen_Use(object):
    def _tokgen(self, **kwargs):
        assert kwargs.has_key('name')

        if kwargs.has_key('items'):
            return 'USE %s, ONLY : %s'%(kwargs['name'], ', '.join(kwargs['items']))
        else:
            return 'USE %s'%kwargs['name']

class GenK_Use(GenK_Statement, Gen_Use):
    def __init__(self, node, k_id):
        super(GenK_Use, self).__init__(node, k_id)

    def process(self):
        # leave items that are actually used in kernel
        if self.isvalid:
            if self.stmt and self.stmt.isonly:
                items = []
                for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                    if not uname.firstpartname() in items:
                        items.append(uname.firstpartname())
                if items!=self.stmt.items:
                    self.set_attr('items', items)

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Use(GenS_Statement, Gen_Use):
    def process(self):
        if self.isvalid:
            if self.stmt and self.stmt.isonly:
                pass

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Public ############
class Gen_Public(object):
    def _tokgen(self, **kwargs):
        items = None
        if kwargs.has_key('items'):
            items = kwargs['items']
        if items:
            return 'PUBLIC %s'%', '.join(items)
        else:
            return 'PUBLIC'

class GenK_Public(GenK_Statement, Gen_Public):

    def tostr(self):
        if self.isvalid:
            if self.stmt and self.stmt.parent is State.topblock['stmt']:
                return 
            else:
                return super(GenK_Public, self).tostr()

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Public(GenS_Statement, Gen_Public):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Call ############
class Gen_Call(object):
    def _tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        if name is None: raise ProgramException('No subroutine name is provided in call stmt.')
        
        args = ''
        if kwargs.has_key('args'):
            args = ', '.join(kwargs['args'])

        return 'CALL %s( %s )'%(name, args)

    pass

class GenK_Call(GenK_Statement, Gen_Call):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Call(GenS_Statement, Gen_Call):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Open ############
class Gen_Open(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('connect_specs'):
            return 'OPEN ( %s )'%', '.join(kwargs['connect_specs'])
        else: raise ProgramException('No connect spec is provided in open stmt.')

class GenK_Open(GenK_Statement, Gen_Open):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Open(GenS_Statement, Gen_Open):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Close ############
class Gen_Close(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('close_specs'):
            return 'CLOSE ( %s )'%', '.join(kwargs['close_specs'])
        else: raise ProgramException('No close spec is provided in close stmt.')

class GenK_Close(GenK_Statement, Gen_Close):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Close(GenS_Statement, Gen_Close):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Stop ############
class Gen_Stop(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('stop_code'):
            return 'STOP %s'%kwargs['stop_code']
        else: return 'STOP'

class GenK_Stop(GenK_Statement, Gen_Stop):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Stop(GenS_Statement, Gen_Stop):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Endfile ############
class Gen_Endfile(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('file_unit'):
            return 'ENDFILE %s'%kwargs['file_unit']
        elif kwargs.has_key('position_specs'):
            return 'ENDFILE ( %s )'%', '.join(kwargs['position_specs'])
        else: raise ProgramException('No file unit or position spec is provided in endfile stmt.')

class GenK_Endfile(GenK_Statement, Gen_Endfile):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Endfile(GenS_Statement, Gen_Endfile):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Endfile ############
class Gen_Implicit(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('implicit_specs'):
            return 'IMPLICIT %s'%', '.join(kwargs['implicit_specs'])
        else:
            return 'IMPLICIT NONE'

class GenK_Implicit(GenK_Statement, Gen_Implicit):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Implicit(GenS_Statement, Gen_Implicit):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### TypeDeclarationStatement ############
class Gen_TypeDeclarationStatement(object):
    def _tokgen(self, **kwargs):
        typespec = None
        if kwargs.has_key('typespec'):
            typespec = kwargs['typespec']
        if typespec is None: raise ProgramException('No typespecis provided.')
        
        selector = ''
        if kwargs.has_key('selector'):
            selector = '( %s )'%kwargs['selector']

        attr_specs = ''
        if kwargs.has_key('attr_specs'):
            attr_specs = ', ' + ', '.join(kwargs['attr_specs'])

        entity_decls = ''
        if kwargs.has_key('entity_decls'):
            entity_decls = ' ' + ', '.join(kwargs['entity_decls'])

        return '%s%s%s ::%s'%(typespec, selector, attr_specs, entity_decls)

class GenK_TypeDeclarationStatement(GenK_Statement, Gen_TypeDeclarationStatement):

    def process(self):

        if self.isvalid:
            # any typedecl stmt used in kernel is marked as input. If it is also used for output, 
            # then it also marked as output
            if self.stmt and hasattr(self.stmt, 'geninfo') and KGGenType.has_state(self.stmt.geninfo) and \
                not 'parameter' in self.stmt.attrspec:

                # following for state read in parent block of callsite and modules
                if isinstance(self.parent, GenK_SubroutineP) or isinstance(self.parent, Gen_Module):
                    #  select IN entities in the typedecl stmt.
                    items = []; decls = []
                    for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                        iname = uname.firstpartname()
                        ename = [ e for e in self.stmt.entity_decls if e. startswith(iname) ]
                        assert len(ename)==1
                        ename = ename[0]
                        if not iname in items:
                            items.append(iname)
                            decls.append(ename)

                    #  select OUT entities in the typedecl stmt.
                    out_items = []; out_decls = []
                    for (uname, req) in KGGenType.get_state_out(self.stmt.geninfo):
                        iname = uname.firstpartname()
                        ename = [ e for e in self.stmt.entity_decls if e. startswith(iname) ]
                        assert len(ename)==1
                        ename = ename[0]
                        if not iname in out_items:
                            out_items.append(iname)
                            out_decls.append('ref_%s'%ename)
                    if out_items:
                        outobj = self.parent.create_typedeclstmt(self.stmt)
                        outobj.set_attr('entity_decls', out_decls)
                        outobj.set_attr('remove_attr', 'intent')

                    # create verify
                    rsubrs = self.gen_verify(out_items)

                    # create reads
                    if isinstance(self.parent, GenK_SubroutineP):
                        self.gen_read(items, out_items)
                    elif isinstance(self.parent, Gen_Module):
                        extinsubrname = 'kr_externs_in_%s'%self.parent.name
                        if len(items)>0 and not self.parent.has_name(extinsubrname, GenK_Subroutine):

                            # add kr_externs subroutine
                            self.parent.add_line(self.parent.insert_in_subprograms)
                            self.parent.add_comment('reading extern input variable', self.parent.insert_in_subprograms)

                            extsubrobj = self.parent.create_subroutine()
                            extsubrobj.set_attr('name', extinsubrname)
                            extsubrobj.set_attr('args', ['kgen_unit'])
                            self.parent.extrinsubr = extsubrobj

                            # kgen_unit
                            unitobj = extsubrobj.create_typedeclstmt()
                            unitobj.set_attr('typespec', 'INTEGER')
                            unitobj.append_attr('attr_specs', 'INTENT(IN)')
                            unitobj.append_attr('entity_decls', 'kgen_unit')

                            endsubrobj = extsubrobj.create_endobj()
                            endsubrobj.set_attr('name', extinsubrname)
                            endsubrobj.set_attr('blockname', 'SUBROUTINE')

                            # add public
                            pubobj = self.parent.create_public()
                            pubobj.append_attr('items', extinsubrname)

                            pblock = State.parentblock['stmt'].genkpair

                            # add use in parentblock
                            useobj = pblock.create_use()
                            useobj.set_attr('name', self.parent.name)
                            useobj.append_attr('items', extinsubrname)

                            # add call in read extern block
                            callobj = pblock.create_callstmt(insert_in=pblock.insert_in_input_module_state)
                            callobj.set_attr('name', extinsubrname)
                            callobj.set_attr('args', ['kgen_unit'])

                        extoutsubrname = 'kr_externs_out_%s'%self.parent.name
                        if len(out_items)>0 and not self.parent.has_name(extoutsubrname, GenK_Subroutine):

                            # add kr_externs subroutine
                            self.parent.add_line(self.parent.insert_in_subprograms)
                            self.parent.add_comment('writing extern output variable', self.parent.insert_in_subprograms)

                            extsubrobj = self.parent.create_subroutine()
                            extsubrobj.set_attr('name', extoutsubrname)
                            extsubrobj.set_attr('args', ['kgen_unit'])
                            self.parent.extroutsubr = extsubrobj

                            # kgen_unit
                            unitobj = extsubrobj.create_typedeclstmt()
                            unitobj.set_attr('typespec', 'INTEGER')
                            unitobj.append_attr('attr_specs', 'INTENT(IN)')
                            unitobj.append_attr('entity_decls', 'kgen_unit')

                            endsubrobj = extsubrobj.create_endobj()
                            endsubrobj.set_attr('name', extoutsubrname)
                            endsubrobj.set_attr('blockname', 'SUBROUTINE')

                            # add public
                            pubobj = self.parent.create_public()
                            pubobj.append_attr('items', extoutsubrname)

                            pblock = State.parentblock['stmt'].genkpair

                            # add use in parentblock
                            useobj = pblock.create_use()
                            useobj.set_attr('name', self.parent.name)
                            useobj.append_attr('items', extoutsubrname)

                            # add call in read extern block
                            callobj = pblock.create_callstmt(insert_in=pblock.insert_in_output_module_state)
                            callobj.set_attr('name', extoutsubrname)
                            callobj.set_attr('args', ['kgen_unit'])

                        self.gen_read(items, out_items)

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

    def gen_verify_subrs_spec(self, subrobj, var):

        # var name
        varnameobj = subrobj.create_typedeclstmt()
        varnameobj.set_attr('typespec', 'CHARACTER')
        varnameobj.set_attr('selector', '*')
        varnameobj.append_attr('attr_specs', 'INTENT(IN)')
        varnameobj.append_attr('entity_decls', 'varname')

        # check_status
        checkobj = subrobj.create_typedeclstmt()
        checkobj.set_attr('typespec', 'TYPE')
        checkobj.set_attr('selector', 'check_t')
        checkobj.append_attr('attr_specs', 'INTENT(INOUT)')
        checkobj.append_attr('entity_decls', 'check_status')

        # dtype ckeck status
        if self.stmt.is_derived():
            dcheckobj = subrobj.create_typedeclstmt()
            dcheckobj.set_attr('typespec', 'TYPE')
            dcheckobj.set_attr('selector', 'check_t')
            dcheckobj.append_attr('entity_decls', 'dtype_check_status')

        # check result
        varnobj = subrobj.create_typedeclstmt()
        varnobj.set_attr('typespec', 'INTEGER')
        varnobj.append_attr('entity_decls', 'check_result') 

        # print check result
        printobj = subrobj.create_typedeclstmt()
        printobj.set_attr('typespec', 'LOGICAL')
        printobj.append_attr('entity_decls', 'is_print = .FALSE.') 

        # typedecl var, ref_var
        varobj = subrobj.create_typedeclstmt(stmt=self.stmt)
        for attrspec in self.stmt.attrspec:
            if attrspec in ['pointer', 'allocatable']:
                varobj.append_attr('attr_specs', attrspec.upper())
        if var.is_array():
            varobj.append_attr('attr_specs', 'DIMENSION(%s)'% ','.join(':'*var.rank))
        varobj.append_attr('attr_specs', 'INTENT(IN)')
        varobj.set_attr('entity_decls', ['var', 'ref_var'])

        # typedecls for array
        typename = self.stmt.name.upper()
        selector = self.stmt._selector_str()
        if var.is_array():
            dim_shape = ','.join(':'*var.rank)
            if self.stmt.is_derived():
                varidxobj = subrobj.create_typedeclstmt()
                varidxobj.set_attr('typespec', 'INTEGER')
                varidxobj.extend_attr('entity_decls', [ 'idx%d'%(r+1) for r in range(var.rank)])

            elif self.stmt.is_numeric():
                varrmsobj = subrobj.create_typedeclstmt()
                varrmsobj.set_attr('typespec', typename)
                varrmsobj.set_attr('selector', selector)
                varrmsobj.extend_attr('entity_decls', ['nrmsdiff', 'rmsdiff'])

                vartempobj = subrobj.create_typedeclstmt()
                vartempobj.set_attr('typespec', typename)
                vartempobj.set_attr('selector', selector)
                vartempobj.append_attr('attr_specs', 'ALLOCATABLE')
                vartempobj.append_attr('attr_specs', 'DIMENSION(%s)'%dim_shape)
                vartempobj.extend_attr('entity_decls', ['temp', 'temp2'])

                varnobj = subrobj.create_typedeclstmt()
                varnobj.set_attr('typespec', 'INTEGER')
                varnobj.append_attr('entity_decls', 'n') 
        elif self.stmt.is_numeric():
            varrmsobj = subrobj.create_typedeclstmt()
            varrmsobj.set_attr('typespec', typename)
            varrmsobj.set_attr('selector', selector)
            varrmsobj.append_attr('entity_decls', 'diff')

    def gen_verify_subrs_check(self, subrobj, topobj, var):
        from statements import Use

        incobj = topobj.create_assignstmt()
        incobj.set_attr('lhs', 'check_status%numTotal')
        incobj.set_attr('rhs', 'check_status%numTotal + 1')

        writeblankobj = topobj.create_write()

        if var.is_array(): # array
            dim_shape = ','.join(':'*var.rank)
            get_size = ','.join(['SIZE(var,dim=%d)'%(dim+1) for dim in range(var.rank)])

            if self.stmt.is_derived():
                # initialize dtype check status
                callinitobj = topobj.create_callstmt()
                callinitobj.set_attr('name', 'kgen_init_check')
                callinitobj.set_attr('args', ['dtype_check_status'])

                pobj = topobj
                doobjs = []
                for d in range(var.rank):
                    doobj = pobj.create_do()
                    doobj.set_attr('loop_control', ' idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1})
                    doobjs.append(doobj)
                    pobj = doobj

                vname = None
                for uname, req in self.stmt.unknowns.iteritems():
                    if uname.firstpartname()==self.stmt.name:
                        vname = 'kv_%s'%self.get_dtype_subpname(req.res_stmts[0])
                        for res_stmt in req.res_stmts:
                            # add use stmt and public stmt if required
                            if isinstance(res_stmt, Use) and res_stmt.isonly:
                                useobj = res_stmt.genkpair.parent.create_use()
                                useobj.set_attr('name', res_stmt.name)
                                useobj.append_attr('items', vname)

                                pubobj = res_stmt.genkpair.parent.create_public()
                                pubobj.append_attr('items', vname)
                                break
                        break
                assert vname, 'Can not get verfiy name for %s'%var.name

                # initialize dtype check status
                callvobj = doobjs[-1].create_callstmt()
                callvobj.set_attr('name', vname)
                indexes = ','.join([ 'idx%d'%(r+1) for r in range(var.rank)])
                callvobj.set_attr('args', ['"%s"'%var.name, 'dtype_check_status', 'var(%s)'%indexes, 'ref_var(%s)'%indexes])

                for doobj in reversed(doobjs):
                    endobj = doobj.create_endobj()
                    endobj.set_attr('blockname', 'DO')

                topobj.add_line(topobj.insert_in_exe_part)

                # create if stmt for dtype check result
                ifthenobj = topobj.create_ifthen()
                ifthenobj.set_attr('expr', 'dtype_check_status%numTotal == dtype_check_status%numIdentical')

                # increment identical check count
                incobj = ifthenobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numIdentical')
                incobj.set_attr('rhs', 'check_status%numIdentical + 1')

                # create else if stmt
                elifobj = ifthenobj.create_elseif()
                elifobj.set_attr('expr', 'dtype_check_status%numOutTol > 0')

                # increment fatal check count
                incobj = ifthenobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numOutTol')
                incobj.set_attr('rhs', 'check_status%numOutTol + 1')

                # create else if stmt
                elifobj = ifthenobj.create_elseif()
                elifobj.set_attr('expr', 'dtype_check_status%numInTol > 0')

                # increment warning check count
                incobj = ifthenobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numInTol')
                incobj.set_attr('rhs', 'check_status%numInTol + 1')

                # create end subroutine
                endifobj = ifthenobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

                topobj.add_line(topobj.insert_in_exe_part)

            else:
                # check
                ifcmpobj = topobj.create_ifthen()
                if self.stmt.name=='logical':
                    ifcmpobj.set_attr('expr', 'ALL(var .EQV. ref_var)')
                else:
                    ifcmpobj.set_attr('expr', 'ALL(var == ref_var)')

                identobj = ifcmpobj.create_assignstmt()
                identobj.set_attr('lhs', 'check_result')
                identobj.set_attr('rhs', 'CHECK_IDENTICAL')

                # increment fatal check count
                incobj = ifcmpobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numIdentical')
                incobj.set_attr('rhs', 'check_status%numIdentical + 1')

                writeobj = ifcmpobj.create_write()
                writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is IDENTICAL."'])

                # check else
                ifcmpobj.create_else()

                if self.stmt.is_numeric():

                    alloc1obj = ifcmpobj.create_allocate()
                    alloc1obj.set_attr('alloc_list', ['temp(%s)'%get_size])

                    alloc2obj = ifcmpobj.create_allocate()
                    alloc2obj.set_attr('alloc_list', ['temp2(%s)'%get_size])

                    countobj = ifcmpobj.create_assignstmt()
                    countobj.set_attr('lhs', 'n')
                    countobj.set_attr('rhs', 'count(var /= ref_var)')

                    whereobj = ifcmpobj.create_where()
                    whereobj.set_attr('expr', 'abs(ref_var) > check_status%minvalue')

                    tempobj = whereobj.create_assignstmt()
                    tempobj.set_attr('lhs', 'temp')
                    tempobj.set_attr('rhs', '((var-ref_var)/ref_var)**2')

                    temp2obj = whereobj.create_assignstmt()
                    temp2obj.set_attr('lhs', 'temp2')
                    temp2obj.set_attr('rhs', '(var-ref_var)**2')

                    whereobj.create_elsewhere()

                    etempobj = whereobj.create_assignstmt()
                    etempobj.set_attr('lhs', 'temp')
                    etempobj.set_attr('rhs', '(var-ref_var)**2')

                    etemp2obj = whereobj.create_assignstmt()
                    etemp2obj.set_attr('lhs', 'temp2')
                    etemp2obj.set_attr('rhs', 'temp')

                    endwhereobj = whereobj.create_endobj()
                    endwhereobj.set_attr('blockname', 'WHERE')

                    nrmsobj = ifcmpobj.create_assignstmt()
                    nrmsobj.set_attr('lhs', 'nrmsdiff')
                    nrmsobj.set_attr('rhs', 'sqrt(sum(temp)/real(n))')

                    rmsobj = ifcmpobj.create_assignstmt()
                    rmsobj.set_attr('lhs', 'rmsdiff')
                    rmsobj.set_attr('rhs', 'sqrt(sum(temp2)/real(n))')

                    ifv1obj = ifcmpobj.create_ifthen()
                    ifv1obj.set_attr('expr', 'nrmsdiff > check_status%tolerance')

                    ifotobj = ifv1obj.create_assignstmt()
                    ifotobj.set_attr('lhs', 'check_result')
                    ifotobj.set_attr('rhs', 'CHECK_OUT_TOL')

                    # increment fatal check count
                    incobj = ifv1obj.create_assignstmt()
                    incobj.set_attr('lhs', 'check_status%numOutTol')
                    incobj.set_attr('rhs', 'check_status%numOutTol + 1')

                    writeobj = ifv1obj.create_write()
                    writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL out of tolerance."'])

                    ifv1obj.create_else()

                    ifotobj = ifv1obj.create_assignstmt()
                    ifotobj.set_attr('lhs', 'check_result')
                    ifotobj.set_attr('rhs', 'CHECK_IN_TOL')

                    # increment fatal check count
                    incobj = ifv1obj.create_assignstmt()
                    incobj.set_attr('lhs', 'check_status%numInTol')
                    incobj.set_attr('rhs', 'check_status%numInTol + 1')

                    writeobj = ifv1obj.create_write()
                    writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL within tolerance."'])

                    ifendv1obj = ifv1obj.create_endobj()
                    ifendv1obj.set_attr('blockname', 'IF')

                else:
                    countobj = ifcmpobj.create_assignstmt()
                    countobj.set_attr('lhs', 'n')
                    countobj.set_attr('rhs', 'count(var /= ref_var)')

                    ifotobj = ifcompobj.create_assignstmt()
                    ifotobj.set_attr('lhs', 'check_result')
                    ifotobj.set_attr('rhs', 'CHECK_OUT_TOL')

                    # increment fatal check count
                    incobj = ifcmpobj.create_assignstmt()
                    incobj.set_attr('lhs', 'check_status%numOutTol')
                    incobj.set_attr('rhs', 'check_status%numOutTol + 1')

                    writeobj = ifcmpobj.create_write()
                    writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL."'])


                # check end if
                endobj = ifcmpobj.create_endobj()
                endobj.set_attr('blockname', 'IF')

        else: # scalar
            if not self.stmt.is_derived():

                # check
                ifcmpobj = topobj.create_ifthen()
                if self.stmt.name=='logical':
                    ifcmpobj.set_attr('expr', 'var .EQV. ref_var')
                else:
                    ifcmpobj.set_attr('expr', 'var == ref_var')

                identobj = ifcmpobj.create_assignstmt()
                identobj.set_attr('lhs', 'check_result')
                identobj.set_attr('rhs', 'CHECK_IDENTICAL')

                # increment fatal check count
                incobj = ifcmpobj.create_assignstmt()
                incobj.set_attr('lhs', 'check_status%numIdentical')
                incobj.set_attr('rhs', 'check_status%numIdentical + 1')

                writeobj = ifcmpobj.create_write()
                writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is IDENTICAL."'])

                # check else
                ifcmpobj.create_else()

                if self.stmt.is_numeric():
                    diffobj = ifcmpobj.create_assignstmt()
                    diffobj.set_attr('lhs', 'diff')
                    diffobj.set_attr('rhs', 'ABS(var - ref_var)')

                    iftolobj = ifcmpobj.create_ifthen()
                    iftolobj.set_attr('expr', 'diff < check_status%tolerance')

                    itobj = iftolobj.create_assignstmt()
                    itobj.set_attr('lhs', 'check_result')
                    itobj.set_attr('rhs', 'CHECK_IN_TOL')

                    writeobj = iftolobj.create_write()
                    writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL within tolerance."'])

                    # increment fatal check count
                    incobj = iftolobj.create_assignstmt()
                    incobj.set_attr('lhs', 'check_status%numInTol')
                    incobj.set_attr('rhs', 'check_status%numInTol + 1')

                    iftolobj.create_else()

                    otobj = iftolobj.create_assignstmt()
                    otobj.set_attr('lhs', 'check_result')
                    otobj.set_attr('rhs', 'CHECK_OUT_TOL')

                    # increment fatal check count
                    incobj = iftolobj.create_assignstmt()
                    incobj.set_attr('lhs', 'check_status%numOutTol')
                    incobj.set_attr('rhs', 'check_status%numOutTol + 1')

                    writeobj = iftolobj.create_write()
                    writeobj.set_attr('item_list', ['trim(adjustl(varname))','" is NOT IDENTICAL out of tolerance."'])

                    endobj = iftolobj.create_endobj()
                    endobj.set_attr('blockname', 'IF')
                else:
                    otobj = ifcmpobj.create_assignstmt()
                    otobj.set_attr('lhs', 'check_result')
                    otobj.set_attr('rhs', 'CHECK_OUT_TOL')

                # check end if
                endobj = ifcmpobj.create_endobj()
                endobj.set_attr('blockname', 'IF')


    def gen_verify_subrs_print(self, subrobj, topobj, var):

        def print_numarr_detail(parent):
            obj = parent.create_write()
            obj.set_attr('item_list', ['count( var /= ref_var)', '" of "', 'size( var )', '" elements are different."'])

            obj = parent.create_write()
            obj.set_attr('item_list', ['"Average - kernel "', 'sum(var)/real(size(var))'])

            obj = parent.create_write()
            obj.set_attr('item_list', ['"Average - reference "', 'sum(ref_var)/real(size(ref_var))'])

            obj = parent.create_write()
            obj.set_attr('item_list', ['"RMS of difference is "', 'rmsdiff'])

            obj = parent.create_write()
            obj.set_attr('item_list', ['"Normalized RMS of difference is "', 'nrmsdiff'])

        def print_num_detail(parent):
            obj = parent.create_write()
            obj.set_attr('item_list', ['"Difference is "', 'diff'])

        def print_dummy_detail(parent):
            obj = parent.create_write()
            obj.set_attr('item_list', ['"NOT IMPLEMENTED"'])

        print_detail = print_dummy_detail
        if var.is_array(): # array
            if self.stmt.is_derived():
                pass
            else:
                if self.stmt.is_numeric():
                    print_detail = print_numarr_detail
                else:
                    pass
        else:
            if self.stmt.is_derived():
                pass
            else:
                if self.stmt.is_numeric():
                    print_detail = print_num_detail
                else:
                    pass


        ifvobj = topobj.create_ifthen()
        ifvobj.set_attr('expr', 'check_status%verboseLevel > 2')

        incobj = ifvobj.create_assignstmt()
        incobj.set_attr('lhs', 'is_print')
        incobj.set_attr('rhs', '.TRUE.')

        ifelse1obj = ifvobj.create_elseif()
        ifelse1obj.set_attr('expr', 'check_status%verboseLevel == 2')

        ifot2obj = ifvobj.create_ifthen()
        ifot2obj.set_attr('expr', 'check_result /= CHECK_IDENTICAL')

        incobj = ifot2obj.create_assignstmt()
        incobj.set_attr('lhs', 'is_print')
        incobj.set_attr('rhs', '.TRUE.')

        endifot2obj = ifot2obj.create_endobj()
        endifot2obj.set_attr('blockname', 'IF')

        ifelse1obj = ifvobj.create_elseif()
        ifelse1obj.set_attr('expr', 'check_status%verboseLevel == 1')

        ifot3obj = ifvobj.create_ifthen()
        ifot3obj.set_attr('expr', 'check_result == CHECK_OUT_TOL')

        incobj = ifot3obj.create_assignstmt()
        incobj.set_attr('lhs', 'is_print')
        incobj.set_attr('rhs', '.TRUE.')

        endifot3obj = ifot3obj.create_endobj()
        endifot3obj.set_attr('blockname', 'IF')

        ifelse1obj = ifvobj.create_elseif()
        ifelse1obj.set_attr('expr', 'check_status%verboseLevel < 1')

        incobj = ifvobj.create_assignstmt()
        incobj.set_attr('lhs', 'is_print')
        incobj.set_attr('rhs', '.FALSE.')

        endifvobj = ifvobj.create_endobj()
        endifvobj.set_attr('blockname', 'IF')

        topobj.add_line(topobj.insert_in_exe_part)

        ifprintobj = topobj.create_ifthen()
        ifprintobj.set_attr('expr', 'is_print')

        print_detail(ifprintobj)

        endifprintobj = ifprintobj.create_endobj()
        endifprintobj.set_attr('blockname', 'IF')

    def gen_verify(self, enames=None):

        if enames is None:
            enames = []
            for entity in self.stmt.entity_decls:
                enames.append(get_entity_name(entity))

        vnames = self.get_verifynames(enames)
        for ename, vname in zip(enames, vnames):

            var = self.stmt.get_variable(ename)

            # create calls and/or extern subroutine
            if isinstance(self.parent, GenK_SubroutineP):

                # create a call stmt for this var
                vlcallobj = self.parent.create_callstmt(insert_in=self.parent.insert_in_verify_local_state)
                vlcallobj.set_attr('name', vname)
                vlcallobj.set_attr('args', ['"%s"'%ename, 'check_status', ename, 'ref_%s'%ename])

                target = self.parent

            elif isinstance(self.parent, Gen_Module):
                extsubrname = 'kv_externs_%s'%self.parent.name
                if not self.parent.has_name(extsubrname, GenK_Subroutine):
                    # add kv_externs subroutine
                    extsubrobj = self.parent.create_subroutine()
                    extsubrobj.set_attr('name', extsubrname)
                    extsubrobj.set_attr('args', ['check_status'])
                    self.parent.extvsubr = extsubrobj

                    # check_status
                    checkobj = extsubrobj.create_typedeclstmt()
                    checkobj.set_attr('typespec', 'TYPE')
                    checkobj.set_attr('selector', 'check_t')
                    checkobj.append_attr('attr_specs', 'INTENT(INOUT)')
                    checkobj.append_attr('entity_decls', 'check_status')


                    endsubrobj = extsubrobj.create_endobj()
                    endsubrobj.set_attr('name', extsubrname)
                    endsubrobj.set_attr('blockname', 'SUBROUTINE')

                    # add public
                    pubobj = self.parent.create_public()
                    pubobj.append_attr('items', extsubrname)

                    pblock = State.parentblock['stmt'].genkpair

                    # add use in parentblock
                    useobj = pblock.create_use()
                    useobj.set_attr('name', self.parent.name)
                    useobj.append_attr('items', extsubrname)

                    # add call in verfiy extern block
                    callobj = pblock.create_callstmt(insert_in=pblock.insert_in_verify_module_state)
                    callobj.set_attr('name', extsubrname)
                    callobj.set_attr('args', ['check_status'])

                # create a call stmt for this var in kv_extern_<module name> subroutine
                vlcallobj = self.parent.extvsubr.create_callstmt()
                vlcallobj.set_attr('name', vname)
                vlcallobj.set_attr('args', ['"%s"'%ename, 'check_status', ename, 'ref_%s'%ename])

                target = self.parent
            elif isinstance(self.parent, Gen_TypeDecl):
                target = self.parent.parent
            else: raise ProgramException('Unknown class: %s'%self.parent.__class__)

            if not target.has_name(vname, GenK_Subroutine) and \
                (var.is_array() or not self.stmt.is_derived()):

                target.add_line(target.insert_in_subprograms)

                # subroutine stmt
                subrobj = target.create_subroutine()
                subrobj.set_attr('name', vname)
                subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

                #### spec. part ####
                self.gen_verify_subrs_spec(subrobj, var)
                subrobj.add_line(subrobj.insert_in_decl_construct)

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

                #### exec. part - check ####
                self.gen_verify_subrs_check(subrobj, topobj, var)
                subrobj.add_line(subrobj.insert_in_exe_part)

                #### exec. part - print ####
                self.gen_verify_subrs_print(subrobj, topobj, var)

                if ifassocobj:
                    endobj = ifassocobj.create_endobj()
                    endobj.set_attr('blockname', 'IF')

                if ifallocobj:
                    endobj = ifallocobj.create_endobj()
                    endobj.set_attr('blockname', 'IF')

                # end subroutine stmt
                endsubrobj = subrobj.create_endobj()
                endsubrobj.set_attr('name', vname)
                endsubrobj.set_attr('blockname', 'SUBROUTINE')

    def create_read_subr(self, krname, var):
        from statements import Use

        if isinstance(self.parent, Gen_Module) or isinstance(self.parent, Gen_SubroutineP):
            if self.parent.has_name(krname, Gen_Subroutine): return
            target = self.parent
        elif isinstance(self.parent, Gen_TypeDecl):
            if self.parent.parent.has_name(krname, Gen_Subroutine): return
            target = self.parent.parent
        else: raise ProgramException('Unknown  class: %s'%self.parent.__class__)

        target.add_line(target.insert_in_subprograms)
        target.add_comment('reading typedecl variable', target.insert_in_subprograms)

        subrobj = target.create_subroutine()
        subrobj.set_attr('name', krname)
        subrobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])

        # variable
        varobj = subrobj.create_typedeclstmt(stmt=self.stmt)
        for attrspec in self.stmt.attrspec:
            if attrspec in ['allocatable', 'pointer']:
                varobj.append_attr('attr_specs', attrspec.upper())
        if var.is_array():
            varobj.append_attr('attr_specs', 'DIMENSION(%s)'% ','.join(':'*var.rank))
        varobj.append_attr('attr_specs', 'INTENT(OUT)')
        varobj.set_attr('entity_decls', ['var'])

        # kgen_unit
        unitobj = subrobj.create_typedeclstmt()
        unitobj.set_attr('typespec', 'INTEGER')
        unitobj.append_attr('attr_specs', 'INTENT(IN)')
        unitobj.append_attr('entity_decls', 'kgen_unit')

        # printvar
        printobj = subrobj.create_typedeclstmt()
        printobj.set_attr('typespec', 'CHARACTER')
        printobj.set_attr('selector', '*')
        printobj.extend_attr('attr_specs', ['INTENT(IN)', 'OPTIONAL'])
        printobj.append_attr('entity_decls', 'printvar')

        # is_true
        istrueobj = subrobj.create_typedeclstmt()
        istrueobj.set_attr('typespec', 'LOGICAL')
        istrueobj.append_attr('entity_decls', 'is_true')

        # idx
        if var.is_array():
            indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
            idxobj = subrobj.create_typedeclstmt()
            idxobj.set_attr('typespec', 'INTEGER')
            idxobj.append_attr('entity_decls', ','.join(indexes))

        # bound
        bndobj = subrobj.create_typedeclstmt()
        bndobj.set_attr('typespec', 'INTEGER')
        bndobj.append_attr('attr_specs', 'DIMENSION(2,%d)'%abs(var.rank))
        bndobj.append_attr('entity_decls', 'kgen_bound')

        subrobj.add_line(subrobj.insert_in_exe_part)

        robj = subrobj.create_read()
        robj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
        robj.set_attr('item_list', ['is_true'])

        subrobj.add_line(subrobj.insert_in_exe_part)

        ifistrueobj = subrobj.create_ifthen()
        ifistrueobj.set_attr('expr', 'is_true')

        if var.is_array():
            shape = []
            for dim in range(var.rank):
                rbnd1obj = ifistrueobj.create_read()
                rbnd1obj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                rbnd1obj.set_attr('item_list', ['kgen_bound(1, %d)'%(dim+1)])

                rbnd2obj = ifistrueobj.create_read()
                rbnd2obj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                rbnd2obj.set_attr('item_list', ['kgen_bound(2, %d)'%(dim+1)])

                shape.append('kgen_bound(2, %d) - kgen_bound(1, %d) + 1'%(dim+1, dim+1))

            if var.is_allocatable() or var.is_pointer():
                allocobj = ifistrueobj.create_allocate()
                allocobj.set_attr('alloc_list', ['var(%s)'%','.join(shape)])

            if self.stmt.is_derived():

                indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
                str_indexes = ','.join(indexes)

                pobj = ifistrueobj
                doobjs = []
                for d in range(var.rank):
                    doobj = pobj.create_do()
                    doobj.set_attr('loop_control', 'idx%(d)d=kgen_bound(1,%(d)d), kgen_bound(2, %(d)d)'%{'d':d+1})
                    doobjs.append(doobj)
                    pobj = doobj

                ifprintobj = doobjs[-1].create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')
                rname = None
                for uname, req in self.stmt.unknowns.iteritems():
                    if uname.firstpartname()==self.stmt.name:
                        rname = 'kr_%s'%self.get_dtype_subpname(req.res_stmts[0])
                        for res_stmt in req.res_stmts:
                            # add use stmt and public stmt if required
                            if isinstance(res_stmt, Use) and res_stmt.isonly:
                                useobj = res_stmt.genkpair.parent.create_use()
                                useobj.set_attr('name', res_stmt.name)
                                useobj.append_attr('items', rname)

                                pubobj = res_stmt.genkpair.parent.create_public()
                                pubobj.append_attr('items', rname)
                                break
                        break
                assert rname, 'Can not find rname'

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', rname)
                callobj.set_attr('args', ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes])

                ifprintobj.create_else()

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', rname)
                callobj.set_attr('args', ['var(%s)'%str_indexes, 'kgen_unit'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')


                for doobj in reversed(doobjs):
                    endobj = doobj.create_endobj()
                    endobj.set_attr('blockname', 'DO')

            else: # intrinsic type
                rvarobj = ifistrueobj.create_read()
                rvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                rvarobj.set_attr('item_list', ['var'])

                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')

                wprintobj = ifprintobj.create_write()
                wprintobj.set_attr('item_list', ['"** KGEN DEBUG: " // printvar // " **"', 'var'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

        else: # scalar
            if self.stmt.is_derived():
                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')
                rname = None
                for uname, req in self.stmt.unknowns.iteritems():
                    if uname.firstpartname()==self.stmt.name:
                        rname = 'kr_%s'%self.get_dtype_subpname(req.res_stmts[0])
                        for res_stmt in req.res_stmts:
                            # add use stmt and public stmt if required
                            if isinstance(res_stmt, Use) and res_stmt.isonly:
                                useobj = res_stmt.genkpair.parent.create_use()
                                useobj.set_attr('name', res_stmt.name)
                                useobj.append_attr('items', rname)

                                pubobj = res_stmt.genkpair.parent.create_public()
                                pubobj.append_attr('items', rname)
                                break
                        break
                assert rname, 'Can not find rname'

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', rname)
                callobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])

                ifprintobj.create_else()

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', rname)
                callobj.set_attr('args', ['var', 'kgen_unit'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

            else: # intrinsic type
                rvarobj = ifistrueobj.create_read()
                rvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                rvarobj.set_attr('item_list', ['var'])

                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')

                wprintobj = ifprintobj.create_write()
                wprintobj.set_attr('item_list', ['"** KGEN DEBUG: " // printvar // " **"', 'var'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

        endifiistrueobj = ifistrueobj.create_endobj()
        endifiistrueobj.set_attr('blockname', 'IF')

        endsubrobj = subrobj.create_endobj()
        endsubrobj.set_attr('name', krname)
        endsubrobj.set_attr('blockname', 'SUBROUTINE')

        return subrobj

    def create_call4readsubr(self, is_outtype, subrname, varname):

        callobj = None
        if isinstance(self.parent, Gen_Module):
            if is_outtype:
                callobj = self.parent.extroutsubr.create_callstmt()
            else:
                callobj = self.parent.extrinsubr.create_callstmt()
        elif isinstance(self.parent, Gen_SubroutineP):
            if is_outtype:
                callobj = self.parent.create_callstmt(insert_in=self.parent.insert_in_output_local_state)
            else:
                callobj = self.parent.create_callstmt(insert_in=self.parent.insert_in_input_local_state)
        elif isinstance(self.parent, Gen_TypeDecl):
            rvarname = 'var%%%s'%varname

            ifobj = self.parent.parent.rtypesubr.create_ifthen()
            ifobj.set_attr('expr', 'PRESENT(printvar)')

            call1obj = ifobj.create_callstmt()
            call1obj.set_attr('name', subrname)
            call1obj.set_attr('args', [rvarname, 'kgen_unit', 'printvar // "%%%s"'%varname])

            elseobj = ifobj.create_else()

            call2obj = ifobj.create_callstmt()
            call2obj.set_attr('name', subrname)
            call2obj.set_attr('args', [rvarname, 'kgen_unit'])

            endifobj = ifobj.create_endobj()
            endifobj.set_attr('blockname', 'IF')
        else: raise ProgramException('Unknown class: %s'%self.parent.__class__)

        if callobj:
            callobj.set_attr('name', subrname)
            if any(match_namepath(pattern, pack_exnamepath(self.stmt, pattern), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    callobj.set_attr('args', [varname, 'kgen_unit', '"ref_%s"'%varname])
                else:
                    callobj.set_attr('args', [varname, 'kgen_unit', '"%s"'%varname])
            else:
                callobj.set_attr('args', [varname, 'kgen_unit'])

    def create_read_stmt(self, is_outtype, varname):

        rvarobj = None    
        if isinstance(self.parent, Gen_Module):
            if is_outtype:
                rvarobj = self.parent.extroutsubr.create_read()
            else:
                rvarobj = self.parent.extrinsubr.create_read()

            rvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            rvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    rprintobj = self.parent.create_write(insert_in=self.parent.extroutsubr.insert_in_exe_part)
                    rprintobj.set_attr('item_list', ['"** KGEN DEBUG: ref_%s **"'%varname, varname])
                else:
                    rprintobj = self.parent.create_write(insert_in=self.parent.extrinsubr.insert_in_exe_part)
                    rprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        elif isinstance(self.parent, Gen_SubroutineP):
            if is_outtype:
                rvarobj = self.parent.create_read(insert_in=self.parent.insert_in_output_local_state)
            else:
                rvarobj = self.parent.create_read(insert_in=self.parent.insert_in_input_local_state)

            rvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            rvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    rprintobj = self.parent.create_write(insert_in=self.parent.insert_in_output_local_state)
                    rprintobj.set_attr('item_list', ['"** KGEN DEBUG: ref_%s **"'%varname, varname])
                else:
                    rprintobj = self.parent.create_write(insert_in=self.parent.insert_in_input_local_state)
                    rprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        elif isinstance(self.parent, Gen_TypeDecl):
            varname = 'var%%%s'%varname

            rvarobj = self.parent.parent.rtypesubr.create_read()
            rvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            rvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                rprintobj = self.parent.parent.rtypesubr.create_write()
                rprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        else: raise ProgramException('Unknown class: %s'%self.parent.__class__)

    def gen_read_in_or_out(self, name, is_outtype):

        var = self.stmt.get_variable(name)

        subrnames = self.get_readnames([name])
        assert len(subrnames)==1, 'More than one subroutine name'%subrnames
        subrname = subrnames[0]
        varname = name

        if var.is_array():
            if self.stmt.is_derived():
                # create a read subroutine
                self.create_call4readsubr(is_outtype, subrname, varname)
                subrobj = self.create_read_subr(subrname, var)

            else: # intrinsic type
                if var.is_explicit_shape_array():
                    # create a read stmt
                    self.create_read_stmt(is_outtype, varname)
                else:
                    # create a read subroutine
                    self.create_call4readsubr(is_outtype, subrname, varname)
                    subrobj = self.create_read_subr(subrname, var)

        else: # scalar
            if self.stmt.is_derived():
                if var.is_pointer():
                    # create a read subroutine
                    self.create_call4readsubr(is_outtype, subrname, varname)
                    self.create_read_subr(subrname, var)
                else:
                    # create a call stmt
                    self.create_call4readsubr(is_outtype, subrname, varname)
            else:
                if var.is_pointer():
                    # create a read subroutine
                    self.create_call4readsubr(is_outtype, subrname, varname)
                    self.create_read_subr(subrname, var)
                else:
                    # create a read stmt
                    self.create_read_stmt(is_outtype, varname)

    def gen_read(self, in_names, out_names):
        if in_names:
            for in_name in in_names:
                self.gen_read_in_or_out(in_name, False)

        if out_names:
            for out_name in out_names:
                self.gen_read_in_or_out(out_name, True)

    def get_readnames(self, names=None):
        subpnames = self.get_typedecl_subpname(self.stmt, names)
        if subpnames: return [ 'kr_%s'%sname for sname in subpnames ]
        else: return []

    def get_verifynames(self, names=None):
        subpnames = self.get_typedecl_subpname(self.stmt, names)
        if subpnames: return [ 'kv_%s'%sname for sname in subpnames ]
        else: return []

class GenS_TypeDeclarationStatement(GenS_Statement, Gen_TypeDeclarationStatement):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

    def process(self):
        if self.isvalid:
            if self.stmt and hasattr(self.stmt, 'geninfo') and KGGenType.has_state(self.stmt.geninfo) and \
                not 'parameter' in self.stmt.attrspec:
                if isinstance(self.parent, GenS_SubroutineP) or isinstance(self.parent, Gen_Module):
                    #  select IN entities in the typedecl stmt.
                    items = []; decls = []
                    for (uname, req) in KGGenType.get_state(self.stmt.geninfo):
                        iname = uname.firstpartname()
                        ename = [ e for e in self.stmt.entity_decls if e. startswith(iname) ]
                        assert len(ename)==1
                        ename = ename[0]
                        if not iname in items:
                            items.append(iname)
                            decls.append(ename)

                    #  select OUT entities in the typedecl stmt.
                    out_items = []; out_decls = []
                    for (uname, req) in KGGenType.get_state_out(self.stmt.geninfo):
                        iname = uname.firstpartname()
                        ename = [ e for e in self.stmt.entity_decls if e. startswith(iname) ]
                        assert len(ename)==1
                        ename = ename[0]
                        if not iname in out_items:
                            out_items.append(iname)
                            out_decls.append('ref_%s'%ename)

                    # create write
                    if isinstance(self.parent, GenS_SubroutineP):
                        self.gen_write(items, out_items)
                    elif isinstance(self.parent, Gen_Module):
                        extinsubrname = 'kw_externs_in_%s'%self.parent.name
                        if len(items)>0 and not self.parent.has_name(extinsubrname, GenS_Subroutine):
                            self.stmt.top.used4genstate = True

                            # add kw_externs subroutine
                            self.parent.add_line(self.parent.insert_in_subprograms)
                            self.parent.add_comment('writing extern input variable', self.parent.insert_in_subprograms)

                            extsubrobj = self.parent.create_subroutine()
                            extsubrobj.set_attr('name', extinsubrname)
                            extsubrobj.set_attr('args', ['kgen_unit'])
                            self.parent.extwinsubr = extsubrobj

                            # kgen_unit
                            unitobj = extsubrobj.create_typedeclstmt()
                            unitobj.set_attr('typespec', 'INTEGER')
                            unitobj.append_attr('attr_specs', 'INTENT(IN)')
                            unitobj.append_attr('entity_decls', 'kgen_unit')

                            endsubrobj = extsubrobj.create_endobj()
                            endsubrobj.set_attr('name', extinsubrname)
                            endsubrobj.set_attr('blockname', 'SUBROUTINE')

                            # add public
                            pubobj = self.parent.create_public()
                            pubobj.append_attr('items', extinsubrname)

                            pblock = State.parentblock['stmt'].genspair

                            # add use in parentblock
                            useobj = pblock.create_use()
                            useobj.set_attr('name', self.parent.name)
                            useobj.append_attr('items', extinsubrname)

                            # add call in write extern block
                            callobj = pblock.create_callstmt(insert_in=pblock.insert_in_input_module_state)
                            callobj.set_attr('name', extinsubrname)
                            callobj.set_attr('args', ['kgen_unit'])

                        extoutsubrname = 'kw_externs_out_%s'%self.parent.name
                        if len(out_items)>0 and not self.parent.has_name(extoutsubrname, GenS_Subroutine):
                            self.stmt.top.used4genstate = True

                            # add kw_externs subroutine
                            self.parent.add_line(self.parent.insert_in_subprograms)
                            self.parent.add_comment('writing extern output variable', self.parent.insert_in_subprograms)

                            extsubrobj = self.parent.create_subroutine()
                            extsubrobj.set_attr('name', extoutsubrname)
                            extsubrobj.set_attr('args', ['kgen_unit'])
                            self.parent.extwoutsubr = extsubrobj

                            # kgen_unit
                            unitobj = extsubrobj.create_typedeclstmt()
                            unitobj.set_attr('typespec', 'INTEGER')
                            unitobj.append_attr('attr_specs', 'INTENT(IN)')
                            unitobj.append_attr('entity_decls', 'kgen_unit')


                            endsubrobj = extsubrobj.create_endobj()
                            endsubrobj.set_attr('name', extoutsubrname)
                            endsubrobj.set_attr('blockname', 'SUBROUTINE')

                            # add public
                            pubobj = self.parent.create_public()
                            pubobj.append_attr('items', extoutsubrname)

                            pblock = State.parentblock['stmt'].genspair

                            # add use in parentblock
                            useobj = pblock.create_use()
                            useobj.set_attr('name', self.parent.name)
                            useobj.append_attr('items', extoutsubrname)

                            # add call in write extern block
                            callobj = pblock.create_callstmt(insert_in=pblock.insert_in_output_module_state)
                            callobj.set_attr('name', extoutsubrname)
                            callobj.set_attr('args', ['kgen_unit'])

                        self.gen_write(items, out_items)

    def create_write_subr(self, kwname, var):
        from statements import Use

        if isinstance(self.parent, Gen_Module) or isinstance(self.parent, Gen_SubroutineP):
            if self.parent.has_name(kwname, Gen_Subroutine): return
            target = self.parent
        elif isinstance(self.parent, Gen_TypeDecl):
            if self.parent.parent.has_name(kwname, Gen_Subroutine): return
            target = self.parent.parent
        else: raise ProgramException('Unknown  class: %s'%self.parent.__class__)

        target.add_line(target.insert_in_subprograms)
        target.add_comment('writing typedecl variable', target.insert_in_subprograms)

        subrobj = target.create_subroutine()
        subrobj.set_attr('name', kwname)
        subrobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])

        # variable
        varobj = subrobj.create_typedeclstmt(stmt=self.stmt)
        for attrspec in self.stmt.attrspec:
            if attrspec in ['pointer']:
                varobj.append_attr('attr_specs', attrspec.upper())
        if var.is_array():
            varobj.append_attr('attr_specs', 'DIMENSION(%s)'% ','.join(':'*var.rank))
        varobj.append_attr('attr_specs', 'INTENT(IN)')
        varobj.set_attr('entity_decls', ['var'])

        # kgen_unit
        unitobj = subrobj.create_typedeclstmt()
        unitobj.set_attr('typespec', 'INTEGER')
        unitobj.append_attr('attr_specs', 'INTENT(IN)')
        unitobj.append_attr('entity_decls', 'kgen_unit')

        # printvar
        printobj = subrobj.create_typedeclstmt()
        printobj.set_attr('typespec', 'CHARACTER')
        printobj.set_attr('selector', '*')
        printobj.extend_attr('attr_specs', ['INTENT(IN)', 'OPTIONAL'])
        printobj.append_attr('entity_decls', 'printvar')

        # is_true
        istrueobj = subrobj.create_typedeclstmt()
        istrueobj.set_attr('typespec', 'LOGICAL')
        istrueobj.append_attr('entity_decls', 'is_true')

        # idx
        if var.is_array():
            indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
            idxobj = subrobj.create_typedeclstmt()
            idxobj.set_attr('typespec', 'INTEGER')
            idxobj.append_attr('entity_decls', ','.join(indexes))

        subrobj.add_line(subrobj.insert_in_exe_part)

        if var.is_pointer():
            ifchkobj = subrobj.create_ifthen()
            ifchkobj.set_attr('expr', '.NOT. ASSOCIATED(var)') 

            istrueobj = ifchkobj.create_assignstmt()
            istrueobj.set_attr('lhs', 'is_true')
            istrueobj.set_attr('rhs', '.FALSE.')

            if var.is_array():
                elifobj = ifchkobj.create_elseif()
                elifobj.set_attr('expr', 'SIZE(var)==1')
        elif var.is_array():
            ifchkobj = subrobj.create_ifthen()
            ifchkobj.set_attr('expr', 'SIZE(var)==1') 
        else: raise ProgramException('Variable is not either pointer or array')

        if var.is_array():
            ifbndobj = ifchkobj.create_ifthen()
            ifbndobj.set_attr('expr', 'UBOUND(var, 1)<LBOUND(var, 1)')

            istrueobj = ifbndobj.create_assignstmt()
            istrueobj.set_attr('lhs', 'is_true')
            istrueobj.set_attr('rhs', '.FALSE.')

            elifobj = ifbndobj.create_elseif()
            elifobj.set_attr('expr', 'UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0')

            istrueobj = ifbndobj.create_assignstmt()
            istrueobj.set_attr('lhs', 'is_true')
            istrueobj.set_attr('rhs', '.FALSE.')

            elseobj = ifbndobj.create_else()

            istrueobj = ifbndobj.create_assignstmt()
            istrueobj.set_attr('lhs', 'is_true')
            istrueobj.set_attr('rhs', '.TRUE.')
            
            endifobj = ifbndobj.create_endobj()
            endifobj.set_attr('blockname', 'IF')

        elseobj = ifchkobj.create_else()

        istrueobj = ifchkobj.create_assignstmt()
        istrueobj.set_attr('lhs', 'is_true')
        istrueobj.set_attr('rhs', '.TRUE.')

        endifobj = ifchkobj.create_endobj()
        endifobj.set_attr('blockname', 'IF')

        subrobj.add_line(subrobj.insert_in_exe_part)

        wobj = subrobj.create_write()
        wobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
        wobj.set_attr('item_list', ['is_true'])

        subrobj.add_line(subrobj.insert_in_exe_part)

        ifistrueobj = subrobj.create_ifthen()
        ifistrueobj.set_attr('expr', 'is_true')


        if var.is_array():
            for dim in range(var.rank):
                wbnd1obj = ifistrueobj.create_write()
                wbnd1obj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                wbnd1obj.set_attr('item_list', ['LBOUND(var, %d)'%(dim+1)])

                wbnd2obj = ifistrueobj.create_write()
                wbnd2obj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                wbnd2obj.set_attr('item_list', ['UBOUND(var, %d)'%(dim+1)])

            if self.stmt.is_derived():

                indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
                str_indexes = ','.join(indexes)

                pobj = ifistrueobj
                doobjs = []
                for d in range(var.rank):
                    doobj = pobj.create_do()
                    doobj.set_attr('loop_control', 'idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1})
                    doobjs.append(doobj)
                    pobj = doobj

                ifprintobj = doobjs[-1].create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')
                wname = None
                for uname, req in self.stmt.unknowns.iteritems():
                    if uname.firstpartname()==self.stmt.name:
                        wname = 'kw_%s'%self.get_dtype_subpname(req.res_stmts[0])
                        for res_stmt in req.res_stmts:
                            # add use stmt and public stmt if required
                            if isinstance(res_stmt, Use) and res_stmt.isonly:
                                res_stmt.top.used4genstate = True
                                useobj = res_stmt.genspair.parent.create_use()
                                useobj.set_attr('name', res_stmt.name)
                                useobj.append_attr('items', wname)

                                pubobj = res_stmt.genspair.parent.create_public()
                                pubobj.append_attr('items', wname)
                                break
                        break
                assert wname, 'Can not find wname'

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', wname)
                callobj.set_attr('args', ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes])

                ifprintobj.create_else()

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', wname)
                callobj.set_attr('args', ['var(%s)'%str_indexes, 'kgen_unit'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')


                for doobj in reversed(doobjs):
                    endobj = doobj.create_endobj()
                    endobj.set_attr('blockname', 'DO')

            else: # intrinsic type
                wvarobj = ifistrueobj.create_write()
                wvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                wvarobj.set_attr('item_list', ['var'])

                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')

                wprintobj = ifprintobj.create_write()
                wprintobj.set_attr('item_list', ['"** KGEN DEBUG: " // printvar // " **"', 'var'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

        else: # scalar
            if self.stmt.is_derived():
                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')
                wname = None
                for uname, req in self.stmt.unknowns.iteritems():
                    if uname.firstpartname()==self.stmt.name:
                        wname = 'kw_%s'%self.get_dtype_subpname(req.res_stmts[0])
                        for res_stmt in req.res_stmts:
                            # add use stmt and public stmt if required
                            if isinstance(res_stmt, Use) and res_stmt.isonly:
                                res_stmt.top.used4genstate = True
                                useobj = res_stmt.genspair.parent.create_use()
                                useobj.set_attr('name', res_stmt.name)
                                useobj.append_attr('items', wname)

                                pubobj = res_stmt.genspair.parent.create_public()
                                pubobj.append_attr('items', wname)
                                break
                        break
                assert wname, 'Can not find wname'

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', wname)
                callobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])

                ifprintobj.create_else()

                # create a call stmt
                callobj = ifprintobj.create_callstmt()
                callobj.set_attr('name', wname)
                callobj.set_attr('args', ['var', 'kgen_unit'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

            else: # intrinsic type
                wvarobj = ifistrueobj.create_write()
                wvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
                wvarobj.set_attr('item_list', ['var'])

                ifprintobj = ifistrueobj.create_ifthen()
                ifprintobj.set_attr('expr', 'PRESENT(printvar)')

                wprintobj = ifprintobj.create_write()
                wprintobj.set_attr('item_list', ['"** KGEN DEBUG: " // printvar // " **"', 'var'])

                endifobj = ifprintobj.create_endobj()
                endifobj.set_attr('blockname', 'IF')

        endifiistrueobj = ifistrueobj.create_endobj()
        endifiistrueobj.set_attr('blockname', 'IF')

        endsubrobj = subrobj.create_endobj()
        endsubrobj.set_attr('name', kwname)
        endsubrobj.set_attr('blockname', 'SUBROUTINE')

        return subrobj

    def create_write_stmt(self, is_outtype, varname):

        wvarobj = None    
        if isinstance(self.parent, Gen_Module):
            if is_outtype:
                wvarobj = self.parent.extwoutsubr.create_write()
            else:
                wvarobj = self.parent.extwinsubr.create_write()

            wvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            wvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    wprintobj = self.parent.create_write(insert_in=self.parent.extwoutsubr.insert_in_exe_part)
                    wprintobj.set_attr('item_list', ['"** KGEN DEBUG: ref_%s **"'%varname, varname])
                else:
                    wprintobj = self.parent.create_write(insert_in=self.parent.extwinsubr.insert_in_exe_part)
                    wprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        elif isinstance(self.parent, Gen_SubroutineP):
            if is_outtype:
                wvarobj = self.parent.create_write(insert_in=self.parent.insert_in_output_local_state)
            else:
                wvarobj = self.parent.create_write(insert_in=self.parent.insert_in_input_local_state)

            wvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            wvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    wprintobj = self.parent.create_write(insert_in=self.parent.insert_in_output_local_state)
                    wprintobj.set_attr('item_list', ['"** KGEN DEBUG: ref_%s **"'%varname, varname])
                else:
                    wprintobj = self.parent.create_write(insert_in=self.parent.insert_in_input_local_state)
                    wprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        elif isinstance(self.parent, Gen_TypeDecl):
            varname = 'var%%%s'%varname

            wvarobj = self.parent.parent.wtypesubr.create_write()
            wvarobj.set_attr('ctrl_list', ['UNIT = kgen_unit'])
            wvarobj.set_attr('item_list', [varname])

            if any(match_namepath(pattern, pack_exnamepath(self.stmt, varname), internal=False) for pattern in Config.debug['printvar']):
                wprintobj = self.parent.parent.wtypesubr.create_write()
                wprintobj.set_attr('item_list', ['"** KGEN DEBUG: %s **"'%varname, varname])

        else: raise ProgramException('Unknown class: %s'%self.parent.__class__)


    def create_call4writesubr(self, is_outtype, subrname, varname):

        callobj = None
        if isinstance(self.parent, Gen_Module):
            if is_outtype:
                callobj = self.parent.extwoutsubr.create_callstmt()
            else:
                callobj = self.parent.extwinsubr.create_callstmt()
        elif isinstance(self.parent, Gen_SubroutineP):
            if is_outtype:
                callobj = self.parent.create_callstmt(insert_in=self.parent.insert_in_output_local_state)
            else:
                callobj = self.parent.create_callstmt(insert_in=self.parent.insert_in_input_local_state)
        elif isinstance(self.parent, Gen_TypeDecl):
            wvarname = 'var%%%s'%varname

            ifobj = self.parent.parent.wtypesubr.create_ifthen()
            ifobj.set_attr('expr', 'PRESENT(printvar)')

            call1obj = ifobj.create_callstmt()
            call1obj.set_attr('name', subrname)
            call1obj.set_attr('args', [wvarname, 'kgen_unit', 'printvar // "%%%s"'%varname])

            elseobj = ifobj.create_else()

            call2obj = ifobj.create_callstmt()
            call2obj.set_attr('name', subrname)
            call2obj.set_attr('args', [wvarname, 'kgen_unit'])

            endifobj = ifobj.create_endobj()
            endifobj.set_attr('blockname', 'IF')
        else: raise ProgramException('Unknown class: %s'%self.parent.__class__)

        if callobj:
            callobj.set_attr('name', subrname)
            if any(match_namepath(pattern, pack_exnamepath(self.stmt, pattern), internal=False) for pattern in Config.debug['printvar']):
                if is_outtype:
                    callobj.set_attr('args', [varname, 'kgen_unit', '"ref_%s"'%varname])
                else:
                    callobj.set_attr('args', [varname, 'kgen_unit', '"%s"'%varname])
            else:
                callobj.set_attr('args', [varname, 'kgen_unit'])

    def gen_write_in_or_out(self, name, is_outtype):

        var = self.stmt.get_variable(name)

        subrnames = self.get_writenames([name])
        assert len(subrnames)==1, 'More than one subroutine name'%subrnames
        subrname = subrnames[0]
        varname = name

        if var.is_array():
            if self.stmt.is_derived():
                # create a write subroutine
                self.create_call4writesubr(is_outtype, subrname, varname)
                subrobj = self.create_write_subr(subrname, var)

            else: # intrinsic type
                if var.is_explicit_shape_array():
                    # create a write stmt
                    self.create_write_stmt(is_outtype, varname)
                else:
                    # create a write subroutine
                    self.create_call4writesubr(is_outtype, subrname, varname)
                    subrobj = self.create_write_subr(subrname, var)

        else: # scalar
            if self.stmt.is_derived():
                if var.is_pointer():
                    # create a write subroutine
                    self.create_call4writesubr(is_outtype, subrname, varname)
                    self.create_write_subr(subrname, var)
                else:
                    # create a call stmt
                    self.create_call4writesubr(is_outtype, subrname, varname)
            else:
                if var.is_pointer():
                    # create a write subroutine
                    self.create_call4writesubr(is_outtype, subrname, varname)
                    self.create_write_subr(subrname, var)
                else:
                    # create a write stmt
                    self.create_write_stmt(is_outtype, varname)

    def gen_write(self, in_names, out_names):
        if in_names:
            for in_name in in_names:
                self.gen_write_in_or_out(in_name, False)

        if out_names:
            for out_name in out_names:
                self.gen_write_in_or_out(out_name, True)

    def get_writenames(self, names=None):
        subpnames = self.get_typedecl_subpname(self.stmt, names)
        if subpnames: return [ 'kw_%s'%sname for sname in subpnames ]
        else: return []

########### Assignment ############
class Gen_Assignment(object):
    def _tokgen(self, **kwargs):
        assert kwargs.has_key('lhs')
        assert kwargs.has_key('rhs')
        return '%s = %s'%(kwargs['lhs'], kwargs['rhs'])

class GenK_Assignment(GenK_Statement, Gen_Assignment):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Assignment(GenS_Statement, Gen_Assignment):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### ElseIf ############
class Gen_ElseIf(object):
    def _tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'ELSE IF( %s ) THEN'%kwargs['expr']

class GenK_ElseIf(GenK_Statement, Gen_ElseIf):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_ElseIf(GenS_Statement, Gen_ElseIf):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Else ############
class Gen_Else(object):
    pass

class GenK_Else(GenK_Statement, Gen_Else):

    def tokgen(self, **kwargs):
        return 'ELSE'

class GenS_Else(GenS_Statement, Gen_Else):

    def tokgen(self, **kwargs):
        return 'ELSE'

########### ElseWhere ############
class Gen_ElseWhere(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('mask_expr'): mask_expr = '( %s )'%kwargs['mask_expr']
        else: mask_expr = ''
        if kwargs.has_key('name'): items = ' ' + kwargs['name']
        else: name = ''

        return 'ELSEWHERE%s%s'%(mask_expr, name)

    pass

class GenK_ElseWhere(GenK_Statement, Gen_ElseWhere):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_ElseWhere(GenS_Statement, Gen_ElseWhere):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Write ############
class Gen_Write(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('ctrl_list'): ctrls = kwargs['ctrl_list']
        else: ctrls = ['*','*']
        if kwargs.has_key('item_list'): items = kwargs['item_list']
        else: items = []

        return 'WRITE(%s) %s'%(','.join(ctrls), ', '.join(items))

class GenK_Write(GenK_Statement, Gen_Write):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Write(GenS_Statement, Gen_Write):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Read ############
class Gen_Read(object):
    def _tokgen(self, **kwargs):
        if kwargs.has_key('ctrl_list'): ctrls = kwargs['ctrl_list']
        else: ctrls = ['*','*']
        if kwargs.has_key('item_list'): items = kwargs['item_list']
        else: items = []

        return 'READ(%s) %s'%(','.join(ctrls), ', '.join(items))

class GenK_Read(GenK_Statement, Gen_Read):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Read(GenS_Statement, Gen_Read):

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### Allocate ############
class Gen_Allocate(object):
    pass

class GenK_Allocate(GenK_Statement, Gen_Allocate):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('alloc_list')

        if kwargs.has_key('type_spec'): type_spec = kwargs['type_spec'] + ' :: '
        else: type_spec = ''
        if kwargs.has_key('alloc_opt_list'): alloc_opt_list = ', ' + ', '.join(kwargs['alloc_opt_list'])
        else: alloc_opt_list = ''

        return 'ALLOCATE(%s%s%s)'%(type_spec, ', '.join(kwargs['alloc_list']), alloc_opt_list)

########### Deallocate ############
class Gen_Deallocate(object):
    pass

class GenK_Deallocate(GenK_Statement, Gen_Deallocate):

    def tokgen(self, **kwargs):
        assert kwargs.has_key('dealloc_list')

        if kwargs.has_key('type_spec'): type_spec = kwargs['type_spec'] + ' :: '
        else: type_spec = ''
        if kwargs.has_key('dealloc_opt_list'): dealloc_opt_list = ', ' + ', '.join(kwargs['dealloc_opt_list'])
        else: dealloc_opt_list = ''

        return 'DEALLOCATE(%s%s%s)'%(type_spec, ', '.join(kwargs['dealloc_list']), dealloc_opt_list)

########### BeginStatement ############
class Gen_BeginStatement(object):
    def _initialize(self, node):
        import inspect

        self.items = []
        self.end_obj = None

        for cls in inspect.getmro(self.__class__):
            if cls.__name__.startswith('Gen_Has_'):
                cls.__init__(self)

        self.gen_subnodes(node)

    def _tostr(self):
        if self.isvalid:
            lines = []
            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_list())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

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

    def insert_in_order(self, item, class_order, end_classes, callsite=None):
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
        elif callsite and item.stmt is callsite[0]:
            insert_in_list(callsite[1], item)
            return class_order[1:]

        classes = []
        matched = False
        for blockcls in class_order:
            if not matched and item.stmt.__class__ in blockcls.classes:
                matched = True
                insert_in_list(blockcls, item)
            if matched:
                classes.append(blockcls)

        if not matched:
            raise ProgramException('Wrong sequence of stmt type: %s'%item.stmt.__class__)

        return classes

    def _create_stmtobj(self, obj, insert_in):
        obj.parent = self
        obj.indent = ''
        insert_in(obj)
        return obj

    def create_endobj(self, stmt=None, insert_in=None):
        assert hasattr(self, 'end_obj')

        exec('obj = Gen%s_EndStatement(stmt, self.k_id)'%self.gentype)
        obj.parent = self
        if insert_in:
            insert_in(obj)
        else:
            self.end_obj = obj
        return obj

    def create_subroutine(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_subprograms')
            insert_in = self.insert_in_subprograms

        exec('obj = Gen%s_Subroutine(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_typedeclstmt(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_decl_construct')
            insert_in = self.insert_in_decl_construct


        exec('obj = Gen%s_TypeDeclarationStatement(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_assignstmt(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Assignment(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_callstmt(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Call(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_ifthen(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_IfThen(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_elseif(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_ElseIf(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_else(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Else(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_use(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_use_stmts')
            insert_in = self.insert_in_use_stmts

        exec('obj = Gen%s_Use(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_public(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_decl_construct')
            insert_in = self.insert_in_decl_construct

        exec('obj = Gen%s_Public(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_write(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Write(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_read(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Read(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_allocate(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Allocate(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_deallocate(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Deallocate(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_where(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Where(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_elsewhere(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_ElseWhere(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_do(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Do(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_open(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Open(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_close(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Close(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_endfile(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Endfile(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_stop(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_exe_part')
            insert_in = self.insert_in_exe_part

        exec('obj = Gen%s_Stop(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def create_implicit(self, stmt=None, insert_in=None):
        if insert_in is None:
            assert hasattr(self, 'insert_in_implicit_part')
            insert_in = self.insert_in_implicit_part

        exec('obj = Gen%s_Implicit(stmt, self.k_id)'%self.gentype)
        return self._create_stmtobj(obj, insert_in)

    def has_name(self, name, cls):
        for func in [getattr(self, m) for m in dir(self) if callable(getattr(self, m)) and m.startswith('hasname_in')]:
            if func(name, cls): return True
        return False

class GenK_BeginStatement(GenK_Statement, Gen_BeginStatement):

    def __init__(self, node, k_id):
        super(GenK_BeginStatement, self).__init__(node, k_id)
        self._initialize(node)

    def process_blockhead(self):
        super(GenK_BeginStatement, self).process()

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_items()

    def tostr_blockhead(self):
        return super(GenK_BeginStatement, self).tostr()

    def tostr(self):
        return self._tostr()

class GenS_BeginStatement(GenS_Statement, Gen_BeginStatement):
    def __init__(self, node, k_id):
        super(GenS_BeginStatement, self).__init__(node, k_id)
        self._initialize(node)

    def process_blockhead(self):
        super(GenS_BeginStatement, self).process()

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_items()

    def tostr_blockhead(self):
        return super(GenS_BeginStatement, self).tostr()

    def tostr(self):
        return self._tostr()

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
        if blockname is None:
            raise ProgramException('No block name is provided.')
        
        name = ''
        if kwargs.has_key('name'):
            name = kwargs['name']

        return 'END %s %s'%(blockname.upper(), name)

########### Module ############
class Gen_Module(object):
    def process_module_items(self):
        from block_statements import EndModule

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, \
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
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        elif len(self.subprograms)>0:
            lines.append(self.indent+'CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        return lines

class GenK_Module(GenK_BeginStatement, Gen_Module, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_module_items()

            use1obj = self.create_use()
            use1obj.set_attr('name', 'kgen_utils_mod')
            use1obj.extend_attr('items', ['check_t', 'kgen_init_check', 'kgen_dp'])
            use2obj = self.create_use()
            use2obj.set_attr('name', 'kgen_utils_mod')
            use2obj.extend_attr('items', ['CHECK_IDENTICAL', 'CHECK_IN_TOL', 'CHECK_OUT_TOL'])
  

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_module())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

class GenS_Module(GenS_BeginStatement, Gen_Module, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

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

########### Program ############
class Gen_Program(object):
    def process_program_items(self):
        from block_statements import EndProgram

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, \
            Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms]
        end_classes = [ EndProgram ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_program(self):
        lines = []
        lines.extend(self.tostr_list(self.use_stmts))
        lines.extend(self.tostr_list(self.import_stmts))
        lines.extend(self.tostr_list(self.implicit_part))
        lines.extend(self.tostr_list(self.decl_construct))
        lines.extend(self.tostr_list(self.exe_part))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        elif len(self.subprograms)>0:
            lines.append(self.indent+'CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        return lines

class GenK_Program(GenK_BeginStatement, Gen_Program, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_program_items()

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)

            lines.extend(self.tostr_program())
            if self.end_obj: lines.append(self.end_obj.tostr())

            return '\n'.join(lines)

    def tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        if name is None: raise ProgramException('No program name is provided in call stmt.')

        return 'PROGRAM %s'%name


class GenS_Program(GenS_BeginStatement, Gen_Program, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
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
            lines.append(self.indent+'CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        return lines

class GenK_SubProgramStatement(GenK_BeginStatement, Gen_SubProgramStatement, Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, \
    Gen_Has_DeclConstruct, Gen_Has_ExecutionPart, Gen_Has_ContainsStmt, Gen_Has_Subprograms):

    def process(self):
        if self.isvalid:
            # process first line
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
    def _tokgen(self, **kwargs):
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


class GenK_Subroutine(GenK_SubProgramStatement, Gen_Subroutine):
    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

class GenS_Subroutine(GenS_SubProgramStatement, Gen_Subroutine):
    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

########### SubroutineP ############
class Gen_SubroutineP(object):
    def _tokgen(self, **kwargs):
        name = None
        if kwargs.has_key('name'):
            name = kwargs['name']
        elif self.stmt:
            name = self.stmt.name
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

 
class GenK_SubroutineP(GenK_SubProgramStatement, Gen_SubroutineP, Gen_Has_InputModuleState, Gen_Has_InputLocalState, \
    Gen_Has_OutputModuleState, Gen_Has_OutputLocalState, Gen_Has_CallsiteStmts, Gen_Has_VerifyModuleState, Gen_Has_VerifyLocalState, Gen_Has_MeasureTiming):

    def insert_parentblock_default(self):

        # kgen_unit
        unitobj = self.create_typedeclstmt()
        unitobj.set_attr('typespec', 'INTEGER')
        unitobj.append_attr('attr_specs', 'INTENT(IN)')
        unitobj.append_attr('entity_decls', 'kgen_unit')

        # total time
        vkcntobj = self.create_typedeclstmt()
        vkcntobj.set_attr('typespec', 'REAL')
        vkcntobj.set_attr('selector', 'KIND=kgen_dp')
        vkcntobj.append_attr('attr_specs', 'INTENT(IN)')
        vkcntobj.append_attr('entity_decls', 'total_time')

        # check_status
        checkobj = self.create_typedeclstmt()
        checkobj.set_attr('typespec', 'TYPE')
        checkobj.set_attr('selector', 'check_t')
        checkobj.append_attr('entity_decls', 'check_status')

        # initialize dtype check status
        callobj = self.create_callstmt(insert_in=self.insert_in_callsite_stmts)
        callobj.parent = self
        callobj.set_attr('name', 'kgen_init_check')
        callobj.set_attr('args', ['check_status'])

    def process_subp_items(self):
        from block_statements import EndSubroutine

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, \
            Gen_Has_CallsiteStmts, Gen_Has_ContainsStmt, Gen_Has_Subprograms]
        end_classes = [ EndSubroutine ]

        self.add_line(self.insert_in_input_module_state);
        self.add_comment('read input module state', self.insert_in_input_module_state)
        self.add_line(self.insert_in_input_local_state);
        self.add_comment('read input local state', self.insert_in_input_local_state)
        self.add_line(self.insert_in_output_module_state);
        self.add_comment('read output module state', self.insert_in_output_module_state)
        self.add_line(self.insert_in_output_local_state);
        self.add_comment('read output local state', self.insert_in_output_local_state)
        self.add_line(self.insert_in_callsite_stmts);
        self.add_comment('callsite statements', self.insert_in_callsite_stmts)
        self.add_line(self.insert_in_verify_module_state);
        self.add_comment('verify module state', self.insert_in_verify_module_state)
        self.add_line(self.insert_in_verify_local_state);
        self.add_comment('verify local state', self.insert_in_verify_local_state)
        self.add_line(self.insert_in_measure_timing);
        self.add_comment('measure timing', self.insert_in_measure_timing)

        self.set_attr('args', ['kgen_unit', 'total_time'])

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_subp(self):

        self.insert_parentblock_default()

        lines = []
        lines.extend(self.tostr_list(self.use_stmts))
        lines.extend(self.tostr_list(self.import_stmts))
        lines.extend(self.tostr_list(self.implicit_part))
        lines.extend(self.tostr_list(self.decl_construct))
        lines.extend(self.tostr_list(self.input_module_state))
        lines.extend(self.tostr_list(self.input_local_state))
        lines.extend(self.tostr_list(self.output_module_state))
        lines.extend(self.tostr_list(self.output_local_state))
        lines.extend(self.tostr_list(self.callsite_stmts))
        lines.extend(self.tostr_list(self.verify_module_state))
        lines.extend(self.tostr_list(self.verify_local_state))
        lines.extend(self.tostr_list(self.measure_timing))
        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        elif len(self.subprograms)>0:
            lines.append(self.indent+'CONTAINS')
        lines.extend(self.tostr_list(self.subprograms))
        return lines


    def tokgen(self, **kwargs):
        self._tokgen(**kwargs)


class GenS_SubroutineP(GenS_SubProgramStatement, Gen_SubroutineP, Gen_Has_InputModuleState, Gen_Has_InputLocalState, \
    Gen_Has_CallsiteStmts, Gen_Has_OutputModuleState, Gen_Has_OutputLocalState, Gen_Has_ExecutionPart1, Gen_Has_ExecutionPart2):

    def process_subp_items(self):
        from block_statements import EndSubroutine

        class_order = [Gen_Has_UseStmts, Gen_Has_ImportStmts, Gen_Has_ImplicitPart, Gen_Has_DeclConstruct, Gen_Has_ExecutionPart1, \
            Gen_Has_ExecutionPart2, Gen_Has_ContainsStmt, Gen_Has_Subprograms]
        end_classes = [ EndSubroutine ]

        self.add_line(self.insert_in_input_module_state);
        self.add_comment('read input module state', self.insert_in_input_module_state)
        self.add_line(self.insert_in_input_local_state);
        self.add_comment('read input local state', self.insert_in_input_local_state)
        self.add_line(self.insert_in_callsite_stmts);
        self.add_comment('run callsite', self.insert_in_callsite_stmts)
        self.add_line(self.insert_in_output_module_state);
        self.add_comment('read output module state', self.insert_in_output_module_state)
        self.add_line(self.insert_in_output_local_state);
        self.add_comment('read output local state', self.insert_in_output_local_state)

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes, callsite=(State.callsite['stmt'], Gen_Has_CallsiteStmts))
            item.process()

    def tostr_subp(self):
        from kgen_extra import kgen_subprograms, kgen_print_counter

        self.insert_writeframe()

        lines = []
        lines.extend(self.tostr_list(self.use_stmts))
        lines.extend(self.tostr_list(self.import_stmts))
        lines.extend(self.tostr_list(self.implicit_part))
        lines.extend(self.tostr_list(self.decl_construct))
        lines.extend(self.tostr_list(self.exe_part1))
        lines.extend(self.tostr_list(self.input_module_state))
        lines.extend(self.tostr_list(self.input_local_state))
        lines.extend(self.tostr_list(self.callsite_stmts))
        lines.extend(self.tostr_list(self.output_module_state))
        lines.extend(self.tostr_list(self.output_local_state))
        lines.extend(self.tostr_list(self.exe_part2))

        if len(self.contains_stmt)>0:
            lines.extend(self.tostr_list(self.contains_stmt))
        else:
            lines.append(self.indent+'CONTAINS')

        indent = self.indent+TAB
        lines.append(indent + kgen_subprograms.replace('\n', '\n'+indent))
        lines.append(indent + kgen_print_counter.replace('\n', '\n'+indent))

        lines.extend(self.tostr_list(self.subprograms))

        return lines

    def tokgen(self, **kwargs):
        self._tokgen(**kwargs)

    def insert_writeframe(self):

        # write spec part

        if Config.mpi['enabled']:
            vmpiobj = self.create_typedeclstmt()
            vmpiobj.set_attr('typespec', 'INTEGER')
            vmpiobj.extend_attr('entity_decls', ['kgen_mpi_rank', 'kgen_mpi_size', 'kgen_cur_rank'])

            vrconvobj = self.create_typedeclstmt()
            vrconvobj.set_attr('typespec', 'CHARACTER')
            vrconvobj.set_attr('selector', 'LEN=16')
            vrconvobj.append_attr('entity_decls', 'kgen_mpi_rank_conv')

            vlrankobj = self.create_typedeclstmt()
            vlrankobj.set_attr('typespec', 'INTEGER')
            vlrankobj.append_attr('attr_specs', 'PARAMETER')
            vlrankobj.append_attr('attr_specs', 'DIMENSION(%s)'%Config.mpi['size'])
            vlrankobj.append_attr('entity_decls', 'kgen_mpi_rank_at = (/ %s /)'%', '.join(Config.mpi['ranks']))

        vkgenobj = self.create_typedeclstmt()
        vkgenobj.set_attr('typespec', 'INTEGER')
        vkgenobj.extend_attr('entity_decls', ['kgen_ierr', 'kgen_unit'])

        vkidxobj = self.create_typedeclstmt()
        vkidxobj.set_attr('typespec', 'INTEGER')
        vkidxobj.append_attr('attr_specs', 'DIMENSION(3,10)')
        vkidxobj.append_attr('entity_decls', 'kgen_indexes')

        vkcntobj = self.create_typedeclstmt()
        vkcntobj.set_attr('typespec', 'INTEGER')
        vkcntobj.append_attr('attr_specs', 'SAVE')
        vkcntobj.append_attr('entity_decls', 'kgen_counter = 1')

        vcconvobj = self.create_typedeclstmt()
        vcconvobj.set_attr('typespec', 'CHARACTER')
        vcconvobj.set_attr('selector', 'LEN=16')
        vcconvobj.append_attr('entity_decls', 'kgen_counter_conv')

        vlcntobj = self.create_typedeclstmt()
        vlcntobj.set_attr('typespec', 'INTEGER')
        vlcntobj.append_attr('attr_specs', 'PARAMETER')
        vlcntobj.append_attr('attr_specs', 'DIMENSION(%s)'%Config.invocation['size'])
        vlcntobj.append_attr('entity_decls', 'kgen_counter_at = (/ %s /)'%', '.join(Config.invocation['numbers']))

        vfpathobj = self.create_typedeclstmt()
        vfpathobj.set_attr('typespec', 'CHARACTER')
        vfpathobj.set_attr('selector', 'LEN=1025')
        vfpathobj.append_attr('entity_decls', 'kgen_filepath')

        self.add_line(self.insert_in_decl_construct)

        # file open head
        self.add_line(self.insert_in_exe_part1)
        self.add_openmp('MASTER', self.insert_in_exe_part1)

        if Config.mpi['enabled']:
            callmpirobj = self.create_callstmt(insert_in=self.insert_in_exe_part1)
            callmpirobj.set_attr('name', 'mpi_comm_rank')
            callmpirobj.set_attr('args', [Config.mpi['comm'], 'kgen_mpi_rank', 'kgen_ierr'])

            iferrobj = self.create_ifthen(insert_in=self.insert_in_exe_part1)
            iferrobj.set_attr('expr', 'kgen_ierr /= 0')

            callerrobj = iferrobj.create_callstmt()
            callerrobj.set_attr('name', 'kgen_error_stop')
            callerrobj.set_attr('args', ['"MPI ERROR"'])
           
            endiferrobj = iferrobj.create_endobj() 
            endiferrobj.set_attr('blockname', 'IF')

            callmpisobj = self.create_callstmt(insert_in=self.insert_in_exe_part1)
            callmpisobj.set_attr('name', 'mpi_comm_size')
            callmpisobj.set_attr('args', [Config.mpi['comm'], 'kgen_mpi_size', 'kgen_ierr'])

            iferrobj = self.create_ifthen(insert_in=self.insert_in_exe_part1)
            iferrobj.set_attr('expr', 'kgen_ierr /= 0')

            callerrobj = iferrobj.create_callstmt()
            callerrobj.set_attr('name', 'kgen_error_stop')
            callerrobj.set_attr('args', ['"MPI ERROR"'])
           
            endiferrobj = iferrobj.create_endobj() 
            endiferrobj.set_attr('blockname', 'IF')

            currankobj = self.create_assignstmt(insert_in=self.insert_in_exe_part1)
            currankobj.set_attr('lhs', 'kgen_cur_rank')
            currankobj.set_attr('rhs', '0')

            kunitobj = self.create_assignstmt(insert_in=self.insert_in_exe_part1)
            kunitobj.set_attr('lhs', 'kgen_unit')
            kunitobj.set_attr('rhs', '-1')

            dorankobj = self.create_dowhile(insert_in=self.insert_in_exe_part1)
            dorankobj.set_attr('expr', 'kgen_cur_rank < kgen_mpi_size')

            ifrankobj = dorankobj.create_ifthen()
            ifrankobj.set_attr('expr', 'ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank')

            ifcntobj = ifrankobj.create_ifthen()
            ifcntobj.set_attr('expr', 'ANY(kgen_counter == kgen_counter_at)')

            wrankobj = ifcntobj.create_write()
            wrankobj.set_attr('ctrl_list', ['kgen_mpi_rank_conv', '*'])
            wrankobj.set_attr('item_list', ['kgen_mpi_rank'])

            wcntobj = ifcntobj.create_write()
            wcntobj.set_attr('ctrl_list', ['kgen_counter_conv', '*'])
            wcntobj.set_attr('item_list', ['kgen_counter'])

            fpathobj = ifcntobj.create_assignstmt()
            fpathobj.set_attr('lhs', 'kgen_filepath')
            fpathobj.set_attr('rhs', '"%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
                (os.path.abspath(Config.path['kernel'])+'/'+Config.callsite['subpname'].firstpartname()))
        else:
            kunitobj = self.create_assignstmt(insert_in=self.insert_in_exe_part1)
            kunitobj.set_attr('lhs', 'kgen_unit')
            kunitobj.set_attr('rhs', '-1')

            ifcntobj = self.create_ifthen(insert_in=self.insert_in_exe_part1)
            ifcntobj.set_attr('expr', 'ANY(kgen_counter == kgen_counter_at)')

            wcntobj = ifcntobj.create_write()
            wcntobj.set_attr('ctrl_list', ['kgen_counter_conv', '*'])
            wcntobj.set_attr('item_list', ['kgen_counter'])

            fpathobj = ifcntobj.create_assignstmt()
            fpathobj.set_attr('lhs', 'kgen_filepath')
            fpathobj.set_attr('rhs', '"%s." // TRIM(ADJUSTL(kgen_counter_conv))'% \
                (os.path.abspath(Config.path['kernel'])+'/'+Config.callsite['subpname'].firstpartname()))

        kunit2obj = ifcntobj.create_assignstmt()
        kunit2obj.set_attr('lhs', 'kgen_unit')
        kunit2obj.set_attr('rhs', 'kgen_get_newunit()')

        openobj = ifcntobj.create_open()
        openobj.set_attr('connect_specs', ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="REPLACE"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"'])

        iferrobj = ifcntobj.create_ifthen()
        iferrobj.set_attr('expr', 'kgen_ierr /= 0')

        callerrobj = iferrobj.create_callstmt()
        callerrobj.set_attr('name', 'kgen_error_stop')
        callerrobj.set_attr('args', ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))'])
       
        endiferrobj = iferrobj.create_endobj() 
        endiferrobj.set_attr('blockname', 'IF')

        if Config.mpi['enabled']:
            callerrobj = ifcntobj.create_callstmt()
            callerrobj.set_attr('name', 'kgen_print_mpirank_counter')
            callerrobj.set_attr('args', ['kgen_mpi_rank', 'kgen_counter'])
        else:
            callerrobj = ifcntobj.create_callstmt()
            callerrobj.set_attr('name', 'kgen_print_counter')
            callerrobj.set_attr('args', ['kgen_counter'])

         # file open tail
        self.add_line(self.insert_in_input_local_state)
        if Config.mpi['enabled']:
            endifcntobj = ifcntobj.create_endobj(insert_in=self.insert_in_input_local_state) 
            endifcntobj.set_attr('blockname', 'IF')

            endifrankobj = ifrankobj.create_endobj(insert_in=self.insert_in_input_local_state) 
            endifrankobj.set_attr('blockname', 'IF')

            incobj = dorankobj.create_assignstmt(insert_in=self.insert_in_input_local_state)
            incobj.set_attr('lhs', 'kgen_cur_rank')
            incobj.set_attr('rhs', 'kgen_cur_rank + 1')
            
            enddorankobj = dorankobj.create_endobj(insert_in=self.insert_in_input_local_state) 
            enddorankobj.set_attr('blockname', 'DO')
        else:
            endifcntobj = ifcntobj.create_endobj(insert_in=self.insert_in_input_local_state) 
            endifcntobj.set_attr('blockname', 'IF')

        self.add_openmp('END MASTER', self.insert_in_input_local_state)
   
        # file close head
        self.add_line(self.insert_in_callsite_stmts)
        self.add_openmp('MASTER', self.insert_in_callsite_stmts)

        if Config.mpi['enabled']:
            incobj = self.create_assignstmt(insert_in=self.insert_in_callsite_stmts)
            incobj.set_attr('lhs', 'kgen_cur_rank')
            incobj.set_attr('rhs', '0')

            dorankobj = self.create_dowhile(insert_in=self.insert_in_callsite_stmts)
            dorankobj.set_attr('expr', 'kgen_cur_rank < kgen_mpi_size')

            ifrankobj = dorankobj.create_ifthen(insert_in=self.insert_in_callsite_stmts)
            ifrankobj.set_attr('expr', 'ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank')

            ifcntobj = self.create_ifthen(insert_in=self.insert_in_callsite_stmts)
            ifcntobj.set_attr('expr', 'ANY(kgen_counter == kgen_counter_at)')

            wrankobj = ifcntobj.create_write(insert_in=self.insert_in_callsite_stmts)
            wrankobj.set_attr('item_list', ['"KGEN writes output state variables at count = "', 'kgen_counter', '" on mpirank = "', 'kgen_mpi_rank'])

        else:
            ifcntobj = self.create_ifthen(insert_in=self.insert_in_callsite_stmts)
            ifcntobj.set_attr('expr', 'ANY(kgen_counter == kgen_counter_at)')

        # file close tail
        self.add_line(self.insert_in_output_local_state)
        if Config.mpi['enabled']:
            endfileobj = ifcntobj.create_endfile(insert_in=self.insert_in_output_local_state)
            endfileobj.set_attr('file_unit', 'kgen_unit')

            callsleepobj = ifcntobj.create_callstmt(insert_in=self.insert_in_output_local_state)
            callsleepobj.set_attr('name', 'sleep')
            callsleepobj.set_attr('args', ['1'])

            closeobj = ifcntobj.create_close(insert_in=self.insert_in_output_local_state)
            closeobj.set_attr('close_specs', ['UNIT=kgen_unit'])

            endifcntobj = ifcntobj.create_endobj(insert_in=self.insert_in_output_local_state) 
            endifcntobj.set_attr('blockname', 'IF')

            endifrankobj = ifrankobj.create_endobj(insert_in=self.insert_in_output_local_state) 
            endifrankobj.set_attr('blockname', 'IF')

            incobj = dorankobj.create_assignstmt(insert_in=self.insert_in_output_local_state)
            incobj.set_attr('lhs', 'kgen_cur_rank')
            incobj.set_attr('rhs', 'kgen_cur_rank + 1')
            
            enddorankobj = dorankobj.create_endobj(insert_in=self.insert_in_output_local_state) 
            enddorankobj.set_attr('blockname', 'DO')

            ifmaxcobj = self.create_ifthen(insert_in=self.insert_in_output_local_state)
            ifmaxcobj.set_attr('expr', 'kgen_counter > maxval(kgen_counter_at)')

            callsleepobj = ifmaxcobj.create_callstmt(insert_in=self.insert_in_output_local_state)
            callsleepobj.set_attr('name', 'sleep')
            callsleepobj.set_attr('args', ['1'])

            wstopobj = ifmaxcobj.create_write(insert_in=self.insert_in_output_local_state)
            wstopobj.set_attr('item_list', ['"All state data is collected.  Stopping program..."'])

            callmpiaobj = ifmaxcobj.create_callstmt(insert_in=self.insert_in_output_local_state)
            callmpiaobj.set_attr('name', 'mpi_abort')
            callmpiaobj.set_attr('args', [Config.mpi['comm'], '1', 'kgen_ierr'])

            elsemaxcobj = ifmaxcobj.create_else(insert_in=self.insert_in_output_local_state)

            wcntobj = ifmaxcobj.create_write(insert_in=self.insert_in_output_local_state)
            wcntobj.set_attr('item_list', ['"kgen_counter = "', 'kgen_counter', '" at rank "', 'kgen_mpi_rank'])
           
            endifmaxcobj = ifmaxcobj.create_endobj(insert_in=self.insert_in_output_local_state)
            endifmaxcobj.set_attr('blockname', 'IF') 
        else:
            endfileobj = ifcntobj.create_endfile(insert_in=self.insert_in_output_local_state)
            endfileobj.set_attr('file_unit', 'kgen_unit')

            callsleepobj = ifcntobj.create_callstmt(insert_in=self.insert_in_output_local_state)
            callsleepobj.set_attr('name', 'sleep')
            callsleepobj.set_attr('args', ['1'])

            closeobj = ifcntobj.create_close(insert_in=self.insert_in_output_local_state)
            closeobj.set_attr('close_specs', ['UNIT=kgen_unit'])

            endifcntobj = ifcntobj.create_endobj(insert_in=self.insert_in_output_local_state) 
            endifcntobj.set_attr('blockname', 'IF')

            ifmaxcobj = self.create_ifthen(insert_in=self.insert_in_output_local_state)
            ifmaxcobj.set_attr('expr', 'kgen_counter > maxval(kgen_counter_at)')

            callsleepobj = ifmaxcobj.create_callstmt(insert_in=self.insert_in_output_local_state)
            callsleepobj.set_attr('name', 'sleep')
            callsleepobj.set_attr('args', ['1'])

            wstopobj = ifmaxcobj.create_write(insert_in=self.insert_in_output_local_state)
            wstopobj.set_attr('item_list', ['"All state data is collected.  Stopping program..."'])

            # stop
            stopobj = ifmaxcobj.create_stop(insert_in=self.insert_in_output_local_state)

            elsemaxcobj = ifmaxcobj.create_else(insert_in=self.insert_in_output_local_state)

            wcntobj = self.create_write(insert_in=self.insert_in_output_local_state)
            wcntobj.set_attr('item_list', ['"kgen_counter = "', 'kgen_counter'])
           
            endifmaxcobj = ifmaxcobj.create_endobj(insert_in=self.insert_in_output_local_state)
            endifmaxcobj.set_attr('blockname', 'IF') 

        self.add_openmp('END MASTER', insert_in=self.insert_in_output_local_state)
        self.add_line(self.insert_in_output_local_state)



########### IfThen ############
class Gen_IfThen(object):
    def process_ifthen_items(self):
        from block_statements import EndIfThen

        class_order = [Gen_Has_ExecutionPart]
        end_classes = [ EndIfThen ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_ifthen(self):
        lines = []
        lines.extend(self.tostr_list(self.exe_part))
        return lines

class GenK_IfThen(GenK_BeginStatement, Gen_IfThen, Gen_Has_ExecutionPart):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_ifthen_items()

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'IF ( %s ) THEN'%kwargs['expr']

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_ifthen())
            if self.end_obj: lines.append(self.end_obj.tostr())
            return '\n'.join(lines)

class GenS_IfThen(GenS_BeginStatement, Gen_IfThen, Gen_Has_ExecutionPart):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_ifthen_items()

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'IF ( %s ) THEN'%kwargs['expr']

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_ifthen())
            if self.end_obj: lines.append(self.end_obj.tostr())
            return '\n'.join(lines)

########### Do ############
class Gen_Do(object):
    def process_do_items(self):
        from block_statements import EndDo

        class_order = [Gen_Has_ExecutionPart]
        end_classes = [ EndDo ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_do(self):
        lines = []
        lines.extend(self.tostr_list(self.exe_part))
        return lines

    def _tokgen(self, **kwargs):
        assert kwargs.has_key('loop_control')
        return 'DO %s'%kwargs['loop_control']


class GenK_Do(GenK_BeginStatement, Gen_Do, Gen_Has_ExecutionPart):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_do_items()

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_do())
            if self.end_obj: lines.append(self.end_obj.tostr())
            return '\n'.join(lines)

class GenS_Do(GenS_BeginStatement, Gen_Do, Gen_Has_ExecutionPart):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_do_items()

    def tokgen(self, **kwargs):
        return self._tokgen(**kwargs)

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_do())
            if self.end_obj: lines.append(self.end_obj.tostr())
            return '\n'.join(lines)

########### Where ############
class Gen_Where(object):
    def process_where_items(self):
        from block_statements import EndWhere

        class_order = [Gen_Has_ExecutionPart]
        end_classes = [ EndWhere ]

        for item in self.items:
            class_order = self.insert_in_order(item, class_order, end_classes)
            item.process()

    def tostr_where(self):
        lines = []
        lines.extend(self.tostr_list(self.exe_part))
        return lines

class GenK_Where(GenK_BeginStatement, Gen_Where, Gen_Has_ExecutionPart):

    def process(self):
        if self.isvalid:
            self.process_blockhead()
            self.process_do_items()

    def tokgen(self, **kwargs):
        assert kwargs.has_key('expr')

        return 'WHERE ( %s )'%kwargs['expr']

    def tostr(self):
        if self.isvalid:
            lines = []

            l = self.tostr_blockhead()
            if l is not None: lines.append(l)
            lines.extend(self.tostr_where())
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

    def get_writename(self):
        if self.stmt is None: return
        subpname = self.get_dtype_subpname(self.stmt)
        if subpname: return 'kw_%s'%subpname

    def get_verifyname(self):
        if self.stmt is None: return
        subpname = self.get_dtype_subpname(self.stmt)
        if subpname: return 'kv_%s'%subpname

class GenK_TypeDecl(GenK_BeginStatement, Gen_TypeDecl, Gen_Has_TypeParamDefStmts, Gen_Has_PrivateOrSequence, Gen_Has_ComponentPart, \
    Gen_Has_TypeBoundProcedurePart):

    def read_components(self, subrobj):
        from base_classes import EndStatement
        from block_statements import EndType

        for item in self.items:
            if isinstance(item, GenK_TypeDeclarationStatement):

                in_names = [ get_entity_name(e) for e in item.stmt.entity_decls ]
                item.gen_read(in_names, None)

        subrobj.add_line(subrobj.insert_in_exe_part)

    def gen_read_type(self):
        self.parent.add_line(self.parent.insert_in_subprograms)
        self.parent.add_comment('reading type variable', self.parent.insert_in_subprograms)

        subrname = self.get_readname()
        if subrname is None: return

        if not self.parent.has_name(subrname, Gen_Subroutine):
            # create subroutine
            subrobj = self.parent.create_subroutine()
            subrobj.set_attr('name', subrname)
            subrobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])
            self.parent.rtypesubr = subrobj

            # variable
            varobj = subrobj.create_typedeclstmt()
            varobj.set_attr('typespec', 'TYPE')
            varobj.set_attr('selector', self.stmt.name)
            varobj.append_attr('attr_specs', 'INTENT(OUT)')
            varobj.append_attr('entity_decls', 'var')

            # kgen_unit
            unitobj = subrobj.create_typedeclstmt()
            unitobj.set_attr('typespec', 'INTEGER')
            unitobj.append_attr('attr_specs', 'INTENT(IN)')
            unitobj.append_attr('entity_decls', 'kgen_unit')

            # printvar
            printobj = subrobj.create_typedeclstmt()
            printobj.set_attr('typespec', 'CHARACTER')
            printobj.set_attr('selector', '*')
            printobj.extend_attr('attr_specs', ['INTENT(IN)', 'OPTIONAL'])
            printobj.append_attr('entity_decls', 'printvar')

            # is_true
            istrueobj = subrobj.create_typedeclstmt()
            istrueobj.set_attr('typespec', 'LOGICAL')
            istrueobj.append_attr('entity_decls', 'is_true')

            subrobj.add_line(subrobj.insert_in_exe_part)

            # call to read components
            self.read_components(subrobj)

            # create end subroutine
            endsubrobj = subrobj.create_endobj()
            endsubrobj.set_attr('name', subrname)
            endsubrobj.set_attr('blockname', 'SUBROUTINE')

            # create public stmt
            pubobj = self.parent.create_public()
            pubobj.append_attr('items', subrname)

            return subrobj

    def verify_components(self, subrobj):
        from base_classes import EndStatement
        from block_statements import EndType

        for item in self.items:
            if isinstance(item, GenK_TypeDeclarationStatement):
                vnames = item.get_verifynames()
                if not vnames: continue


                enames = [ get_entity_name(e) for e in item.stmt.entity_decls ]

                vsubrs = item.gen_verify(enames)
                for vname, ename in zip(vnames, enames):
                    callobj = subrobj.create_callstmt()
                    callobj.set_attr('name', vname)
                    callobj.set_attr('args', ['"%s"'%ename, 'dtype_check_status', 'var%%%s'%ename, 'ref_var%%%s'%ename])

        subrobj.add_line(subrobj.insert_in_exe_part)

    def gen_verify_type(self):

        self.parent.add_comment('verifying type variable', self.parent.insert_in_subprograms)

        subrname = self.get_verifyname()
        if not subrname: return

        if not self.parent.has_name(subrname, Gen_Subroutine):
            # subroutine
            subrobj = self.parent.create_subroutine()
            subrobj.set_attr('name', subrname)
            subrobj.set_attr('args', ['varname', 'check_status', 'var', 'ref_var'])

            # varname
            vnameobj = subrobj.create_typedeclstmt()
            vnameobj.set_attr('typespec', 'CHARACTER')
            vnameobj.set_attr('selector', '*')
            vnameobj.append_attr('attr_specs', 'INTENT(IN)')
            vnameobj.append_attr('entity_decls', 'varname')

            # check_status
            checkobj = subrobj.create_typedeclstmt()
            checkobj.set_attr('typespec', 'TYPE')
            checkobj.set_attr('selector', 'check_t')
            checkobj.append_attr('attr_specs', 'INTENT(INOUT)')
            checkobj.append_attr('entity_decls', 'check_status')

            # dtype ckeck status
            dcheckobj = subrobj.create_typedeclstmt()
            dcheckobj.set_attr('typespec', 'TYPE')
            dcheckobj.set_attr('selector', 'check_t')
            dcheckobj.append_attr('entity_decls', 'dtype_check_status')

            # variable
            varobj = subrobj.create_typedeclstmt()
            varobj.set_attr('typespec', 'TYPE')
            varobj.set_attr('selector', self.stmt.name)
            varobj.append_attr('attr_specs', 'INTENT(IN)')
            varobj.extend_attr('entity_decls', [ 'var', 'ref_var'])
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
            elifobj.set_attr('expr', 'dtype_check_status%numOutTol > 0')

            # increment fatal check count
            incobj = ifthenobj.create_assignstmt()
            incobj.set_attr('lhs', 'check_status%numOutTol')
            incobj.set_attr('rhs', 'check_status%numOutTol + 1')

            # create else if stmt
            elifobj = ifthenobj.create_elseif()
            elifobj.set_attr('expr', 'dtype_check_status%numInTol > 0')

            # increment warning check count
            incobj = ifthenobj.create_assignstmt()
            incobj.set_attr('lhs', 'check_status%numInTol')
            incobj.set_attr('rhs', 'check_status%numInTol + 1')

            # create end subroutine
            endifobj = ifthenobj.create_endobj()
            endifobj.set_attr('blockname', 'IF')

            # create end subroutine
            endsubrobj = subrobj.create_endobj()
            endsubrobj.set_attr('blockname', 'SUBROUTINE')
            endsubrobj.set_attr('name', subrname)

            # create public stmt
            pubobj = self.parent.create_public()
            pubobj.append_attr('items', subrname)

            return subrobj

    def process(self):
        if self.isvalid:
            if KGGenType.has_state(self.stmt.geninfo):
                krsubr = self.gen_read_type()
                self.add_line(self.parent.insert_in_subprograms)

                kvsubr = self.gen_verify_type()
                self.add_line(self.parent.insert_in_subprograms)

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

    def write_components(self, subrobj):
        from base_classes import EndStatement
        from block_statements import EndType

        for item in self.items:
            if isinstance(item, GenS_TypeDeclarationStatement):

                in_names = [ get_entity_name(e) for e in item.stmt.entity_decls ]
                item.gen_write(in_names, None)

        subrobj.add_line(subrobj.insert_in_exe_part)

    def gen_write_type(self):

        self.parent.add_line(self.parent.insert_in_subprograms)
        self.parent.add_comment('writing type variable', self.parent.insert_in_subprograms)

        subrname = self.get_writename()
        if subrname is None: return

        if not self.parent.has_name(subrname, Gen_Subroutine):
            # create subroutine
            subrobj = self.parent.create_subroutine()
            subrobj.set_attr('name', subrname)
            subrobj.set_attr('args', ['var', 'kgen_unit', 'printvar'])
            self.parent.wtypesubr = subrobj

            # variable
            varobj = subrobj.create_typedeclstmt()
            varobj.set_attr('typespec', 'TYPE')
            varobj.set_attr('selector', self.stmt.name)
            varobj.append_attr('attr_specs', 'INTENT(IN)')
            varobj.append_attr('entity_decls', 'var')

            # kgen_unit
            unitobj = subrobj.create_typedeclstmt()
            unitobj.set_attr('typespec', 'INTEGER')
            unitobj.append_attr('attr_specs', 'INTENT(IN)')
            unitobj.append_attr('entity_decls', 'kgen_unit')

            # printvar
            printobj = subrobj.create_typedeclstmt()
            printobj.set_attr('typespec', 'CHARACTER')
            printobj.set_attr('selector', '*')
            printobj.extend_attr('attr_specs', ['INTENT(IN)', 'OPTIONAL'])
            printobj.append_attr('entity_decls', 'printvar')

            # is_true
            istrueobj = subrobj.create_typedeclstmt()
            istrueobj.set_attr('typespec', 'LOGICAL')
            istrueobj.append_attr('entity_decls', 'is_true')

            subrobj.add_line(subrobj.insert_in_exe_part)

            # call to write components
            self.write_components(subrobj)

            # create end subroutine
            endsubrobj = subrobj.create_endobj()
            endsubrobj.set_attr('name', subrname)
            endsubrobj.set_attr('blockname', 'SUBROUTINE')

            # create public stmt
            pubobj = self.parent.create_public()
            pubobj.append_attr('items', subrname)

            return subrobj

    def process(self):
        if self.isvalid:
            if KGGenType.has_state(self.stmt.geninfo):
                kwsubr = self.gen_write_type()

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
    pass
#    def gensrc(self, fd):
#        self.process()
#        lines = self.tostr()
#        if lines is not None: fd.write(lines)

class GenK_BeginSource(GenK_BeginStatement, Gen_BeginSource):
    def tokgen(self, **kwargs): pass

class GenS_BeginSource(GenS_BeginStatement, Gen_BeginSource):
    def tokgen(self, **kwargs): pass

########### functions ############
def gen_driver_specpart(program):
    usekutilobj = program.create_use()
    usekutilobj.set_attr('name', 'kgen_utils_mod')
    usekutilobj.extend_attr('items', ['kgen_get_newunit', 'kgen_error_stop', 'kgen_dp'])

    usecsobj = program.create_use()
    usecsobj.set_attr('name', State.topblock['stmt'].name)
    usecsobj.extend_attr('items', [State.parentblock['stmt'].name])

    program.add_line(program.insert_in_use_stmts)
 
    impobj = program.create_implicit()

    program.add_line(program.insert_in_decl_construct)

    if Config.mpi['enabled']:
        vmpiobj = program.create_typedeclstmt()
        vmpiobj.set_attr('typespec', 'INTEGER')
        vmpiobj.append_attr('entity_decls', 'kgen_mpi_rank')

        vrconvobj = program.create_typedeclstmt()
        vrconvobj.set_attr('typespec', 'CHARACTER')
        vrconvobj.set_attr('selector', 'LEN=16')
        vrconvobj.append_attr('entity_decls', 'kgen_mpi_rank_conv')

        vlrankobj = program.create_typedeclstmt()
        vlrankobj.set_attr('typespec', 'INTEGER')
        vlrankobj.append_attr('attr_specs', 'PARAMETER')
        vlrankobj.append_attr('attr_specs', 'DIMENSION(%s)'%Config.mpi['size'])
        vlrankobj.append_attr('entity_decls', 'kgen_mpi_rank_at = (/ %s /)'%', '.join(Config.mpi['ranks']))

    vkgenobj = program.create_typedeclstmt()
    vkgenobj.set_attr('typespec', 'INTEGER')
    vkgenobj.extend_attr('entity_decls', ['kgen_ierr', 'kgen_unit', 'kgen_counter', 'kgen_repeat_counter'])

    vcconvobj = program.create_typedeclstmt()
    vcconvobj.set_attr('typespec', 'CHARACTER')
    vcconvobj.set_attr('selector', 'LEN=16')
    vcconvobj.append_attr('entity_decls', 'kgen_counter_conv')

    vlcntobj = program.create_typedeclstmt()
    vlcntobj.set_attr('typespec', 'INTEGER')
    vlcntobj.append_attr('attr_specs', 'PARAMETER')
    vlcntobj.append_attr('attr_specs', 'DIMENSION(%s)'%Config.invocation['size'])
    vlcntobj.append_attr('entity_decls', 'kgen_counter_at = (/ %s /)'%', '.join(Config.invocation['numbers']))

    vfpathobj = program.create_typedeclstmt()
    vfpathobj.set_attr('typespec', 'CHARACTER')
    vfpathobj.set_attr('selector', 'LEN=1025')
    vfpathobj.append_attr('entity_decls', 'kgen_filepath')

    vkcntobj = program.create_typedeclstmt()
    vkcntobj.set_attr('typespec', 'REAL')
    vkcntobj.set_attr('selector', 'KIND=kgen_dp')
    vkcntobj.append_attr('entity_decls', 'total_time')
  
def gen_driver_exepart(program):

    program.add_line(program.insert_in_exe_part)

    ttimeobj = program.create_assignstmt()
    ttimeobj.set_attr('lhs', 'total_time')
    ttimeobj.set_attr('rhs', '0.0_kgen_dp')

    # file open head
    if Config.mpi['enabled']:
        len = Config.mpi['size'] * Config.invocation['size']
    else:
        len = Config.invocation['size']

    doobj = program.create_do()
    doobj.set_attr('loop_control', 'kgen_repeat_counter = 0, %d'%(len-1))

    cntobj = doobj.create_assignstmt()
    cntobj.set_attr('lhs', 'kgen_counter')
    cntobj.set_attr('rhs', 'kgen_counter_at(mod(kgen_repeat_counter, %d)+1)'%Config.invocation['size'])

    wcntobj = doobj.create_write()
    wcntobj.set_attr('ctrl_list', ['kgen_counter_conv', '*'])
    wcntobj.set_attr('item_list', ['kgen_counter'])

    if Config.mpi['enabled']:

        rankobj = doobj.create_assignstmt()
        rankobj.set_attr('lhs', 'kgen_mpi_rank')
        rankobj.set_attr('rhs', 'kgen_mpi_rank_at(mod(kgen_repeat_counter, %d)+1)'%Config.mpi['size'])

        fpathobj = doobj.create_assignstmt()
        fpathobj.set_attr('lhs', 'kgen_filepath')
        fpathobj.set_attr('rhs', '"%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'% \
            Config.callsite['subpname'].firstpartname())

    else:
        fpathobj = doobj.create_assignstmt()
        fpathobj.set_attr('lhs', 'kgen_filepath')
        fpathobj.set_attr('rhs', '"%s." // TRIM(ADJUSTL(kgen_counter_conv))'% Config.callsite['subpname'].firstpartname())


    kunitobj = doobj.create_assignstmt()
    kunitobj.set_attr('lhs', 'kgen_unit')
    kunitobj.set_attr('rhs', 'kgen_get_newunit()')

    openobj = doobj.create_open()
    openobj.set_attr('connect_specs', ['UNIT=kgen_unit', 'FILE=kgen_filepath', 'STATUS="OLD"', 'ACCESS="STREAM"', \
        'FORM="UNFORMATTED"', 'ACTION="READ"', 'IOSTAT=kgen_ierr', 'CONVERT="BIG_ENDIAN"'])

    iferrobj = doobj.create_ifthen()
    iferrobj.set_attr('expr', 'kgen_ierr /= 0')

    callerrobj = iferrobj.create_callstmt()
    callerrobj.set_attr('name', 'kgen_error_stop')
    callerrobj.set_attr('args', ['"FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath))'])

    endiferrobj = iferrobj.create_endobj() 
    endiferrobj.set_attr('blockname', 'IF')

    wmsgobj = doobj.create_write()
    wmsgobj.set_attr('item_list', ['"** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' **"'])


    doobj.add_line(doobj.insert_in_exe_part)

    callobj = doobj.create_callstmt()
    callobj.set_attr('name', State.parentblock['stmt'].name)
    callobj.set_attr('args', ['kgen_unit', 'total_time'])

    doobj.add_line(doobj.insert_in_exe_part)

    closeobj = doobj.create_close()
    closeobj.set_attr('close_specs', ['UNIT=kgen_unit'])

    enddoobj = doobj.create_endobj()
    enddoobj.set_attr('blockname', 'DO')

    wv1obj = program.create_write()
    wv1obj.append_attr('item_list', '""')
    wv2obj = program.create_write()
    wv2obj.append_attr('item_list', '"******************************************************************************"')
    wv3obj = program.create_write()
    wv3obj.append_attr('item_list', '"%s summary: Total number of verification cases: %d"'%(Config.callsite['subpname'].firstpartname(), len))
    wv4obj = program.create_write()
    wv4obj.append_attr('item_list', '"%s summary: Total time of all calls (usec): ", total_time'%Config.callsite['subpname'].firstpartname())
    wv5obj = program.create_write()
    wv5obj.append_attr('item_list', '"******************************************************************************"')


def generate_kgen_driver(k_id):
    driver = GenK_BeginSource(None, k_id)

    program = GenK_Program(None, k_id)
    program.parent = driver
    program.set_attr('name', 'kernel_driver')
    driver.items.append(program)

    program.add_line(program.insert_in_use_stmts)

    gen_driver_specpart(program)
    gen_driver_exepart(program)

    program.add_line(program.insert_in_exe_part)

    endprogobj = program.create_endobj()
    endprogobj.set_attr('name', 'kernel_driver')
    endprogobj.set_attr('blockname', 'PROGRAM')

    with open('%s/kernel_driver.f90'%Config.path['kernel'], 'wb') as fd:
        driver.process()
        lines = driver.tostr()
        if lines is not None: fd.write(lines)

def generate_kgen_utils(k_id):
    from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr, \
        kgen_get_newunit, kgen_error_stop

    with open('%s/kgen_utils.f90'%Config.path['kernel'], 'wb') as f:
        f.write('MODULE kgen_utils_mod')
        f.write(kgen_utils_file_head)
        f.write('CONTAINS\n')
        f.write(kgen_utils_file_checksubr)
        f.write(kgen_get_newunit)
        f.write(kgen_error_stop)
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

    # generate kgen_driver.f90 in kernel directory
    generate_kgen_driver(0)

    # generate kgen_utils.f90 in kernel directory
    generate_kgen_utils(0)

    # construct a generation tree
    genfiles = []
    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
            kfile = genkobj(srcobj.tree, 0)
            sfile = gensobj(srcobj.tree, 0)
            if kfile is None or sfile is None:
                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
            genfiles.append((kfile, sfile, filepath))

    # process each nodes in the tree
    for kfile, sfile, filepath in genfiles:
        kfile.process()
        sfile.process()

    # generate source files from each node of the tree
    for kfile, sfile, filepath in genfiles:
        filename = os.path.basename(filepath)
        klines = kfile.tostr()
        if klines is not None:
            with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                fd.write(klines)

        slines = sfile.tostr()
        if slines is not None:
            with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                fd.write(slines)

    State.state = State.STATE_GENERATED

