"""
Fortran block statements.

-----
Permission to use, modify, and distribute this software is given under the
terms of the NumPy License. See http://scipy.org.

NO WARRANTY IS EXPRESSED OR IMPLIED.  USE AT YOUR OWN RISK.
Author: Pearu Peterson <pearu@cens.ioc.ee>
Created: May 2006
-----
"""

__all__ = ['BeginSource','Module','PythonModule','Program','BlockData','Interface',
           'Subroutine','Function','Select','WhereConstruct','ForallConstruct',
           'IfThen','If','Do','Associate','TypeDecl','Enum',
           'EndSource','EndModule','EndPythonModule','EndProgram','EndBlockData','EndInterface',
           'EndSubroutine','EndFunction','EndSelect','EndWhere','EndForall',
           'EndIfThen','EndDo','EndAssociate','EndType','EndEnum',
           ]

import re
import sys

from base_classes import BeginStatement, EndStatement, Statement,\
     AttributeHolder, ProgramBlock, Variable
from readfortran import Line
from utils import split_comma, filter_stmts, parse_bind, parse_result, AnalyzeError, is_name

import Fortran2003 # KGEN addition
from kgen_utils import Logger

class HasImplicitStmt(object):

    a = AttributeHolder(implicit_rules = {})

    def get_type_by_name(self, name):
        implicit_rules = self.a.implicit_rules
        if implicit_rules is None:
            raise AnalyzeError,'Implicit rules mapping is null while getting %r type' % (name)
        l = name[0].lower()
        if l in implicit_rules:
            return implicit_rules[l]
        # default rules:
        if l in 'ijklmn':
            l = 'default_integer'
        else:
            l = 'default_real'
        t = implicit_rules.get(l, None)
        if t is None:
            if l[8:]=='real':
                implicit_rules[l] = t = Real(self, self.item.copy('real'))
            else:
                implicit_rules[l] = t = Integer(self, self.item.copy('integer'))
        return t

    def topyf(self, tab='  '):
        implicit_rules = self.a.implicit_rules
        if implicit_rules is None:
            return tab + 'IMPLICIT NONE\n'
        items = {}
        for c,t in implicit_rules.items():
            if c.startswith('default'):
                continue
            st = t.tostr()
            if st in items:
                items[st].append(c)
            else:
                items[st] = [c]
        if not items:
            return tab + '! default IMPLICIT rules apply\n'
        s = 'IMPLICIT'
        ls = []
        for st,l in items.items():
            l.sort()
            ls.append(st + ' (%s)' % (', '.join(l)))
        s += ' ' + ', '.join(ls)
        return tab + s + '\n'

class HasUseStmt(object):

    a = AttributeHolder(use = {},
                        use_provides = {})

    def get_entity(self, name):
        for modname, modblock in self.top.a.module.items():
            for stmt in modblock.content:
                if getattr(stmt,'name','') == name:
                    return stmt
        return

    def topyf(self, tab='  '):
        sys.stderr.write('HasUseStmt.topyf not implemented\n')
        return ''

class AccessSpecs(object):

    a = AttributeHolder(private_id_list = [], public_id_list = [])

    def topyf(self, tab='  '):
        private_list = self.a.private_id_list
        public_list = self.a.public_id_list
        l = []
        if '' in private_list: l.append(tab + 'PRIVATE\n')
        if '' in public_list: l.append(tab + 'PUBLIC\n')
        for a in private_list:
            if not a: continue
            l.append(tab + 'PRIVATE :: %s\n' % (a))
        for a in public_list:
            if not a: continue
            l.append(tab + 'PUBLIC :: %s\n' % (a))
        return ''.join(l)

class HasVariables(object):

    a = AttributeHolder(variables = {},
                        variable_names = [] # defines the order of declarations
                        )

    def get_variable_by_name(self, name):
        variables = self.a.variables
        if name in variables:
            var = variables[name]
        else:
            var = variables[name] = Variable(self, name)
            self.a.variable_names.append(name)
        return var

    def topyf(self,tab='', only_variables = None):
        s = ''
        if only_variables is None:
            only_variables = self.a.variables.keys()
        for name in only_variables:
            var = self.a.variables[name]
            s += tab + str(var) + '\n'
        return s

class HasTypeDecls(object):

    a = AttributeHolder(type_decls = {})

    def topyf(self, tab=''):
        s = ''
        for name, stmt in self.a.type_decls.items():
            s += stmt.topyf(tab='  '+tab)
        return s

    def get_type_decl_by_kind(self, kind):
        type_decls = self.a.type_decls
        type_decl = type_decls.get(kind, None)
        if type_decl is None:
            return self.get_entity(kind)
        return type_decl

class HasAttributes(object):

    known_attributes = []
    a = AttributeHolder(attributes = [])

    def topyf(self, tab=''):
        s = ''
        for attr in self.a.attributes:
            s += tab + attr + '\n'
        return s

    def update_attributes(self,*attrs):
        attributes = self.a.attributes
        known_attributes = self.known_attributes
        if len(attrs)==1 and isinstance(attrs[0],(tuple,list)):
            attrs = attrs[0]
        for attr in attrs:
            uattr = attr.upper()
            if uattr not in attributes:
                if isinstance(known_attributes,(list, tuple)):
                    if uattr not in known_attributes:
                        self.warning('unknown attribute %r' % (attr))
                elif not known_attributes(uattr):
                    self.warning('unknown attribute %r' % (attr))
                attributes.append(uattr)
            else:
                self.warning('multiple specification of attribute %r' % (attr))                
        return

class HasModuleProcedures(object):

    a = AttributeHolder(module_procedures = [])

# start of KGEN addition
class HasCommonStmts(object):

    a = AttributeHolder(common_stmts = {})

# end of KGEN addition

# File block

class EndSource(EndStatement):
    """
    Dummy End statement for BeginSource.
    """
    match = staticmethod(lambda s: False)

class BeginSource(BeginStatement):
    """
    Fortran source content.
    """
    f2003_class = Fortran2003.Begin_Source # KGEN addition

    match = staticmethod(lambda s: True)
    end_stmt_cls = EndSource
    a = AttributeHolder(module = {},
                        external_subprogram = {},
                        blockdata = {},
                        )
    def tofortran(self,isfix=None):
        if isfix:
            tab = 'C'
        else:
            tab = self.get_indent_tab(isfix=isfix) + '!'
        return tab + BeginStatement.tofortran(self, isfix=isfix)
        
    def tostr(self):
        return self.blocktype.upper() + ' '+ self.name

    # start of KGEN addition
    def tokgen(self, **kwargs):
        return

    # end of KGEN addition

    def process_item(self):
        self.name = self.reader.name
        self.top = self
        self.fill(end_flag = True)
        return

    def analyze(self):
        for stmt in self.content:
            if isinstance(stmt, Module):
                stmt.analyze()
                self.a.module[stmt.name] = stmt
            elif isinstance(stmt, SubProgramStatement):
                stmt.analyze()
                self.a.external_subprogram[stmt.name] = stmt
            elif isinstance(stmt, BlockData):
                stmt.analyze()
                self.a.blockdata[stmt.name] = stmt
            else:
                stmt.analyze()
        return

    def get_classes(self):
        if self.reader.ispyf:
            return [PythonModule] + program_unit
        return program_unit

    def process_subitem(self, item):
        # MAIN block does not define start/end line conditions,
        # so it should never end until all lines are read.
        # However, sometimes F77 programs lack the PROGRAM statement,
        # and here we fix that:
        if self.reader.isf77:
            line = item.get_line()
            if line=='end':
                message = item.reader.format_message(\
                        'WARNING',
                        'assuming the end of undefined PROGRAM statement',
                        item.span[0],item.span[1])
                logger.warning(message)
                # print >> sys.stderr, message
                p = Program(self)
                p.content.extend(self.content)
                p.content.append(EndProgram(p,item))
                self.content[:] = [p]
                return
        return BeginStatement.process_subitem(self, item)

    def topyf(self, tab=''): # XXXX
        s = ''
        for name, stmt in self.a.module.items():
            s += stmt.topyf(tab=tab)
        for name, stmt in self.a.external_subprogram.items():
            s += stmt.topyf(tab=tab)
        for name, stmt in self.a.blockdata.items():
            s += stmt.topyf(tab=tab)
        return s
# Module

class EndModule(EndStatement):
    f2003_class = Fortran2003.End_Module_Stmt # KGEN addition

    match = re.compile(r'end(\s*module\s*\w*|)\Z', re.I).match

class Module(BeginStatement, HasAttributes,
             #HasImplicitStmt, HasUseStmt, HasVariables, # KGEN deletion
             HasImplicitStmt, HasUseStmt, HasVariables, HasCommonStmts, # KGEN addition
             HasTypeDecls, AccessSpecs):
    """
    MODULE <name>
     ..
    END [MODULE [name]]
    """
    f2003_class = Fortran2003.Module_Stmt # KGEN addition

    match = re.compile(r'module\s*\w+\Z', re.I).match
    end_stmt_cls = EndModule

    a = AttributeHolder(module_subprogram = {},
                        module_provides = {}, # all symbols that are public and so
                                              # can be imported via USE statement
                                              # by other blocks
                        module_interface = {}
                        )

    known_attributes = ['PUBLIC', 'PRIVATE']

    def get_classes(self):
        return access_spec + specification_part + module_subprogram_part

    def process_item(self):
        name = self.item.get_line().replace(' ','')[len(self.blocktype):].strip()
        self.name = name
        return BeginStatement.process_item(self)

    def get_provides(self):
        return self.a.module_provides

    def get_interface(self):
        return self.a.module_interface

    def analyze(self):
        content = self.content[:]

        while content:
            stmt = content.pop(0)
            if isinstance(stmt, Contains):
                stmt.analyze() # KGEN addition
                for stmt in filter_stmts(content, SubProgramStatement):
                    stmt.analyze()
                    self.a.module_subprogram[stmt.name] = stmt
                stmt = content.pop(0)
                while isinstance(stmt, Comment):
                    stmt = content.pop(0)
                if not isinstance(stmt, EndModule):
                    stmt.error('Expected END MODULE statement (analyzer).')
                continue
            #from statements import Comment
            #if not isinstance(stmt, Comment) and str(stmt).find('[')>0: import pdb; pdb.set_trace()
            stmt.analyze()

        if content:
            logger.info('Not analyzed content: %s' % content)
            # self.show_message('Not analyzed content: %s' % content)

        module_provides = self.a.module_provides
        for name, var in self.a.variables.items():
            if var.is_public():
                if name in module_provides:
                    self.warning('module data object name conflict with %s, overriding.' % (name))
                module_provides[name] = var
        return

    def topyf(self, tab=''):
        s = tab + 'MODULE '+self.name + '\n'
        s +=  HasImplicitStmt.topyf(self, tab=tab+'  ')
        s +=  AccessSpecs.topyf(self, tab=tab+'  ')
        s +=  HasAttributes.topyf(self, tab=tab+'  ')
        s +=  HasTypeDecls.topyf(self, tab=tab+'  ')
        s +=  HasVariables.topyf(self, tab=tab+'  ')
        for name, stmt in self.a.module_interface.items():
            s += stmt.topyf(tab=tab+'    ')
        s +=  tab + '  CONTAINS\n'
        for name, stmt in self.a.module_subprogram.items():
            s += stmt.topyf(tab=tab+'    ')
        s += tab + 'END MODULE ' + self.name + '\n'
        return s

    def check_private(self, name):
        if name in self.a.public_id_list: return False
        if name in self.a.private_id_list: return True
        if '' in self.a.public_id_list: return False
        if '' in self.a.private_id_list: return True
        #todo: handle generic-spec-s in id-lists.
        return

# Python Module

class EndPythonModule(EndStatement):
    match = re.compile(r'end(\s*python\s*module\s*\w*|)\Z', re.I).match

class PythonModule(BeginStatement, HasImplicitStmt, HasUseStmt,
                   HasVariables):
    """
    PYTHON MODULE <name>
     ..
    END [PYTHON MODULE [name]]
    """
    modes = ['pyf']
    match = re.compile(r'python\s*module\s*\w+\Z', re.I).match
    end_stmt_cls = EndPythonModule

    def get_classes(self):
        return [Interface, Function, Subroutine, Module]

    def process_item(self):
        self.name = self.item.get_line().replace(' ','')\
                    [len(self.blocktype):].strip()
        return BeginStatement.process_item(self)

# Program

class EndProgram(EndStatement):
    """
    END [PROGRAM [name]]
    """
    f2003_class = Fortran2003.End_Program_Stmt # KGEN addition

    match = re.compile(r'end(\s*program\s*\w*|)\Z', re.I).match

class Program(BeginStatement, ProgramBlock,
              HasAttributes, # XXX: why Program needs .attributes? # uncommended KGEN
              #HasVariables, # KGEN deletion
              HasVariables, HasCommonStmts, # KGEN addition
              HasImplicitStmt, HasUseStmt, AccessSpecs):
    """ PROGRAM [name]
    """
    f2003_class = Fortran2003.Program_Stmt # KGEN addition

    a = AttributeHolder(internal_subprogram = {})

    match = re.compile(r'program\s*\w*\Z', re.I).match
    end_stmt_cls = EndProgram

    def get_classes(self):
        return specification_part + execution_part + internal_subprogram_part

    def process_item(self):
        if self.item is not None:
            name = self.item.get_line().replace(' ','')\
                   [len(self.blocktype):].strip()
            if name:
                self.name = name
        return BeginStatement.process_item(self)

    # start of KGEN
    def analyze(self):
        content = self.content[:]

#        if self.prefix:
#            self.update_attributes(self.prefix.upper().split())
#
#        variables = self.a.variables
#        for a in self.args:
#            assert a not in variables
#            if is_name(a):
#                variables[a] = Variable(self, a)
#            elif a=='*':
#                variables[a] = Variable(self, a) # XXX: fix me appropriately
#            else:
#                raise AnalyzeError('argument must be a name or * but got %r' % (a)) 
#
#        if isinstance(self, Function):
#            var = variables[self.result] = Variable(self, self.result)
#            if self.typedecl is not None:
#                var.set_type(self.typedecl)

        while content:
            stmt = content.pop(0)
            if isinstance(stmt, Contains):
                stmt.analyze()
                for stmt in filter_stmts(content, SubProgramStatement):
                    stmt.analyze()
                    self.a.internal_subprogram[stmt.name] = stmt
                stmt = content.pop(0)
                while isinstance (stmt, Comment):
                    stmt = content.pop(0)
                assert isinstance(stmt, self.end_stmt_cls),`stmt`
            elif isinstance(stmt, self.end_stmt_cls):
                continue
            else:
                stmt.analyze()

        if content:
            logger.info('Not analyzed content: %s' % content)
            # self.show_message('Not analyzed content: %s' % content)

#        parent_provides = self.parent.get_provides()
#        if parent_provides is not None:
#            if self.name in parent_provides:
#                self.warning('module subprogram name conflict with %s, overriding.' % (self.name))
#            if self.is_public():
#                parent_provides[self.name] = self
#
#        if self.is_recursive() and self.is_elemental():
#            self.warning('C1241 violation: prefix cannot specify both ELEMENTAL and RECURSIVE')
        return
    # end of KGEN
# BlockData

class EndBlockData(EndStatement):
    """
    END [ BLOCK DATA [ <block-data-name> ] ]
    """
    match = re.compile(r'end(\s*block\s*data\s*\w*|)\Z', re.I).match
    blocktype = 'blockdata'

class BlockData(BeginStatement, HasImplicitStmt, HasUseStmt,
                #HasVariables, AccessSpecs): # KGEN deletion
                HasVariables, AccessSpecs, HasCommonStmts): # KGEN addition
    """
    BLOCK DATA [ <block-data-name> ]
    """
    f2003_class = Fortran2003.Block_Data_Stmt # KGEN addition

    end_stmt_cls = EndBlockData
    match = re.compile(r'block\s*data\s*\w*\Z', re.I).match

    def process_item(self):
        self.name = self.item.get_line()[5:].lstrip()[4:].lstrip()
        return BeginStatement.process_item(self)

    def get_classes(self):
        return specification_part

# Interface

class EndInterface(EndStatement):
    f2003_class = Fortran2003.End_Interface_Stmt # KGEN addition

    match = re.compile(r'end\s*interface\s*(\w+\s*\(.*\)|\w*)\Z',re.I).match
    blocktype = 'interface'

class Interface(BeginStatement, HasAttributes, HasImplicitStmt, HasUseStmt,
                HasModuleProcedures, AccessSpecs
                ):
    """
    INTERFACE [<generic-spec>] | ABSTRACT INTERFACE
    END INTERFACE [<generic-spec>]

    <generic-spec> = <generic-name>
                   | OPERATOR ( <defined-operator> )
                   | ASSIGNMENT ( = )
                   | <dtio-generic-spec>
    <dtio-generic-spec> = READ ( FORMATTED )
                        | READ ( UNFORMATTED )
                        | WRITE ( FORMATTED )
                        | WRITE ( UNFORMATTED )

    """
    f2003_class = Fortran2003.Interface_Stmt # KGEN addition

    modes = ['free', 'fix', 'pyf']
    match = re.compile(r'(interface\s*(\w+\s*\(.*\)|\w*)|abstract\s*interface)\Z',re.I).match
    end_stmt_cls = EndInterface
    blocktype = 'interface'

    a = AttributeHolder(interface_provides = {})

    # start of KGEN
    def resolve(self, request):
        from kgen_state import ResState
        from kgen_search import f2003_search_unknowns

        if request is None: return

        Logger.info('%s is being resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)

        # if resolved, return
        if request.state == ResState.RESOLVED:
            Logger.info('%s is already resolved'%request.uname.firstpartname(), name=request.uname, stmt=self)
            return

        if request.uname.firstpartname()==self.name:
            Logger.info('The request is being resolved by an interface', name=request.uname, stmt=self)
            request.res_stmt = self
            request.state = ResState.RESOLVED
            request.res_stmt.add_geninfo(request.uname, request)
            #self.check_spec_stmts(request.uname, request.res_stmt)

            for _stmt, _depth in walk(request.res_stmt, -1):
                if not hasattr(_stmt, 'unknowns'):
                    f2003_search_unknowns(_stmt, _stmt.f2003)
                for unk, req in _stmt.unknowns.iteritems():
                    if req.state != ResState.RESOLVED:
                        _stmt.resolve(req) 

        # defer to super
        if request.state != ResState.RESOLVED:
            super(Interface, self).resolve(request)

    # end of KGEN

    def get_classes(self):
        l = intrinsic_type_spec + interface_specification
        if self.reader.mode=='pyf':
            return [Subroutine, Function] + l
        return l

    def process_item(self):
        line = self.item.get_line()
        line = self.item.apply_map(line)
        self.isabstract = line.startswith('abstract')
        if self.isabstract:
            self.generic_spec = ''
        else:
            self.generic_spec = line[len(self.blocktype):].strip()
        self.name = self.generic_spec # XXX
        return BeginStatement.process_item(self)

    def tostr(self):
        if self.isabstract:
            return 'ABSTRACT INTERFACE'
        return 'INTERFACE '+ str(self.generic_spec)

    #def get_provides(self):
    #    return self.a.interface_provides

    def analyze(self):
        content = self.content[:]

        while content:
            stmt = content.pop(0)
            if isinstance(stmt, self.end_stmt_cls):
                break
            stmt.analyze()
            #assert isinstance(stmt, SubProgramStatement),`stmt.__class__.__name__`
        if content:
            logger.info('Not analyzed content: %s' % content)
            # self.show_message('Not analyzed content: %s' % content)

        if self.name in self.parent.a.variables:
            var = self.parent.a.variables.pop(self.name)
            self.update_attributes(var.attributes)

        if isinstance(self.parent, Module):#XXX
            parent_interface = self.parent.get_interface()
            if self.name in parent_interface:
                p = parent_interface[self.name]
                last = p.content.pop()
                assert isinstance(last,EndInterface),`last.__class__`
                p.content += self.content
                p.update_attributes(self.a.attributes)
            else:
                parent_interface[self.name] = self
            return

    def topyf(self, tab=''):
        s = tab + self.tostr() + '\n'
        s +=  HasImplicitStmt.topyf(self, tab=tab+'  ')
        s +=  HasAttributes.topyf(self, tab=tab+'  ')
        s +=  HasUseStmt.topyf(self, tab=tab+'  ')
        s += tab + 'END' + self.tostr() + '\n'
        return s

# Subroutine

class SubProgramStatement(BeginStatement, ProgramBlock,
                          HasImplicitStmt, HasAttributes,
                          #HasUseStmt, # KGEN deletion
                          HasUseStmt, HasCommonStmts, # KGEN addition
                          HasVariables, HasTypeDecls, AccessSpecs
                          ):
    """
    [ <prefix> ] <FUNCTION|SUBROUTINE> <name> [ ( <args> ) ] [ <suffix> ]
    """

    a = AttributeHolder(internal_subprogram = {})
    known_attributes = ['RECURSIVE', 'PURE', 'ELEMENTAL']


    def tofortran(self, isfix=None):
        construct_name = self.construct_name
        construct_name = construct_name + ': ' if construct_name else ''
        l=[self.get_indent_tab(isfix=isfix) + construct_name + self.tostr()]
        for c in self.content:
            l.append(c.tofortran(isfix=isfix))
        return '\n'.join(l)

    # start of KGEN
    def tokgen(self, **kwargs):
        construct_name = self.construct_name
        construct_name = construct_name + ': ' if construct_name else ''
        return construct_name + self.tostr(**kwargs)

#    def tokgen(self, items=None, to_subr=False):
#        construct_name = self.construct_name
#        construct_name = construct_name + ': ' if construct_name else ''
#
#        if not items is None:
#            tmpargs = self.args
#            self.args = items
#            outstr = self.get_indent_tab(isfix=False).lstrip() + construct_name + self.tostr(to_subr=to_subr)
#            self.args = tmpargs
#            return self.item.apply_map(outstr)
#        else:
#            return self.get_indent_tab(isfix=None).lstrip() + construct_name + self.tostr(to_subr=to_subr)
#            #return super(SubProgramStatement, self).tokgen()
    # end of KGEN

    def process_item(self):
        clsname = self.__class__.__name__.lower()
        item = self.item
        line = item.get_line()
        m = self.match(line)
        i = line.lower().find(clsname)
        assert i!=-1,`clsname, line`
        self.prefix = line[:i].rstrip()
        self.name = line[i:m.end()].lstrip()[len(clsname):].strip()
        line = line[m.end():].lstrip()
        args = []
        if line.startswith('('):
            i = line.find(')')
            assert i!=-1,`line`
            line2 = item.apply_map(line[:i+1])
            for a in line2[1:-1].split(','):
                a=a.strip()
                if not a: continue
                args.append(a)
            line = line[i+1:].lstrip()
        suffix = item.apply_map(line)
        self.bind, suffix = parse_bind(suffix, item)
        self.result = None
        if isinstance(self, Function):
            self.result, suffix = parse_result(suffix, item)
            if suffix:
                assert self.bind is None,`self.bind`
                self.bind, suffix = parse_result(suffix, item)
            if self.result is None:
                self.result = self.name
        assert not suffix,`suffix`
        self.args = args
        self.typedecl = None
        return BeginStatement.process_item(self)

    def tostr(self, **kwargs):
        # start of KGEN addition
        if kwargs.has_key('to_subr'): to_subr = True
        else: to_subr = False 

        if to_subr: clsname = 'SUBROUTINE'
        else: clsname = self.__class__.__name__.upper()

        if kwargs.has_key('name'): subpname = kwargs['name']
        else: subpname = self.name

        if kwargs.has_key('args'): args = kwargs['args']
        else: args = self.args
        # end of KGEN addition

        #clsname = self.__class__.__name__.upper() # KGEN deletion
        s = ''
        if self.prefix:
            s += self.prefix + ' '
        #if self.typedecl is not None: # KGEN deletion
        if not to_subr: # KGEN addition
            # start of KGEN addtion
            if self.typedecl is not None:
                assert isinstance(self, Function),`self.__class__.__name__`
                if not hasattr(self, 'result_in_typedecl') or not self.result_in_typedecl:
                    s += self.typedecl.tostr() + ' '
            elif hasattr(self.parent, 'funcresult_in_stmt') and self.name in self.parent.funcresult_in_stmt.keys():
                s += self.parent.funcresult_in_stmt[self.name] + ' '
            # end of KGEN addtion
            #s += self.typedecl.tostr() + ' ' # KGEN deletion
        s += clsname
        suf = ''
        #if self.result and self.result!=self.name: # KGEN deletion
        if not to_subr and self.result and self.result!=self.name: # KGEN addition
            suf += ' RESULT ( %s )' % (self.result)
        if self.bind:
            suf += ' BIND ( %s )' % (', '.join(self.bind))

        return '%s %s(%s)%s' % (s, subpname,', '.join(args),suf) # KGEN additon
        #return '%s %s(%s)%s' % (s, self.name,', '.join(self.args),suf) # KGEN deletion

    def get_classes(self):
        return f2py_stmt + specification_part + execution_part \
               + internal_subprogram_part

    def analyze(self):
        content = self.content[:]

        if self.prefix:
            self.update_attributes(self.prefix.upper().split())

        variables = self.a.variables
        for a in self.args:
            assert a not in variables
            if is_name(a):
                variables[a] = Variable(self, a)
            elif a=='*':
                variables[a] = Variable(self, a) # XXX: fix me appropriately
            else:
                raise AnalyzeError('argument must be a name or * but got %r' % (a)) 

        if isinstance(self, Function):
            var = variables[self.result] = Variable(self, self.result)
            if self.typedecl is not None:
                var.set_type(self.typedecl)

        while content:
            stmt = content.pop(0)
            if isinstance(stmt, Contains):
                stmt.analyze() # KGEN addition
                for stmt in filter_stmts(content, SubProgramStatement):
                    stmt.analyze()
                    self.a.internal_subprogram[stmt.name] = stmt
                stmt = content.pop(0)
                while isinstance (stmt, Comment):
                    stmt = content.pop(0)
                assert isinstance(stmt, self.end_stmt_cls),`stmt`
            elif isinstance(stmt, self.end_stmt_cls):
                continue
            else:
                stmt.analyze()

        if content:
            logger.info('Not analyzed content: %s' % content)
            # self.show_message('Not analyzed content: %s' % content)

        parent_provides = self.parent.get_provides()
        if parent_provides is not None:
            if self.name in parent_provides:
                self.warning('module subprogram name conflict with %s, overriding.' % (self.name))
            if self.is_public():
                parent_provides[self.name] = self

        if self.is_recursive() and self.is_elemental():
            self.warning('C1241 violation: prefix cannot specify both ELEMENTAL and RECURSIVE')
        return

    def topyf(self, tab=''):
        s = tab + self.__class__.__name__.upper()
        s += ' ' + self.name + ' (%s)' % (', '.join(self.args))
        if isinstance(self, Function) and self.result != self.name:
            s += ' RESULT (%s)' % (self.result)
        s += '\n'
        s +=  HasImplicitStmt.topyf(self, tab=tab+'  ')
        s +=  AccessSpecs.topyf(self, tab=tab+'  ')
        s +=  HasTypeDecls.topyf(self, tab=tab+'  ')
        s +=  HasVariables.topyf(self, tab=tab+'  ', only_variables = self.args)
        s += tab + 'END ' + self.__class__.__name__.upper() + ' ' + self.name + '\n'
        return s

    def is_public(self): return not self.is_private()

    def is_private(self): return self.parent.check_private(self.name)
    def is_recursive(self): return 'RECURSIVE' in self.a.attributes
    def is_pure(self): return 'PURE' in self.a.attributes
    def is_elemental(self): return 'ELEMENTAL' in self.a.attributes

class EndSubroutine(EndStatement):
    """
    END [SUBROUTINE [name]]
    """
    f2003_class = Fortran2003.End_Subroutine_Stmt # KGEN addition

    match = re.compile(r'end(\s*subroutine\s*\w*|)\Z', re.I).match


class Subroutine(SubProgramStatement):
    """
    [ <prefix> ] SUBROUTINE <name> [ ( [ <dummy-arg-list> ] ) [ <proc-language-binding-spec> ]]
    """
    f2003_class = Fortran2003.Subroutine_Stmt # KGEN addition

    end_stmt_cls = EndSubroutine
    match = re.compile(r'(recursive|pure|elemental|\s)*subroutine\s*\w+', re.I).match
    _repr_attr_names = ['prefix','bind','suffix','args'] + Statement._repr_attr_names

# Function

class EndFunction(EndStatement):
    """
    END [FUNCTION [name]]
    """
    f2003_class = Fortran2003.End_Function_Stmt # KGEN addition

    match = re.compile(r'end(\s*function\s*\w*|)\Z', re.I).match

    def tokgen(self, **kwargs):
        if kwargs.has_key('to_subr'):
            to_subr = True
        else: to_subr = False

        if to_subr:
            subp_name = 'SUBROUTINE'
        else:
            subp_name = 'FUNCTION'
        return 'END %s %s'%(subp_name,self.name or '')

#    def tokgen(self, to_subr=False):
#        if to_subr:
#            subp_name = 'SUBROUTINE'
#        else:
#            subp_name = 'FUNCTION'
#        return self.get_indent_tab(isfix=None).lstrip() + 'END %s %s'\
#               % (subp_name,self.name or '')

class Function(SubProgramStatement):
    """
    [ <prefix> ] FUNCTION <name> ( [<dummy-arg-list>] ) [<suffix>]
    <prefix> = <prefix-spec> [ <prefix-spec> ]...
    <prefix-spec> = <declaration-type-spec>
                  | RECURSIVE | PURE | ELEMENTAL
    <suffix> = <proc-language-binding-spec> [ RESULT ( <result-name> ) ]
             | RESULT ( <result-name> ) [ <proc-language-binding-spec> ]
    """
    f2003_class = Fortran2003.Function_Stmt # KGEN addition

    end_stmt_cls = EndFunction
    match = re.compile(r'(recursive|pure|elemental|\s)*function\s*\w+', re.I).match
    _repr_attr_names = ['prefix','bind','suffix','args','typedecl'] + Statement._repr_attr_names

    def subroutine_wrapper_code(self):
        name = 'f2pywrap_' + self.name
        args = ['f2pyvalue_'+self.result] + self.args
        var = self.a.variables[self.result]
        typedecl = var.get_typedecl().astypedecl()
        lines = []
        tab = ' '*6
        lines.append('%sSUBROUTINE %s(%s)' % (tab, name, ', '.join(args)))
        if isinstance(self.parent,Module):
            lines.append('%s  USE %s' % (tab, self.parent.name))
        else:
            if isinstance(typedecl, TypeStmt):
                type_decl = typedecl.get_type_decl(typedecl.name)
                if type_decl.parent is self:
                    for line in str(type_decl).split('\n'):
                        lines.append('%s  %s' % (tab, line.lstrip()))
            lines.append('%s  EXTERNAL %s' % (tab, self.name))
            lines.append('%s  %s %s' % (tab, str(typedecl).lstrip(), self.name))
        lines.append('%s  %s %s' % (tab, str(typedecl).lstrip(), args[0]))
        lines.append('!f2py intent(out) %s' % (args[0]))
        for a in self.args:
            v = self.a.variables[a]
            lines.append('%s  %s' % (tab, str(v).lstrip()))
        lines.append('%s  %s = %s(%s)' % (tab, args[0], self.name, ', '.join(self.args)))
        #lines.append('%s  print*,"%s=",%s' % (tab, args[0], args[0])) # debug line
        lines.append('%sEND SUBROUTINE %s' % (tab, name))
        return '\n'.join(lines)

    def subroutine_wrapper(self):
        code = self.subroutine_wrapper_code()
        from api import parse
        block = parse(code) # XXX: set include_dirs
        while len(block.content)==1:
            block = block.content[0]
        return block

# Handle subprogram prefixes

class SubprogramPrefix(Statement):
    """
    <prefix> <declaration-type-spec> <function|subroutine> ...
    """
    match = re.compile(r'(pure|elemental|recursive|\s)+\b',re.I).match
    def process_item(self):
        line = self.item.get_line()
        m = self.match(line)
        prefix = line[:m.end()].rstrip()
        rest = self.item.get_line()[m.end():].lstrip()
        if rest:
            self.parent.put_item(self.item.copy(prefix))
            self.item.clone(rest)
            self.isvalid = False
            return
        if self.parent.__class__ not in [Function, Subroutine]:
            self.isvalid = False
            return
        prefix = prefix + ' ' + self.parent.prefix
        self.parent.prefix = prefix.strip()
        self.ignore = True
        return

# SelectCase

class EndSelect(EndStatement):
    f2003_class = Fortran2003.End_Select_Stmt # KGEN addition

    match = re.compile(r'end\s*select\s*\w*\Z', re.I).match
    blocktype = 'select'

class Select(BeginStatement):
    """
    [ <case-construct-name> : ] SELECT CASE ( <case-expr> )

    """
    f2003_class = Fortran2003.Select_Case_Stmt # KGEN addition

    match = re.compile(r'select\s*case\s*\(.*\)\Z',re.I).match
    end_stmt_cls = EndSelect
    name = ''
    def tostr(self):
        return 'SELECT CASE ( %s )' % (self.expr)
    def process_item(self):
        self.expr = self.item.get_line()[6:].lstrip()[4:].lstrip()[1:-1].strip()
        self.construct_name = self.item.name
        return BeginStatement.process_item(self)

    def get_classes(self):
        return [Case] + execution_part_construct

# Where

class EndWhere(EndStatement):
    """
    END WHERE [ <where-construct-name> ]
    """
    f2003_class = Fortran2003.End_Where_Stmt # KGEN addition

    match = re.compile(r'end\s*\where\s*\w*\Z',re.I).match


class Where(BeginStatement):
    """
    [ <where-construct-name> : ] WHERE ( <mask-expr> )
    <mask-expr> = <logical-expr>
    """
    f2003_class = Fortran2003.Where_Construct_Stmt # KGEN addition

    match = re.compile(r'where\s*\([^)]*\)\Z',re.I).match
    end_stmt_cls = EndWhere
    name = ''
    def tostr(self):
        return 'WHERE ( %s )' % (self.expr)
    def process_item(self):
        self.expr = self.item.apply_map(self.item.get_line()[5:].lstrip()[1:-1].strip()) # KGEN addition
        #self.expr = self.item.get_line()[5:].lstrip()[1:-1].strip() # KGEN deletion
        self.construct_name = self.item.name
        return BeginStatement.process_item(self)

    def get_classes(self):
        return [Assignment, WhereStmt,
                WhereConstruct, ElseWhere
                ]

    # start of KGEN addition
    def analyze(self):
        pass
    # end of KGEN addition

WhereConstruct = Where

# Forall

class EndForall(EndStatement):
    """
    END FORALL [ <forall-construct-name> ]
    """
    f2003_class = Fortran2003.End_Forall_Stmt # KGEN addition

    match = re.compile(r'end\s*forall\s*\w*\Z',re.I).match

class Forall(BeginStatement):
    """
    [ <forall-construct-name> : ] FORALL <forall-header>
      [ <forall-body-construct> ]...
    <forall-body-construct> = <forall-assignment-stmt>
                            | <where-stmt>
                            | <where-construct>
                            | <forall-construct>
                            | <forall-stmt>
    <forall-header> = ( <forall-triplet-spec-list> [ , <scalar-mask-expr> ] )
    <forall-triplet-spec> = <index-name> = <subscript> : <subscript> [ : <stride> ]
    <subscript|stride> = <scalar-int-expr>
    <forall-assignment-stmt> = <assignment-stmt> | <pointer-assignment-stmt>
    """
    f2003_class = Fortran2003.Forall_Construct_Stmt # KGEN addition

    end_stmt_cls = EndForall
    match = re.compile(r'forall\s*\(.*\)\Z',re.I).match
    name = ''
    def process_item(self):
        self.specs = self.item.get_line()[6:].lstrip()[1:-1].strip()
        return BeginStatement.process_item(self)
    def tostr(self):
        return 'FORALL (%s)' % (self.specs)
    def get_classes(self):
        return [GeneralAssignment, WhereStmt, WhereConstruct,
                ForallConstruct, ForallStmt]

ForallConstruct = Forall

# IfThen

class EndIfThen(EndStatement):
    """
    END IF [ <if-construct-name> ]
    """
    f2003_class = Fortran2003.End_If_Stmt # KGEN addition

    match = re.compile(r'end\s*if\s*\w*\Z', re.I).match
    blocktype = 'if'

class IfThen(BeginStatement):
    """
    [<if-construct-name> :] IF ( <scalar-logical-expr> ) THEN

    IfThen instance has the following attributes:
      expr
    """
    f2003_class = Fortran2003.If_Then_Stmt # KGEN addition

    match = re.compile(r'if\s*\(.*\)\s*then\Z',re.I).match
    end_stmt_cls = EndIfThen
    name = ''

    def tostr(self):
        return 'IF (%s) THEN' % (self.expr)

    def process_item(self):
        item = self.item
        line = item.get_line()[2:-4].strip()
        assert line[0]=='(' and line[-1]==')',`line`
        self.expr = item.apply_map(line[1:-1].strip())
        self.construct_name = item.name
        return BeginStatement.process_item(self)

    def get_classes(self):
        return [Else, ElseIf] + execution_part_construct

class If(BeginStatement):
    """
    IF ( <scalar-logical-expr> ) action-stmt
    """
    f2003_class = Fortran2003.If_Stmt # KGEN addition

    match = re.compile(r'if\s*\(',re.I).match

    def process_item(self):
        item = self.item
        mode = self.reader.mode
        classes = self.get_classes()
        classes = [cls for cls in classes if mode in cls.modes]

        line = item.get_line()[2:].lstrip()
        i = line.find(')')
        expr = line[1:i].strip()
        line = line[i+1:].strip()
        if line.lower()=='then':
            self.isvalid = False
            return
        self.expr = item.apply_map(expr)

        if not line:
            newitem = self.get_item()
        else:
            newitem = item.copy(line, apply_map=True)
        newline = newitem.get_line()
        for cls in classes:
            if cls.match(newline):
                stmt = cls(self, newitem)
                if stmt.isvalid:
                    self.content.append(stmt)
                    return
        if not line:
            self.put_item(newitem)
        self.isvalid = False
        return

    def tostr(self):
        assert len(self.content)==1,`self.content`

        # start of KGEN addtion
        line = str(self.content[0]).lstrip()

        # remove label
        line = self.remove_label(line)
        line = self.remove_construct_name(line)

        return 'IF (%s) %s' % (self.expr, line)
        # end of KGEN addtion
        #return 'IF (%s) %s' % (self.expr, str(self.content[0]).lstrip()) # KGEN deletion

    def tofortran(self,isfix=None):
        return self.get_indent_tab(isfix=isfix) + self.tostr()

    def get_classes(self):
        return action_stmt

# Do

class EndDo(EndStatement):
    """
    END DO [ <do-construct-name> ]
    """
    f2003_class = Fortran2003.End_Do_Stmt # KGEN addition

    match = re.compile(r'end\s*do\s*\w*\Z', re.I).match
    blocktype = 'do'

class Do(BeginStatement):
    """
    [ <do-construct-name> : ] DO label [loopcontrol]
    [ <do-construct-name> : ] DO [loopcontrol]

    """
    f2003_class = Fortran2003.Do_Stmt # KGEN addition

    match = re.compile(r'do\b\s*\d*',re.I).match
    item_re = re.compile(r'do\b\s*(?P<label>\d*)\s*,?\s*(?P<loopcontrol>.*)\Z',re.I).match
    end_stmt_cls = EndDo
    name = ''

    def tostr(self):
        l = ['DO']
        for part in [self.endlabel, self.loopcontrol]:
            if part:
                l.append(str(part))
        return ' '.join(l)

    def process_item(self):
        item = self.item
        line = item.get_line()
        m = self.item_re(line)
        label = m.group('label').strip() or None
        if label:
            label = int(label)
        self.endlabel = label
        self.construct_name = item.name
        # start of KGEN addition
        loopcontrol = m.group('loopcontrol').strip()
        if loopcontrol is None or loopcontrol.startswith('='):
            self.isvalid = False
            return
        self.loopcontrol = loopcontrol
        # end of KGEN addition
        #self.loopcontrol = m.group('loopcontrol').strip() # KGEN deletion
        return BeginStatement.process_item(self)

    def process_subitem(self, item):
        r = False
        if self.endlabel:
            label = item.label
            if label == self.endlabel:
                r = True
                if isinstance(self.parent, Do) and label==self.parent.endlabel:
                    # the same item label may be used for different block ends
                    self.put_item(item)
        return BeginStatement.process_subitem(self, item) or r

    def get_classes(self):
        return execution_part_construct

# Associate

class EndAssociate(EndStatement):
    """
    END ASSOCIATE [ <associate-construct-name> ]
    """
    f2003_class = Fortran2003.End_Associate_Stmt # KGEN addition

    match = re.compile(r'end\s*associate\s*\w*\Z',re.I).match

class Associate(BeginStatement):
    """
    [ <associate-construct-name> : ] ASSOCIATE ( <association-list> )
      <block>

    <association> = <associate-name> => <selector>
    <selector> = <expr> | <variable>
    """
    f2003_class = Fortran2003.Associate_Stmt # KGEN addition

    match = re.compile(r'associate\s*\(.*\)\Z',re.I).match
    end_stmt_cls = EndAssociate

    def process_item(self):
        line = self.item.get_line()[9:].lstrip()
        self.associations = line[1:-1].strip()
        return BeginStatement.process_item(self)
    def tostr(self):
        return 'ASSOCIATE (%s)' % (self.associations)
    def get_classes(self):
        return execution_part_construct

# Type

class EndType(EndStatement):
    """
    END TYPE [<type-name>]
    """
    f2003_class = Fortran2003.End_Type_Stmt # KGEN addition

    match = re.compile(r'end\s*type\s*\w*\Z', re.I).match
    blocktype = 'type'

class Type(BeginStatement, HasVariables, HasAttributes, AccessSpecs):
    """
    TYPE [ [ , <type-attr-spec-list>] :: ] <type-name> [ ( <type-param-name-list> ) ]
    <type-attr-spec> = <access-spec> | EXTENDS ( <parent-type-name> )
                       | ABSTRACT | BIND(C)
    """
    f2003_class = Fortran2003.Derived_Type_Stmt # KGEN addition

    match = re.compile(r'type\b\s*').match
    end_stmt_cls = EndType

    a = AttributeHolder(extends = None,
                        parameters = {},
                        component_names = [], # specifies component order for sequence types
                        components = {}
                        )
    known_attributes = re.compile(r'\A(PUBLIC|PRIVATE|SEQUENCE|ABSTRACT|BIND\s*\(.*\))\Z',re.I).match

    def process_item(self):
        line = self.item.get_line()[4:].lstrip()
        if line.startswith('('):
            self.isvalid = False
            return
        specs = []
        i = line.find('::')
        if i!=-1:
            for s in line[:i].split(','):
                s = s.strip()
                if s: specs.append(s)
            line = line[i+2:].lstrip()
        self.specs = specs
        i = line.find('(')
        if i!=-1:
            self.name = line[:i].rstrip()
            assert line[-1]==')',`line`
            self.params = split_comma(line[i+1:-1].lstrip())
        else:
            self.name = line
            self.params = []
        if not is_name(self.name):
            self.isvalid = False
            return
        return BeginStatement.process_item(self)

    def tostr(self):
        s = 'TYPE'
        if self.specs:
            s += ', '.join(['']+self.specs) + ' ::'
        s += ' ' + self.name
        if self.params:
            s += ' ('+', '.join(self.params)+')'
        return s

    def get_classes(self):
        return [Integer] + private_or_sequence + component_part +\
               type_bound_procedure_part

    def analyze(self):
        for spec in self.specs:
            i = spec.find('(')
            if i!=-1:
                assert spec.endswith(')'),`spec`
                s = spec[:i].rstrip().upper()
                n = spec[i+1:-1].strip()
                if s=='EXTENDS':
                    self.a.extends = n
                    continue
                elif s=='BIND':
                    args,rest = parse_bind(spec)
                    assert not rest,`rest`
                    spec = 'BIND(%s)' % (', '.join(args))
                else:
                    spec = '%s(%s)' % (s,n)
            else:
                spec = spec.upper()
            self.update_attributes(spec)

        component_names = self.a.component_names
        content = self.content[:]
        while content:
            stmt = content.pop(0)
            if isinstance(stmt, self.end_stmt_cls):
                break
            stmt.analyze()

        if content:
            logging.info('Not analyzed content: %s' % content)
            # self.show_message('Not analyzed content: %s' % content)

        parameters = self.a.parameters
        components = self.a.components
        component_names = self.a.component_names
        for name in self.a.variable_names:
            var = self.a.variables[name]
            if name in self.params:
                parameters[name] = var
            else:
                component_names.append(name)
                components[name] = var

        self.parent.a.type_decls[self.name] = self

        parent_provides = self.parent.get_provides()
        if parent_provides is not None:
            if self.is_public():
                if self.name in parent_provides:
                    self.warning('type declaration name conflict with %s, overriding.' % (self.name))
                parent_provides[self.name] = self

        return

    def topyf(self, tab=''):
        s = tab + 'TYPE'
        if self.a.extends is not None:
            s += ', EXTENDS(%s) ::' % (self.a.extends)
        s += ' ' + self.name
        if self.a.parameters:
            s += ' (%s)' % (', '.join(self.a.parameters))
        s += '\n'
        s += AccessSpecs.topyf(self, tab=tab+'  ')
        s += HasAttributes.topyf(self, tab=tab+'  ')
        s += HasVariables.topyf(self, tab=tab+'  ')
        s += tab + 'END TYPE ' + self.name + '\n'
        return s

    # Wrapper methods:

    def get_bit_size(self, _cache={}):
        try:
            return _cache[id(self)]
        except KeyError:
            s = 0
            for name,var in self.a.components.items():
                s += var.get_bit_size()
            _cache[id(self)] = s
        return s

    def is_public(self): return not self.is_private()

    def is_private(self):
        if 'PUBLIC' in self.a.attributes: return False
        if 'PRIVATE' in self.a.attributes: return True
        return self.parent.check_private(self.name)

TypeDecl = Type

# Enum

class EndEnum(EndStatement):
    """
    END ENUM
    """
    match = re.compile(r'end\s*enum\Z',re.I).match
    blocktype = 'enum'

class Enum(BeginStatement):
    """
    ENUM , BIND(C)
      <enumerator-def-stmt>
      [ <enumerator-def-stmt> ]...
    """
    blocktype = 'enum'
    end_stmt_cls = EndEnum
    match = re.compile(r'enum\s*,\s*bind\s*\(\s*c\s*\)\Z',re.I).match
    def process_item(self):
        return BeginStatement.process_item(self)
    def get_classes(self):
        return [Enumerator]

###################################################

import statements
import typedecl_statements
__all__.extend(statements.__all__)
__all__.extend(typedecl_statements.__all__)

from statements import *
from typedecl_statements import *

f2py_stmt = [Threadsafe, FortranName, Depend, Check, CallStatement,
             CallProtoArgument]

access_spec = [Public, Private]

interface_specification = [Function, Subroutine,
                           ModuleProcedure
                           ]

module_subprogram_part = [ Contains, Function, Subroutine ]

specification_stmt = access_spec + [ Allocatable, Asynchronous, Bind,
    Common, Data, Dimension, Equivalence, External, Intent, Intrinsic,
    Namelist, Optional, Pointer, Protected, Save, Target, Volatile,
    Value ]

intrinsic_type_spec = [ SubprogramPrefix, Integer , Real,
    DoublePrecision, Complex, DoubleComplex, Character, Logical, Byte
    ]

derived_type_spec = [  ]
type_spec = intrinsic_type_spec + derived_type_spec
declaration_type_spec = intrinsic_type_spec + [ TypeStmt, Class ]

type_declaration_stmt = declaration_type_spec

private_or_sequence = [ Private, Sequence ]

component_part = declaration_type_spec + [ ModuleProcedure ]

proc_binding_stmt = [SpecificBinding, GenericBinding, FinalBinding]

type_bound_procedure_part = [Contains, Private] + proc_binding_stmt

#R214
#action_stmt = [ Allocate, GeneralAssignment, Assign, Backspace, Call, Close, # KGEN deletion
action_stmt = [ Allocate, PointerAssignment, GeneralAssignment, Assignment, Assign, Backspace, Call, Close, # KGEN addition
    Continue, Cycle, Deallocate, Endfile, Exit, Flush, ForallStmt,
    Goto, If, Inquire, Nullify, Open, Print, Read, Return, Rewind,
    Stop, Wait, WhereStmt, Write, ArithmeticIf, ComputedGoto,
    AssignedGoto, Pause ]
# GeneralAssignment = Assignment + PointerAssignment
# EndFunction, EndProgram, EndSubroutine - part of the corresponding blocks

#executable_construct = [ Associate, Do, ForallConstruct, IfThen, # KGEN deletion
executable_construct = [ Associate, Do, ForallStmt, ForallConstruct, IfThen, # KGEN addition
    Select, WhereConstruct ] + action_stmt
#Case, see Select

execution_part_construct = executable_construct + [ Format, Entry,
    Data ]

execution_part = execution_part_construct[:]

#C201, R208
for cls in [EndFunction, EndProgram, EndSubroutine]:
    try: execution_part.remove(cls)
    except ValueError: pass

internal_subprogram = [Function, Subroutine]

internal_subprogram_part = [ Contains, ] + internal_subprogram

declaration_construct = [ TypeDecl, Entry, Enum, Format, Interface,
    Parameter, ModuleProcedure, ] + specification_stmt + \
    type_declaration_stmt
# stmt-function-stmt

implicit_part = [ Implicit, Parameter, Format, Entry ]

specification_part = [ Use, Import ] + implicit_part + \
                     declaration_construct


external_subprogram = [Function, Subroutine]

main_program = [Program] + specification_part + execution_part + \
               internal_subprogram_part

program_unit = main_program + external_subprogram + [Module,
                                                     BlockData ]
