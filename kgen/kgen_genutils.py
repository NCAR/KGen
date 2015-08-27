# kgen_genutils.py

import os

from api import walk
from base_classes import BeginStatement
from block_statements import HasUseStmt,Type
from statements import Comment, Use
from typedecl_statements import TypeDeclarationStatement
from Fortran2003 import Entity_Decl

from kgen_state import State
from kgen_utils import Config, Logger, ProgramException, KGName
from kgen_extra import kgen_file_header, kgen_subprograms, kgen_print_counter, kgen_verify_intrinsic_checkpart, \
    kgen_verify_numeric_array, kgen_verify_nonreal_array, kgen_utils_file_head, kgen_utils_file_checksubr

###############################################################################
# COMMON
###############################################################################

class SrcStage(object):
    ( BEFORE_MODULE_STMT, AFTER_MODULE_STMT, AFTER_MODULE_USE_PART, AFTER_MODULE_CONTAINS_STMT, \
        AFTER_SUBP_STMT, AFTER_SUBP_USE_PART, AFTER_SUBP_SPEC_PART, AFTER_CALLSITE_STMT, \
        AFTER_SUBP_CONTAINS_STMT, AFTER_ENDSUBP_STMT, AFTER_ENDMODULE_STMT ) = range(11)

TAB = ' '*4
split_chars = ' ()[],'

verify_subrnames = []

def write(f, line, d=0, n=True, pren=False, postn=False):
    if f is None or line is None: return

    if pren: f.write('\n')
    if n: line += '\n'
    f.write(TAB*d + line)
    if postn: f.write('\n')

def split_write(f, line, d=0):
    in_comment = False
    is_firstline = True
    sline = ''
    tword = ''
    quote_char = ''
    for c in line:
        splitable = False
        tword += c
        if quote_char:
            if c==quote_char:
                splitable = True
                quote_char = ''
            else: quote_char = c
        elif c is '!':
            in_comment = True
            splitable = True
        elif c is '"' or c is "'":
            quote_char = c
        elif c in split_chars:
            splitable = True

        if splitable:
            if len(sline)+len(tword)+len(TAB)*d<Config.fort['maxlinelen']-3:
                sline += tword
                tword = ''
            else:
                if is_firstline:
                    is_firstline = False
                    if not in_comment:
                        sline = sline + '&'
                else:
                    if in_comment:
                        sline = '! ' + sline
                    elif quote_char:
                        sline = '&' + sline + '&'
                    else:
                        sline = sline + '&'

                write(f, sline, d=d)

                sline = tword
                tword = ''
    if sline or tword:
        sline += tword
        if not is_firstline:
            if in_comment:
                sline = '! ' + sline
            elif quote_char:
                sline = '&' + sline
        write(f, sline, d=d)
    #write(f, line, d=d)

def write_stmt(f, stmt, depth, genonly=False, **kwargs):
    from kgen_state import ResState
    from block_statements import execution_part, Else, ElseIf, ElseWhere, EndDo, EndSelect, EndForall, EndIfThen

    if not hasattr(write_stmt, "writtenlineno"):
        write_stmt.writtenlineno = -1
    if not hasattr(write_stmt, "endblock"):
        write_stmt.endblock = None

    unres = []
    unresstr = ''
    if hasattr(stmt, 'unknowns'):
        for uname, res in stmt.unknowns.iteritems():
            if res.state!=ResState.RESOLVED: unres.append(uname.firstpartname()) 
    if len(unres)>0:
        unresstr = ' ! UNRESOLVED: %s' % ', '.join(unres)
        #print '%s at line %d of %s'%(unresstr, stmt.item.span[0], stmt.reader.id)
        print '%s in %s'%(unresstr, stmt.reader.id)

    if isinstance(stmt, HasUseStmt): write(f, '')
    if isinstance(stmt, Comment):
        if not stmt.item.comment.strip().upper().startswith('!KGEN'):
            if write_stmt.writtenlineno==stmt.item.span[0] and stmt.item.span[0]>1:
                f.seek(-1,1)
                split_write(f, ' ' + stmt.tokgen(), d=0)
                write_stmt.writtenlineno = stmt.item.span[0]
                f.seek(0,2)
            else:
                split_write(f, stmt.tokgen(**kwargs), d=depth)
                write_stmt.writtenlineno = stmt.item.span[0]
    # write only if kgen-marked
    elif genonly:
        if hasattr(stmt, 'unknowns') or hasattr(stmt, 'geninfo'):
            comment_str = ''
            if write_stmt.endblock is not None:
                comment_str = '!kgen_excluded '
            elif hasattr(stmt, 'exclude_names') and State.state==State.STATE_GENERATED:
                for excname, actions in stmt.exclude_names.iteritems():
                    if 'comment' in actions:
                        if isinstance(stmt, BeginStatement):
                            write_stmt.endblock = stmt.content[-1]
                        comment_str = '!kgen_excluded '
                        break
            #split_write(f, comment_str+stmt.tokgen(**kwargs)+unresstr, d=depth)
            if stmt.__class__ in execution_part+[Else, ElseIf, ElseWhere, EndDo, EndSelect, EndForall, EndIfThen]:
                if write_stmt.writtenlineno!=stmt.item.span[0]:
                    srclines = '\n'.join(stmt.top.srcfile.prep[stmt.item.span[0]-1:stmt.item.span[1]])
                    write(f, comment_str+srclines+unresstr)
            else:
                split_write(f, comment_str+stmt.tokgen(**kwargs)+unresstr, d=depth)
            write_stmt.writtenlineno = stmt.item.span[0]

            if write_stmt.endblock is not None and stmt is write_stmt.endblock:
                write_stmt.endblock = None
    else:
        comment_str = ''

        if write_stmt.endblock is not None:
            comment_str = '!kgen_excluded '
        elif hasattr(stmt, 'exclude_names') and State.state==State.STATE_GENERATED:
            for excname, actions in stmt.exclude_names.iteritems():
                if 'comment' in actions:
                    if isinstance(stmt, BeginStatement):
                        write_stmt.endblock = stmt.content[-1]
                    comment_str = '!kgen_excluded '
                    break
        #split_write(f, comment_str+stmt.tokgen(**kwargs)+unresstr, d=depth)
        if stmt.__class__ in execution_part+[Else, ElseIf, ElseWhere, EndDo, EndSelect, EndForall, EndIfThen]:
            if write_stmt.writtenlineno!=stmt.item.span[0]:
                srclines = '\n'.join(stmt.top.srcfile.prep[stmt.item.span[0]-1:stmt.item.span[1]])
                write(f, comment_str+srclines+unresstr)
        else:
            split_write(f, comment_str+stmt.tokgen(**kwargs)+unresstr, d=depth)

        write_stmt.writtenlineno = stmt.item.span[0]

        if write_stmt.endblock is not None and stmt is write_stmt.endblock:
            write_stmt.endblock = None

def write_rw_usepart_module(f, depth, rd, module):
    if rd:
        rwstr = 'read'
        write(f, 'USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check', d=depth+1)
    else:
        rwstr = 'write'

    # use stmts for type write_var
    for mod_name in module['mod_rw_var_depends']:
        mod_num = State.modules[mod_name]['num']
        write(f, 'USE %s, ONLY : kgen_%s_mod%d => kgen_%s'%(mod_name,rwstr,mod_num,rwstr), d=depth)
        if rd:
            write(f, 'USE %s, ONLY : kgen_verify_mod%d => kgen_verify'%(mod_name,mod_num), d=depth)

def write_interface_rw_var(f, depth, rd, tkdpatlist, mod_num, dtypelist=None):

    if rd: rwstr = 'read'
    else: rwstr = 'write'

    # original declarations of type
    if dtypelist:
        write(f, '! %s interface'%rwstr, d=depth, pren=True)
        if mod_num>=0: write(f, 'PUBLIC kgen_%s'%rwstr, depth)
        write(f, 'INTERFACE kgen_%s'%rwstr, d=depth)

        for dtype in dtypelist:
            write(f, 'MODULE PROCEDURE kgen_%s_%s'%(rwstr,dtype.name), d=depth+1)

        write(f, 'END INTERFACE kgen_%s'%rwstr, d=depth, postn=True)

    if rd and dtypelist:
        if mod_num>=0: write(f, 'PUBLIC kgen_verify', depth)
        write(f, 'INTERFACE kgen_verify', d=depth)

        for dtype in dtypelist:
            write(f, 'MODULE PROCEDURE kgen_verify_%s'%dtype.name, d=depth+1)

        write(f, 'END INTERFACE kgen_verify', d=depth, postn=True)

def write_subroutines_type_rw_var(f, depth, rd, dtypelist):

    if rd:
        rwstr = 'read'
        intent = 'out'
    else:
        rwstr = 'write'
        intent = 'in'

    for dtype in dtypelist:

        write(f, 'SUBROUTINE kgen_%s_%s(var, kgen_unit, printvar)'%(rwstr, dtype.name), d=depth)
        write(f, 'INTEGER, INTENT(IN) :: kgen_unit', d=depth+1)
        write(f, 'CHARACTER(*), INTENT(IN), OPTIONAL :: printvar', d=depth+1)
        write(f, 'TYPE(%s), INTENT(%s) :: var'%(dtype.name, intent), d=depth+1)

        for comp in dtype.content:
            if isinstance(comp, TypeDeclarationStatement):
                for decl in comp.entity_decls:
                    entity = Entity_Decl(decl)
                    varname = entity.items[0].string.lower()
                    var = dtype.a.variables[varname]

                    # generate dtype subroutine name to use in call stmt 
                    subrname = GenBase.subrname_stmt(comp, var)

                    if subrname:
                        write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+1)
                        write(f, 'CALL kgen_%s_%s(var%%%s, kgen_unit, printvar=printvar//"%%%s")'%(rwstr, subrname, varname, varname), d=depth+2)
                        write(f, 'ELSE', d=depth+1)
                        write(f, 'CALL kgen_%s_%s(var%%%s, kgen_unit)'%(rwstr, subrname, varname), d=depth+2)
                        write(f, 'END IF', d=depth+1)
                    else:
                        write(f, '%s(UNIT=kgen_unit) var%%%s'%(rwstr.upper(), varname), d=depth+1)
                        write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+1)
                        write(f, 'print *, "** " // printvar // "%%%s **", var%%%s'%(varname, varname), d=depth+2)
                        write(f, 'END IF', d=depth+1)

        write(f, 'END SUBROUTINE', d=depth)

class GenBase(object):
    @classmethod
    def vartypename_stmt(cls, stmt, var, is_elem=False, modcall4dtype=False):
        if stmt.is_derived():
            dtype = stmt.get_res_stmt(stmt.name)
            vtname = dtype.name
            if not is_elem and (var.is_pointer() or var.is_array()):
                return stmt.name
            else:
                if isinstance(dtype, Use):
                    if dtype.module is State.topblock['stmt']:
                        mod_num = 0
                    elif State.modules.has_key(dtype.module.name):
                        mod_num = State.modules[dtype.module.name]['num']
                    else: raise ProgramException('No module is found')
                    return 'mod%d'%mod_num
                elif modcall4dtype:
                    if dtype.name in State.topblock['stmt'].a.type_decls.keys():
                        mod_num = 0
                    elif State.modules.has_key(dtype.parent.name):
                        mod_num = State.modules[dtype.parent.name]['num']
                    else: raise ProgramException('No module is found')
                    return 'mod%d'%mod_num
                else:
                    return stmt.name
        else:
            return '%s_%s'%(stmt.__class__.__name__.lower(), stmt.get_kind())

    @classmethod
    def vartypename_tkdpat(cls, vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=False, modname=None):
        if dtype:
            if not is_elem and (varpointer or vardim!=0):
                if not modname is None and modname:
                    if isinstance(dtype, Use):
                        if dtype.module is State.topblock['stmt']:
                            mod_num = 0
                        elif State.modules.has_key(dtype.module.name):
                            mod_num = State.modules[dtype.module.name]['num']
                        else: raise ProgramException('No module is found')
                        return 'mod%d'%mod_num
                    else:
                        return vartype
                else:
                    return vartype
            else:
                if isinstance(dtype, Use):
                    if not modname is None and not modname:
                        return ''

                    if dtype.module is State.topblock['stmt']:
                        mod_num = 0
                    elif State.modules.has_key(dtype.module.name):
                        mod_num = State.modules[dtype.module.name]['num']
                    else: raise ProgramException('No module is found')
                    return 'mod%d'%mod_num
                else:
                    return vartype
        else:
            return '%s_%s'%(vartype, varkind)

    @classmethod
    def subrname_stmt(cls, stmt, var, is_elem=False, call4arr=False, modcall4dtype=False, subr4kind=False):
        """ Generation subroutine name for call to subroutine """

        vtname = cls.vartypename_stmt(stmt, var, is_elem=is_elem, modcall4dtype=modcall4dtype)

        subrname = ''

        if var.is_array():
            if var.is_explicit_shape_array():
                if stmt.is_derived() or call4arr:
                    subrname += '_dim%d'%var.rank
                else:
                    return None
            else:
                subrname += '_dim%d'%var.rank

        if var.is_allocatable():
            subrname += '_alloc'

        if var.is_pointer():
            subrname += '_ptr'

        if subrname:
            return vtname+subrname
        elif stmt.is_derived():
            return vtname
        elif subr4kind and not isinstance(stmt.get_kind(), int):
            return vtname
        else:
            return None

    @classmethod
    def subrname_tkdpat(cls, vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=False, modname=None, call4arr=False):
        """ Generation subroutine name for subroutine definition """

        vtname = cls.vartypename_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=is_elem, modname=modname)

        subrname = ''

        if vardim!=0:
            if vardim<0:
                if dtype or call4arr:
                    subrname += '_dim%d'%abs(vardim)
                else:
                    return None
            else:
                subrname += '_dim%d'%vardim

        if varalloc:
            subrname += '_alloc'

        if varpointer:
            subrname += '_ptr'

        if subrname:
            return vtname+subrname
        elif dtype and vtname:
            return vtname
        else:
            return None


class GenIOStmt(GenBase):
    @classmethod
    def write_dtype_arr(cls, f, depth, indices, varname):
        from Fortran2003 import Name, Scalar_Int_Expr, Subscript_Triplet, Section_Subscript_List, \
            Data_Ref

        if isinstance(indices, Name) or isinstance(indices, Scalar_Int_Expr) or \
            isinstance(indices, Data_Ref):
            write(f, 'kgen_indexes(1,1) = %s'%str(indices), d=depth)
            write(f, 'kgen_indexes(2,1) = %s'%str(indices), d=depth)
            write(f, 'kgen_indexes(3,1) = 1', d=depth)
        elif isinstance(indices, Subscript_Triplet):
            if indices.items[0] is None:
                write(f, 'kgen_indexes(1,1) = LBOUND(%s)'%varname, d=depth)
            else:
                write(f, 'kgen_indexes(1,1) = %s'%str(indices.items[0]), d=depth)

            if indices.items[1] is None:
                write(f, 'kgen_indexes(2,1) = UBOUND(%s)'%varname, d=depth)
            else:
                write(f, 'kgen_indexes(2,1) = %s'%str(indices.items[1]), d=depth)

            if indices.items[2] is None:
                write(f, 'kgen_indexes(3,1) = 1', d=depth)
            else:
                write(f, 'kgen_indexes(3,1) = %s'%str(indices.items[2]), d=depth)
        elif isinstance(indices, Section_Subscript_List):
            for i, index in enumerate(indices.items):
                if isinstance(index, Name) or isinstance(index, Scalar_Int_Expr) or \
                    isinstance(index, Data_Ref):
                    write(f, 'kgen_indexes(1,%d) = %s'%(i+1, str(index)), d=depth)
                    write(f, 'kgen_indexes(2,%d) = %s'%(i+1, str(index)), d=depth)
                    write(f, 'kgen_indexes(3,%d) = 1'%(i+1), d=depth)
                elif isinstance(index, Subscript_Triplet):
                    if index.items[0] is None:
                        write(f, 'kgen_indexes(1,%d) = LBOUND(%s)'%(i+1, varname), d=depth)
                    else:
                        write(f, 'kgen_indexes(1,%d) = %s'%(i+1, str(index.items[0])), d=depth)

                    if index.items[1] is None:
                        write(f, 'kgen_indexes(2,%d) = UBOUND(%s)'%(i+1, varname), d=depth)
                    else:
                        write(f, 'kgen_indexes(2,%d) = %s'%(i+1, str(index.items[1])), d=depth)

                    if index.items[2] is None:
                        write(f, 'kgen_indexes(3,%d) = 1'%(i+1), d=depth)
                    else:
                        write(f, 'kgen_indexes(3,%d) = %s'%(i+1, str(index.items[2])), d=depth)
            else:
                raise ProgramException('Unknown type: %s'%index.__class__)
        else:
            raise ProgramException('Unknown type: %s'%indices.__class__)

    @classmethod
    def write(cls, f, depth, block, readflag=True, varprefix='', call4arr=False, modcall4dtype=False):
        from Fortran2003 import Data_Ref, Part_Ref

        if readflag: rwstr = 'read'
        else: rwstr = 'write'

        # write non-derived type first
        for uname in block['names']:
            res_stmt = block['res_stmt'][uname]
            if not res_stmt.is_derived():
                printvar = ''
                if varprefix+uname.firstpartname() in Config.debug['printvar']:
                    printvar = varprefix+uname.firstpartname()

                var = res_stmt.parent.a.variables[uname.firstpartname()]
                # generate non-dtype write subroutine to use in call stmt
                subrname = cls.subrname_stmt(res_stmt, var, call4arr=call4arr)

                if subrname:
                    if printvar:
                        write(f, 'CALL kgen_%s_%s(%s, kgen_unit, printvar="%s")'%(rwstr, subrname, printvar, printvar), d=depth)
                    else:
                        write(f, 'CALL kgen_%s_%s(%s, kgen_unit)'%(rwstr, subrname, varprefix+uname.firstpartname()), d=depth)
                else:
                    write(f, '%s(UNIT=kgen_unit) %s'%(rwstr.upper(), varprefix+uname.firstpartname()), d=depth)
                    if printvar:
                        write(f, 'PRINT *, "******* %s *******", %s'%(printvar, printvar), d=depth)

        # and write derived type next
        for uname in block['names']:
            res_stmt = block['res_stmt'][uname]
            if res_stmt.is_derived():
                printvar = ''
                if varprefix+uname.firstpartname() in Config.debug['printvar']:
                    printvar = varprefix+uname.firstpartname()

                var = res_stmt.parent.a.variables[uname.firstpartname()]
                # generate dtype write subroutine to use in call stmt
                subrname = cls.subrname_stmt(res_stmt, var, call4arr=call4arr, modcall4dtype=modcall4dtype)
                if subrname:
                    if readflag:
                        if printvar:
                            write(f, 'CALL kgen_read_%s(%s, kgen_unit, printvar="%s")'%(subrname, printvar, printvar), d=depth)
                        else:
                            write(f, 'CALL kgen_read_%s(%s, kgen_unit)'%(subrname, varprefix+uname.firstpartname()), d=depth)
                    else:
                        written = False
                        if State.callsite['actual_arg']['names']:
                            for actual_arg in State.callsite['actual_arg']['names'].arglist:
                                arg_spec = actual_arg.arg_spec
                                if isinstance(arg_spec, Data_Ref):
                                    firstpart = arg_spec.items[0]
                                    if isinstance(firstpart, Part_Ref):
                                        argname = firstpart.items[0]
                                        if argname.string.lower()==uname.firstpartname():
                                            cls.write_dtype_arr(f, depth, firstpart.items[1], uname.firstpartname())
                                            if printvar:
                                                write(f, 'CALL kgen_write_%s(%s, kgen_unit, kgen_indexes, printvar="%s")'%(subrname, printvar, printvar), d=depth)
                                            else:
                                                write(f, 'CALL kgen_write_%s(%s, kgen_unit, kgen_indexes)'%(subrname, varprefix+uname.firstpartname()), d=depth)
                                            written = True
                                            break

                                elif isinstance(arg_spec, Part_Ref):
                                    argname = arg_spec.items[0]
                                    if argname.string.lower()==uname.firstpartname():
                                        cls.write_dtype_arr(f, depth, arg_spec.items[1], uname.firstpartname())

                                        if printvar:
                                            write(f, 'CALL kgen_write_%s(%s, kgen_unit, kgen_indexes, printvar="%s")'%(subrname, printvar, printvar), d=depth)
                                        else:
                                            write(f, 'CALL kgen_write_%s(%s, kgen_unit, kgen_indexes)'%(subrname, varprefix+uname.firstpartname()), d=depth)
                                        written = True
                                        break

                        if not written:
    
                            if printvar:
                                write(f, 'CALL kgen_write_%s(%s, kgen_unit, printvar="%s")'%(subrname, printvar, printvar), d=depth)
                            else:
                                write(f, 'CALL kgen_write_%s(%s, kgen_unit)'%(subrname, varprefix+uname.firstpartname()), d=depth)

class GenSubroutines(GenBase):

    @classmethod
    def write_specpart(cls, f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype, alloc4arr=False):
        # spec part
        attrstr = ''

        write(f, 'INTEGER, INTENT(IN) :: kgen_unit', d=depth+2)

        if readflag:
            intentstr = 'OUT'
        else:
            intentstr = 'IN'
            write(f, 'INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: indexes', d=depth+2)

        write(f, 'CHARACTER(*), INTENT(IN), OPTIONAL :: printvar', d=depth+2)

        if dtype:
            typestr = 'TYPE'
            kindstr = vartype
        else:
            typestr = vartype
            kindstr = 'KIND=%s'%varkind

        if varpointer:
            attrstr += ', POINTER'

        if varalloc or (readflag and vardim!=0 and alloc4arr):
            attrstr += ', ALLOCATABLE'

        if vardim!=0:
            vardim = abs(vardim)
            attrstr += ', DIMENSION(%s)'%','.join([':']*vardim)

        write(f, '%s(%s), INTENT(%s)%s :: var'%(typestr, kindstr, intentstr, attrstr), d=depth+2) # var decl
        write(f, 'LOGICAL :: is_true', d=depth+2) # logical var

        if vardim!=0:
            indices = [ 'idx%d'%(d+1) for d in range(vardim) ]
            write(f, 'INTEGER :: %s'%','.join(indices), d=depth+2)
            if readflag:
                write(f, 'INTEGER, DIMENSION(2,%d) :: kgen_bound'%abs(vardim), d=depth+2)
        write(f, '')

    @classmethod
    def write_checkpart(cls, f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype):
        # check part

        if readflag:
            write(f, 'READ(UNIT = kgen_unit) is_true', d=depth+2)
        else:
            if vardim==0:
                if varpointer:
                    write(f, 'is_true = ASSOCIATED(var)', d=depth+2)
                    write(f, 'WRITE(UNIT = kgen_unit) is_true', d=depth+2)
                else:
                    if dtype:
                        pass
                    else:
                        raise ProgramException('Derived type is expected')
            else:
                if varpointer:
                    write(f, 'IF ( .NOT. ASSOCIATED(var) ) THEN', d=depth+2)
                    write(f, 'is_true = .FALSE.', d=depth+3)
                    write(f, 'ELSE IF ( SIZE(var)==1 ) THEN', d=depth+2)
                else:
                    write(f, 'IF ( SIZE(var)==1 ) THEN', d=depth+2)
                write(f, 'IF ( UBOUND(var, 1)<LBOUND(var, 1) ) THEN', d=depth+3)
                write(f, 'is_true = .FALSE.', d=depth+4)
                write(f, 'ELSE IF ( UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0 ) THEN', d=depth+3)
                write(f, 'is_true = .FALSE.', d=depth+4)
                write(f, 'ELSE', d=depth+3)
                write(f, 'is_true = .TRUE.', d=depth+4)
                write(f, 'END IF', d=depth+3)
                write(f, 'ELSE', d=depth+2)
                write(f, 'is_true = .TRUE.', d=depth+3)
                write(f, 'END IF', d=depth+2)
                write(f, 'WRITE(UNIT = kgen_unit) is_true', d=depth+2)
        write(f, '')


    @classmethod
    def write_rwpart(cls, f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype, alloc4arr=False):
        import re

        # read/write part

        write(f, 'IF ( is_true ) THEN', d=depth+2)

        # generating subroutine name to use in call stmt
        subrname = cls.subrname_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, modname=True)

        if readflag:

            if vardim==0:
                if varalloc or varpointer:
                    write(f, 'ALLOCATE(var)', d=depth+3)

                if dtype:
                    subrname = re.sub(r'_ptr', '', subrname)
                    subrname = re.sub(r'_alloc', '', subrname)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'CALL kgen_read_%s(var, kgen_unit, printvar=printvar//"%%%s")'%(subrname, dtype.name), d=depth+4)
                    write(f, 'ELSE', depth+3)
                    write(f, 'CALL kgen_read_%s(var, kgen_unit)'%subrname, depth+4)
                    write(f, 'END IF', depth+3)
                else:
                    write(f, 'READ(UNIT = kgen_unit) var', d=depth+3)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'PRINT *, "** " // printvar // " **", var', d=depth+4)
                    write(f, 'END IF', depth+3)
            else:
                shape = []
                for dim in range(abs(vardim)):
                    write(f, 'READ(UNIT = kgen_unit) kgen_bound(1, %d)'%(dim+1), d=depth+3)
                    write(f, 'READ(UNIT = kgen_unit) kgen_bound(2, %d)'%(dim+1), d=depth+3) 
                    shape.append('kgen_bound(2, %d) - kgen_bound(1, %d) + 1'%(dim+1, dim+1))
                if alloc4arr or varalloc or varpointer:
                    write(f, 'ALLOCATE(var(%s))'%(', '.join(shape)), d=depth+3)

                vardim = abs(vardim)

                if dtype:

                    idx = [ 'idx%d'%(d+1) for d in range(vardim) ]

                    # open nested DO loops
                    for d in range(vardim):
                        write(f, 'DO idx%(d)d=kgen_bound(1,%(d)d), kgen_bound(2, %(d)d)'%{'d':d+1}, d=depth+d+3)

                    # generating read subroutine name to use incall stmt
                    subrname = cls.subrname_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=True, modname=True)
                    subrname = re.sub(r'_dim\d+', '', subrname)
                    scalar_subrname = re.sub(r'_ptr', '', subrname)
                    scalar_subrname = re.sub(r'_alloc', '', scalar_subrname)

                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'CALL kgen_read_%s(var(%s), kgen_unit, printvar=printvar)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+4)
                    write(f, 'ELSE', depth+3)
                    write(f, 'CALL kgen_read_%s(var(%s), kgen_unit)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+4)
                    write(f, 'END IF', depth+3)

                    # close nested DO loops
                    for d in reversed(range(vardim)):
                        write(f, 'END DO', d=depth+d+3)

                else:
                    write(f, 'READ(UNIT = kgen_unit) var', d=depth+3)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'PRINT *, "** " // printvar // " **", var', d=depth+4)
                    write(f, 'END IF', depth+3)

        else:
            if vardim==0:
                if dtype:

                    subrname = re.sub(r'_ptr', '', subrname)
                    subrname = re.sub(r'_alloc', '', subrname)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'CALL kgen_write_%s(var, kgen_unit, printvar=printvar)'%subrname, depth+4)
                    write(f, 'ELSE', depth+3)
                    write(f, 'CALL kgen_write_%s(var, kgen_unit)'%subrname, depth+4)
                    write(f, 'END IF', depth+3)
                else:
                    write(f, 'READ(UNIT = kgen_unit) var', d=depth+3)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', depth+3)
                    write(f, 'PRINT *, "** " // printvar // " **", var', d=depth+4)
                    write(f, 'END IF', depth+3)
            else:
                vardim = abs(vardim)

                write(f, 'IF ( PRESENT(indexes) ) THEN', d=depth+3)

                subscripts = []
                for dim in range(vardim):
                    write(f, 'WRITE(UNIT = kgen_unit) indexes(1,%d)'%(dim+1), d=depth+4)
                    write(f, 'WRITE(UNIT = kgen_unit) indexes(2,%d)'%(dim+1), d=depth+4)
                    subscripts.append('indexes(1,%d):indexes(2,%d)'%(dim+1, dim+1))

                if dtype:

                    idx = [ 'idx%d'%(d+1) for d in range(vardim) ]

                    # open nested DO loops
                    for d in range(vardim):
                        write(f, 'DO idx%(d)d=indexes(1,%(d)d), indexes(2,%(d)d)'%{'d':d+1}, d=depth+d+4)

                    # generating write dtype subroutine name to use in call stmt
                    subrname = cls.subrname_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=True, modname=True)
                    subrname = re.sub(r'_dim\d+', '', subrname)
                    scalar_subrname = re.sub(r'_ptr', '', subrname)
                    scalar_subrname = re.sub(r'_alloc', '', scalar_subrname)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+vardim+4)
                    write(f, 'CALL kgen_write_%s(var(%s), kgen_unit, printvar=printvar)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+5)
                    write(f, 'ELSE', d=depth+vardim+4)
                    write(f, 'CALL kgen_write_%s(var(%s), kgen_unit)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+5)
                    write(f, 'END IF', d=depth+vardim+4)

                    # close nested DO loops
                    for d in reversed(range(vardim)):
                        write(f, 'END DO', d=depth+d+4)

                else:
                    write(f, 'WRITE(UNIT = kgen_unit) var(%s)'%','.join(subscripts), d=depth+4)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+4)
                    write(f, 'PRINT *, "** " // printvar // &', d=depth+5)
                    write(f, '&"(%s) **", &'%','.join(subscripts), d=depth+5)
                    write(f, '&var(%s)'%','.join(subscripts), d=depth+5)
                    write(f, 'END IF', d=depth+4)

                write(f, 'ELSE', d=depth+3)

                for dim in range(vardim):
                    write(f, 'WRITE(UNIT = kgen_unit) LBOUND(var, %d)'%(dim+1), d=depth+4) 
                    write(f, 'WRITE(UNIT = kgen_unit) UBOUND(var, %d)'%(dim+1), d=depth+4) 

                if dtype:

                    idx = [ 'idx%d'%(d+1) for d in range(vardim) ]

                    # open nested DO loops
                    for d in range(vardim):
                        write(f, 'DO idx%(d)d=LBOUND(var,%(d)d), UBOUND(var, %(d)d)'%{'d':d+1}, d=depth+d+4)

                    # generating write dtyp subroutine name to use in call stmt
                    subrname = cls.subrname_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, is_elem=True, modname=True)
                    subrname = re.sub(r'_dim\d+', '', subrname)
                    scalar_subrname = re.sub(r'_ptr', '', subrname)
                    scalar_subrname = re.sub(r'_alloc', '', scalar_subrname)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+vardim+4)
                    write(f, 'CALL kgen_write_%s(var(%s), kgen_unit, printvar=printvar)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+5)
                    write(f, 'ELSE', d=depth+vardim+4)
                    write(f, 'CALL kgen_write_%s(var(%s), kgen_unit)'%(scalar_subrname, ','.join(idx)), d=depth+vardim+5)
                    write(f, 'END IF', d=depth+vardim+4)
                    # close nested DO loops
                    for d in reversed(range(vardim)):
                        write(f, 'END DO', d=depth+d+4)

                else:
                    write(f, 'WRITE(UNIT = kgen_unit) var', d=depth+4)
                    write(f, 'IF ( PRESENT(printvar) ) THEN', d=depth+4)
                    write(f, 'PRINT *, "** " // printvar // " **", var', d=depth+5)
                    write(f, 'END IF', d=depth+4)

                write(f, 'END IF', d=depth+3)

        write(f, 'END IF', d=depth+2)

    @classmethod
    def write(cls, f, depth, tkdpatlist, readflag=False, contains=False):
        if readflag: rwstr = 'read'
        else: rwstr = 'write'
        contains_stmt_written = False

        subrnames = []

        for i, (loctype, vartype, varkind, vardim, varpointer, varalloc, dtype) in enumerate(tkdpatlist):

            call4arr = False
            if loctype==State.KD_INPUT or loctype==State.PB_OUTPUT:
                call4arr = True

            # generate subroutine name for generating subroutines to call
            subrname = cls.subrname_tkdpat(vartype, varkind, vardim, varpointer, varalloc, dtype, modname=False, call4arr=call4arr)
            if subrname:
                if subrname in subrnames:
                    continue
                else:
                    subrnames.append(subrname)
                
            if subrname:
                if dtype and vardim==0 and not varpointer and not varalloc: 
                    continue
#                    if vardim>0:
#                        if not call4arr and not varpointer and not varalloc: continue
#                    else:
#                        if not varpointer and not varalloc: 
#                            continue

                if contains and contains_stmt_written:
                    write(f, 'CONTAINS', d=depth)
                    contains_stmt_written = True

                if readflag:
                    write(f, 'SUBROUTINE kgen_%s_%s(var, kgen_unit, printvar)'%(rwstr, subrname), d=depth+1)
                else:
                    write(f, 'SUBROUTINE kgen_%s_%s(var, kgen_unit, indexes, printvar)'%(rwstr, subrname), d=depth+1)

                cls.write_specpart(f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype, alloc4arr=call4arr)
                cls.write_checkpart(f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype)
                cls.write_rwpart(f, depth, readflag, vartype, varkind, vardim, varpointer, varalloc, dtype, alloc4arr=call4arr)

                write(f, 'END SUBROUTINE kgen_%s_%s'%(rwstr, subrname), d=depth+1)
                write(f, '')

###############################################################################
# KERNEL
###############################################################################

def write_kernel_stmt(f, stmt, depth, **kwargs):
    if isinstance(stmt, Comment):
        if hasattr(stmt.parent, 'geninfo'):
            write_stmt(f, stmt, depth, genonly=True, **kwargs)
    else:
        write_stmt(f, stmt, depth, genonly=True, **kwargs)

def write_kernel_read_inputs(f, depth):
    if len(State.parentblock['input']['names'])==0:
        write(f, '! Not kernel driver input', d=depth)
    else:
        GenIOStmt.write(f, depth, State.parentblock['input'], readflag=True)

    write(f, '')


def write_kernel_read_outputs(f, depth):
    if len(State.parentblock['output']['names'])==0:
        write(f, '! No parent output var', d=depth)
    else:
        GenIOStmt.write(f, depth, State.parentblock['output'], readflag=True, varprefix='ref_')

    write(f, '')

def write_kernel_verify_outputs(f, depth):
    write(f, '! kernel verification for output variables', d=depth)
    for uname in State.parentblock['output']['names']:
        res_stmt = State.parentblock['output']['res_stmt'][uname]
        n = uname.firstpartname()
        var = res_stmt.parent.a.variables[n]
        subrname = GenBase.subrname_stmt(res_stmt, var, call4arr=True, subr4kind=True)
        if subrname:
            write(f, 'CALL kgen_verify_%s( "%s", check_status, %s, ref_%s)'%(subrname, n, n, n), d=depth)
        else:
            write(f, 'CALL kgen_verify_%s( "%s", check_status, %s, ref_%s)'%(res_stmt.name, n, n, n), d=depth)

def _recursive_write(f, depth, stmt, used):

    if hasattr(stmt, 'unknowns'):
        for uname, res in stmt.unknowns.iteritems():
            strname = uname.firstpartname()
            res_stmt = res.res_stmt
            if not strname in used and res_stmt:
                if isinstance(res_stmt, Use):
                    if strname in res_stmt.norenames:
                        write_stmt(f, res_stmt, depth, items=[strname])
                    else:
                        for (n,o) in res_stmt.renames:
                            if strname==n:
                                write_stmt(f, res_stmt, depth, items=['%s => %s'%(n,o)])
                                break
                    used.append(strname)
                elif isinstance(res_stmt, TypeDeclarationStatement):
                    if 'parameter' in res_stmt.attrspec:
                        write_stmt(f, res_stmt, depth, items=[strname])
                        used.append(strname)
                        _recursive_write(f, depth, res_stmt, used)
                elif isinstance(res_stmt, Type):
                    write(f, 'USE %s, only : %s'%(res_stmt.parent.name, res_stmt.name), d=depth)
                else: raise ProgramException('Not implemented: %s'%res_stmt.__class__)

def write_driver_usepart_kgen(f, depth):

    write(f, 'USE %s, ONLY : %s'%(State.topblock['stmt'].name, State.parentblock['stmt'].name), d=depth)

    # collect names and resolutions to resolve
    used = []
    for uname, res_stmt in State.kernel_driver['input']['res_stmt'].iteritems():
        _recursive_write(f, depth, res_stmt, used)

    # use stmts for write_externs for topblock
    if len(State.topblock['extern']['names'])>0:
        mod_name = State.topblock['stmt'].name
        write(f, 'USE %s, ONLY : kgen_read_externs_%s'%(mod_name, mod_name), d=depth)

    # use stmts for write_externs for modules
    for mod_name, params in State.modules.iteritems():
        if len(params['extern']['names'])>0:
            write(f, 'USE %s, ONLY : kgen_read_externs_%s'%(mod_name, mod_name), d=depth)

    # use stmts for driver inputs
    for loctype, vartype, varkind, vardim, varpointer, varalloc, dtype in State.kernel_driver['input']['tkdpat']:
        if dtype and not isinstance(dtype, Use):
            write(f, 'USE %s, ONLY : kgen_read_mod0 => kgen_read'%State.topblock['stmt'].name, d=depth)

    write_rw_usepart_module(f, depth, True, State.kernel_driver)

def write_driver_specpart(f, depth):
    write(f, '')
    if Config.mpi['enabled']:
        write(f, 'INTEGER :: kgen_mpi_rank', d=depth)
        write(f, 'CHARACTER(LEN=16) ::kgen_mpi_rank_conv', d=depth)
        line = 'INTEGER, DIMENSION(%s), PARAMETER :: kgen_mpi_rank_at = (/ %s /)'
        write(f, line % ( Config.mpi['size'], ', '.join(Config.mpi['ranks']) ), d=depth)

    write(f, 'INTEGER :: kgen_ierr, kgen_unit', d=depth)
    write(f, 'INTEGER :: kgen_repeat_counter', d=depth)
    write(f, 'INTEGER :: kgen_counter', d=depth)
    write(f, 'CHARACTER(LEN=16) :: kgen_counter_conv', d=depth)
    line = 'INTEGER, DIMENSION(%s), PARAMETER :: kgen_counter_at = (/ %s /)'
    write(f, line  % ( Config.ordinal['size'], ', '.join(Config.ordinal['numbers']) ), d=depth)
    write(f, 'CHARACTER(LEN=1024) :: kgen_filepath', d=depth)

    # write typececls for dummy args
    for uname, res_stmt in State.kernel_driver['input']['res_stmt'].iteritems():
        var = res_stmt.parent.a.variables[uname.firstpartname()]
        dimattr = []
        for attr in var.parent.attrspec:
            if attr.startswith('dimension'):
                dimattr.append(attr)
        delattrs = ['intent(in)', 'intent(out)', 'intent(inout)'] + dimattr
        if var.is_array():
            dim = '(%s)'%','.join([':']*var.rank)
            write_kernel_stmt(f, res_stmt, depth, items=[ uname.firstpartname()+dim ], delattr=delattrs, \
                addattr=['allocatable'])
        else:
            write_kernel_stmt(f, res_stmt, depth, items=[ uname.firstpartname() ], delattr=delattrs)

def write_kernel_interface_read_var_mod(f, depth, tkdpatlist, mod_num, dtypelist=None):
    write_interface_rw_var(f, depth, True, tkdpatlist, mod_num, dtypelist)

def write_driver_read_fileopen(f, depth):
    write(f, '')
    if Config.mpi['enabled']:
        len = Config.mpi['size'] * Config.ordinal['size']
    else:
        len = Config.ordinal['size']

    write(f, 'DO kgen_repeat_counter = 0, %d'%(len-1), d=depth)
    write(f, '    kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, %d)+1)'%Config.ordinal['size'], d=depth)
    write(f, '    WRITE( kgen_counter_conv, * ) kgen_counter', d=depth)

    if Config.mpi['enabled']:
        write(f, '    kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, %d)+1)'%Config.mpi['size'], d=depth)
        write(f, '    WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank', d=depth)
        line = '    kgen_filepath = "%s." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))'
        write(f, line % ('./'+Config.callsite['subpname'].firstpartname()), d=depth)
    else:
        line = '    kgen_filepath = "%s." // trim(adjustl(kgen_counter_conv))'
        write(f, line % ('./'+Config.callsite['subpname'].firstpartname()), d=depth)

    write(f, '    kgen_unit = kgen_get_newunit()', d=depth)
    write(f, '    OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")', d=depth)
    write(f, '    WRITE (*,*)', d=depth)
    write(f, '    IF ( kgen_ierr /= 0 ) THEN', d=depth)
    write(f, '        CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )', d=depth)
    write(f, '    END IF', d=depth)
    write(f, '    WRITE (*,*)', d=depth)
    write(f, '    WRITE (*,*) "** Verification against \'" // trim(adjustl(kgen_filepath)) // "\' **"', d=depth)

def write_driver_call_module_externs(f, depth):
    write(f, '')

    # call to callsite file if exists
    if len(State.topblock['extern']['names'])>0:
        write(f, 'CALL kgen_read_externs_%s(kgen_unit)'%State.topblock['stmt'].name, d=depth)

    # call to module files if exists
    for mod_name, params in State.modules.iteritems():
        if len(params['extern']['names'])>0:
            write(f, 'CALL kgen_read_externs_%s(kgen_unit)'%mod_name, d=depth)

def write_driver_read_fileclose(f, depth):
    write(f, '')
    write(f, '    CLOSE (UNIT=kgen_unit)', d=depth)
    write(f, 'END DO', d=depth)

def write_kernel_subroutine_module_externs(f, depth, block, mod_name):

    if len(block['names'])==0:
        write(f, '! No module extern variables', d=depth)
    else:
        write(f, '! module extern variables', d=depth, pren=True)
        write(f, 'SUBROUTINE kgen_read_externs_%s(kgen_unit)'%mod_name, d=depth, pren=True)
        write(f, 'INTEGER, INTENT(IN) :: kgen_unit', d=depth+1)
        GenIOStmt.write(f, depth+1, block, readflag=True)
        write(f, 'END SUBROUTINE kgen_read_externs_%s'%mod_name, d=depth, postn=True)

def write_kernel_subroutines_read_var(f, depth, tkdpatlist, contains=False):
    write(f, '! write subroutines', d=depth, pren=True)

    if len(tkdpatlist)==0:
        write(f, '! No subroutines', d=depth)
    else:
        GenSubroutines.write(f, depth, tkdpatlist, readflag=True, contains=contains)

def write_kernel_subroutines_type_verify_var(f, depth, dtypelist):
    global verify_subrnames
    verify_subrnames = []

    tempblock = {}
    tempblock['names'] = []
    tempblock['res_stmt'] = {}

    for dtype in dtypelist:
        write(f, 'SUBROUTINE kgen_verify_%s(varname, check_status, var, ref_var)'%dtype.name, d=depth)
        write(f, 'CHARACTER(*), INTENT(IN) :: varname', d=depth+1)
        write(f, 'TYPE(check_t), INTENT(INOUT) :: check_status', d=depth+1)
        write(f, 'TYPE(check_t) :: dtype_check_status', d=depth+1)
        write(f, 'TYPE(%s), INTENT(IN) :: var, ref_var'%dtype.name, d=depth+1)


        write(f, '')
        write(f, 'check_status%numTotal = check_status%numTotal + 1', d=depth+1)
        write(f, 'CALL kgen_init_check(dtype_check_status)', d=depth+1)

        #import pdb; pdb.set_trace()
        for comp, _depth in walk(dtype):
            if isinstance(comp, TypeDeclarationStatement):
                # any one of decls
                decl = Entity_Decl(comp.entity_decls[0])
                uname = KGName(decl.items[0].string)
                var = comp.parent.a.variables[uname.firstpartname()]
                subrname = GenBase.subrname_stmt(comp, var, call4arr=True, subr4kind=True)

                if not subrname:
                    subrname = comp.name

                for entity in comp.entity_decls:
                    decl = Entity_Decl(entity)
                    uname = KGName(decl.items[0].string)
                    n = uname.firstpartname()

                    write(f, 'CALL kgen_verify_%s("%s", dtype_check_status, var%%%s, ref_var%%%s)'%(subrname, n, n, n), d=depth+1)

                if subrname in verify_subrnames:
                    continue

                tempblock['names'].append(uname)
                tempblock['res_stmt'][uname] = comp

        verify_subrnames.append(dtype.name)

        write(f, 'IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN', d=depth+1)
        write(f, '    check_status%numIdentical = check_status%numIdentical + 1', d=depth+1)
        write(f, 'ELSE IF ( dtype_check_status%numFatal > 0 ) THEN', d=depth+1)
        write(f, '    check_status%numFatal = check_status%numFatal + 1', d=depth+1)
        write(f, 'ELSE IF ( dtype_check_status%numWarning > 0 ) THEN', d=depth+1)
        write(f, '    check_status%numWarning = check_status%numWarning + 1', d=depth+1)
        write(f, 'END IF', d=depth+1)

        write(f, 'END SUBROUTINE', d=depth)

    GenVerification.write(f, depth, srcblock=tempblock)

def write_kernel_subroutines_verify_var(f, depth, tkdpatlist):
    write(f, '! verify subroutines', d=depth, pren=True)

    if len(tkdpatlist)==0:
        write(f, '! No verification', d=depth)
    else:
        GenVerification.write(f, depth)

def write_subroutines_kgen(f, depth):
    line = ''.join(kgen_subprograms)
    write(f, line.replace('\n', '\n%s'%(TAB*depth)), d=depth)
    write(f, '')

def write_kernel_timing(f, depth):
    from statements import Assignment

    write(f, 'CALL system_clock(start_clock, rate_clock)', d=depth)
    #write(f, 'start_clock = rdtsc()', d=depth)
    #write(f, 'DO kgen_intvar=1,%d'%Config.timing['repeat'], d=depth)
    write(f, 'DO kgen_intvar=1,maxiter', d=depth)
    expr = State.callsite['expr']
    if isinstance(State.callsite['stmt'], Assignment):
        lhs = State.callsite['stmt'].f2003.items[0]
        write(f, '%s = %s'%(lhs.tofortran().strip(), expr.tofortran().strip()), d=depth+1)
    else:
        write(f, expr.tofortran().strip(), d=depth+1)
    write(f, 'END DO', d=depth)
    #write(f, 'stop_clock = rdtsc()', d=depth)
    write(f, 'CALL system_clock(stop_clock, rate_clock)', d=depth)
    write(f, 'WRITE(*,*)', d=depth)
    write(f, 'PRINT *, "%s : Time per call (usec): ", 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)'%\
        Config.callsite['subpname'].firstpartname(), d=depth)
    #write(f, 'PRINT *, "Elapsed clock (cycles): ", (stop_clock - start_clock)/%d'% \
    #    Config.timing['repeat'], d=depth)

def write_kernel_pertcalls(f, depth):
    from typedecl_statements import Real
 
    # perturbation tests
    for uname in State.parentblock['input']['names']:
        if uname.firstpartname() in Config.check['pert_invar']:
            res_stmt = State.parentblock['input']['res_stmt'][uname]
            n = uname.firstpartname()
            var = res_stmt.parent.a.variables[n]

            if isinstance(res_stmt, Real) and var.is_array():
                subrname = GenBase.subrname_stmt(res_stmt, var, call4arr=True, subr4kind=True)
                if subrname:
                    write(f, '!Uncomment following call(s) to generate perturbed input(s)', d=depth)
                    write(f, '!CALL kgen_perturb_%s( %s )'%(subrname, n), d=depth)
            else:
                logger.warn('%s is not a Real variable or not an arrray.'%n)
    write(f, '')

def write_file_header(f, filename):
    from time import strftime
    kgenversion = '%d.%d.%s'%tuple(Config.kgen['version'])
    datetime = strftime("%Y-%m-%d %H:%M:%S")
    write(f, kgen_file_header%(filename, datetime, kgenversion))


def write_kernel_subroutines_type_write_var(f, depth, dtypelist):
    write_subroutines_type_rw_var(f, depth, True, dtypelist)

def write_kgen_utils_file(f):
    write(f, 'module kgen_utils_mod', d=0)
    write(f, kgen_utils_file_head, d=0)
    write(f, 'contains', d=0)
    write(f, kgen_utils_file_checksubr, d=0)
    write(f, 'end module', d=0)

def write_kernel_subroutines_perturb_var(f, depth):
    from typedecl_statements import Real
 
    # perturbation tests
    for uname in State.parentblock['input']['names']:
        if uname.firstpartname() in Config.check['pert_invar']:
            res_stmt = State.parentblock['input']['res_stmt'][uname]
            n = uname.firstpartname()
            var = res_stmt.parent.a.variables[n]

            if isinstance(res_stmt, Real) and var.is_array():
                subrname = GenBase.subrname_stmt(res_stmt, var, call4arr=True, subr4kind=True)
                if subrname:
                    write(f, 'subroutine kgen_perturb_%s( var )'%subrname, d=depth)

                    kindstr = res_stmt.get_kind()
                    if isinstance(kindstr, int): kindstr = str(kindstr)
                    dimstr = ','.join([':']*var.rank)

                    write(f, 'real(kind=%s), intent(inout), dimension(%s) :: var'%(kindstr, dimstr), d=depth+1)
                    write(f, 'integer, allocatable :: rndm_seed(:)', d=depth+1)
                    write(f, 'integer :: rndm_seed_sz', d=depth+1)
                    write(f, 'real(kind=%s) :: pertval'%kindstr, d=depth+1)
                    write(f, 'real(kind=%s) :: pertlim = %s'%(kindstr, Config.check['pert_lim']), d=depth+1)

                    idxstr = ','.join([ 'idx%d'%(d+1) for d in range(var.rank) ])

                    write(f, 'integer :: %s'%idxstr, d=depth+1)
                    write(f, '', d=depth)
                    write(f, 'call random_seed(size=rndm_seed_sz)', d=depth+1)
                    write(f, 'allocate(rndm_seed(rndm_seed_sz))', d=depth+1)
                    write(f, 'rndm_seed = 121869', d=depth+1)
                    write(f, 'call random_seed(put=rndm_seed)', d=depth+1)

                    for d in range(var.rank):
                        write(f, 'do idx%d=1,size(var, dim=%d)'%(d+1, d+1), d=depth+d+1)

                    write(f, 'call random_number(pertval)', d=depth+var.rank+1)
                    write(f, 'pertval = 2.0_%s*pertlim*(0.5_%s - pertval)'%(kindstr, kindstr), d=depth+var.rank+1)
                    write(f, 'var(%s) = var(%s)*(1.0_%s + pertval)'%(idxstr, idxstr, kindstr), d=depth+var.rank+1)

                    for d in range(var.rank):
                        write(f, 'end do', d=depth+var.rank-d)

                    write(f, 'deallocate(rndm_seed)', d=depth+1)
                    write(f, 'end subroutine', d=depth)


def write_kernel_usepart_module(f, depth, module):
    write_rw_usepart_module(f, depth, True, module)

def write_kernel_usepart_callsite(f, depth):
    write_rw_usepart_module(f, depth, True, State.topblock)

def write_kernel_driver_inputs(f, depth):
    write(f, '! driver variables', d=depth, pren=True)
    if len(State.kernel_driver['input']['names'])==0:
        write(f, '! Not kernel driver input', d=depth)
    else:
        GenIOStmt.write(f, depth, State.kernel_driver['input'], readflag=True, call4arr=True, modcall4dtype=True)

    write(f, '')


###############################################################################
# STATE
###############################################################################

def write_state_usepart_callsite(f, depth):
    # use stmt for MPI if enabled
    if Config.mpi['enabled'] and Config.mpi['use_stmts']:
        for use_stmt in Config.mpi['use_stmts']:
            write(f, use_stmt, d=depth)
 
    # use stmts for write_externs
    for mod_name, params in State.modules.iteritems():
        if len(params['extern']['names'])>0:
            write(f, 'USE %s, only : kgen_write_externs_%s'%(mod_name, mod_name), d=depth)

    # use stmts for type write_var
    write_rw_usepart_module(f, depth, False, State.topblock)


def write_state_interface_write_var_mod(f, depth, tkdpatlist, mod_num, dtypelist=None):
    write_interface_rw_var(f, depth, False, tkdpatlist, mod_num, dtypelist)

def write_state_subroutines_write_var(f, depth, tkdpatlist, contains=False):
    write(f, '! write subroutines', d=depth, pren=True)

    if len(tkdpatlist)==0:
        write(f, '! No subroutines', d=depth)
    else:
        GenSubroutines.write(f, depth, tkdpatlist, readflag=False, contains=contains)

def write_state_specpart_kgen(f, depth):
    if Config.mpi['enabled']:
        write(f, 'INTEGER :: kgen_mpi_rank, kgen_mpi_size, kgen_cur_rank', d=depth)
        write(f, 'CHARACTER(LEN=16) ::kgen_mpi_rank_conv', d=depth)
        line = 'INTEGER, DIMENSION(%s), PARAMETER :: kgen_mpi_rank_at = (/ %s /)'
        write(f, line % ( Config.mpi['size'], ', '.join(Config.mpi['ranks']) ), d=depth)
    
    write(f, 'INTEGER :: kgen_ierr, kgen_unit', d=depth)
    write(f, 'INTEGER, DIMENSION(3,10) :: kgen_indexes', d=depth)
    write(f, 'INTEGER, SAVE :: kgen_counter = 1', d=depth)
    #write(f, 'LOGICAL, SAVE :: kgen_entered1 = .FALSE., kgen_entered2 = .FALSE.', d=depth)
    write(f, 'CHARACTER(LEN=16) :: kgen_counter_conv', d=depth)
    line = 'INTEGER, DIMENSION(%s), PARAMETER :: kgen_counter_at = (/ %s /)'
    write(f, line % ( Config.ordinal['size'], ', '.join(Config.ordinal['numbers']) ), d=depth)
    write(f, 'CHARACTER(LEN=1024) :: kgen_filepath', d=depth)

def write_state_call_module_externs(f, depth):
    # call to callsite file if exists
    if len(State.topblock['extern']['names'])>0:
        write(f, 'CALL kgen_write_externs_%s(kgen_unit)'%State.topblock['stmt'].name, d=depth)

    # call to module files if exists
    for mod_name, params in State.modules.iteritems():
        if len(params['extern']['names'])>0:
            write(f, 'CALL kgen_write_externs_%s(kgen_unit)'%mod_name, d=depth)

def write_state_write_inputs(f, depth):
    write(f, '! input variables for the parent of callsite', d=depth, pren=True)
    #write_rw_inputs(f, depth, False)

    if len(State.parentblock['input']['names'])==0:
        write(f, '! No parent input var', d=depth)
    else:
        GenIOStmt.write(f, depth, State.parentblock['input'], readflag=False)

    write(f, '')

def write_state_write_outputs(f, depth):
    write(f, '! output variables for the parent of callsite', d=depth, pren=True)

    if len(State.parentblock['output']['names'])==0:
        write(f, '! No parent output var', d=depth)
    else:
        GenIOStmt.write(f, depth, State.parentblock['output'], readflag=False)

    write(f, '')

def write_state_write_driver_inputs(f, depth):
    write(f, '! driver variables', d=depth, pren=True)

    if len(State.kernel_driver['input']['names'])==0:
        write(f, '! Not kernel driver input', d=depth)
    else:
        GenIOStmt.write(f, depth, State.kernel_driver['input'], readflag=False, call4arr=True)

def write_state_print_counter(f, depth):
    write(f, kgen_print_counter.replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)

def write_state_write_fileopen(f, depth):
    write(f, '!$OMP MASTER', d=depth, pren=True)
    if Config.mpi['enabled']:
        write(f, 'CALL mpi_comm_rank ( %s, kgen_mpi_rank, kgen_ierr )'%Config.mpi['comm'], d=depth)
        write(f, 'IF ( kgen_ierr /= 0 ) THEN', d=depth)
        write(f, '    CALL kgen_error_stop( "MPI ERROR" )', d=depth)
        write(f, 'END IF', d=depth)
        write(f, 'CALL mpi_comm_size ( %s, kgen_mpi_size, kgen_ierr )'%Config.mpi['comm'], d=depth)
        write(f, 'IF ( kgen_ierr /= 0 ) THEN', d=depth)
        write(f, '    CALL kgen_error_stop( "MPI ERROR" )', d=depth)
        write(f, 'END IF', d=depth)
        write(f, 'kgen_cur_rank = 0', d=depth)
        write(f, 'kgen_unit = -1', d=depth)
        write(f, 'DO WHILE(kgen_cur_rank < kgen_mpi_size)', d=depth)
        write(f, '    IF ( ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank ) THEN', d=depth)
        write(f, '        IF ( ANY(kgen_counter == kgen_counter_at) ) THEN', d=depth)
        write(f, '            WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank', d=depth)
        write(f, '            WRITE( kgen_counter_conv, * ) kgen_counter', d=depth)
        line = '            kgen_filepath = "%s." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))'
        write(f, line % (os.path.abspath(Config.path['kernel'])+'/'+Config.callsite['subpname'].firstpartname()), d=depth)
    else:
        write(f, 'kgen_unit = -1', d=depth)
        write(f, 'IF ( ANY(kgen_counter == kgen_counter_at) ) THEN', d=depth)
        write(f, '    WRITE( kgen_counter_conv, * ) kgen_counter', d=depth)
        line = '    kgen_filepath = "%s." // TRIM(ADJUSTL(kgen_counter_conv))'
        write(f, line % (os.path.abspath(Config.path['kernel'])+'/'+Config.callsite['subpname'].firstpartname()), d=depth)

    write(f, '        kgen_unit = kgen_get_newunit()', d=depth)
    write(f, '        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="REPLACE", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")', d=depth)
    write(f, '        IF ( kgen_ierr /= 0 ) THEN', d=depth)
    write(f, '            CALL kgen_error_stop( "FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)) )', d=depth)
    write(f, '        END IF', d=depth)

    if Config.mpi['enabled']:
        write(f, '        CALL kgen_print_mpirank_counter(kgen_mpi_rank, kgen_counter)', d=depth)
    else:
        write(f, '        CALL kgen_print_counter(kgen_counter)', d=depth)

def write_state_fileopen_tail(f, depth):
    if Config.mpi['enabled']:
        write(f, '        END IF', d=depth-1)
        write(f, '    END IF', d=depth-1)
        write(f, '    kgen_cur_rank = kgen_cur_rank + 1', d=depth-1)
        #write(f, '    call mpi_barrier( %s, kgen_ierr )'%Config.mpi['comm'], d=depth-1)
        write(f, 'END DO', d=depth-1)
    else:
        write(f, 'END IF', d=depth-1)
    write(f, '!$OMP END MASTER', d=depth-1)
    #write(f, '!$OMP BARRIER', d=depth-1, postn=True)

def write_state_fileclose_head(f, depth):
    #write(f, '!$OMP BARRIER', d=depth, pren=True)
    write(f, '!$OMP MASTER', d=depth)
    if Config.mpi['enabled']:
        write(f, 'kgen_cur_rank = 0', d=depth)
        write(f, 'DO WHILE(kgen_cur_rank < kgen_mpi_size)', d=depth)
        write(f, '    IF ( ANY(kgen_mpi_rank == kgen_mpi_rank_at) .AND. kgen_cur_rank == kgen_mpi_rank ) THEN', d=depth)
        write(f, '        IF ( ANY(kgen_counter == kgen_counter_at) ) THEN', d=depth)
        write(f, '            PRINT *, "KGEN writes output state variables at count = ", kgen_counter, " on mpirank = ", kgen_mpi_rank', d=depth)
    else:
        write(f, 'IF ( ANY(kgen_counter == kgen_counter_at) ) THEN', d=depth)

def write_state_write_fileclose(f, depth):
    if Config.mpi['enabled']:
        write(f, '            ENDFILE kgen_unit', d=depth)
        write(f, '            CALL sleep(1)', d=depth)
        write(f, '            CLOSE (UNIT=kgen_unit)', d=depth)
        write(f, '        END IF', d=depth)
        write(f, '    END IF', d=depth)
        write(f, '    kgen_cur_rank = kgen_cur_rank + 1', d=depth)
        #write(f, '    CALL mpi_barrier( %s, kgen_ierr )'%Config.mpi['comm'], d=depth)
        write(f, 'END DO', d=depth)
        write(f, 'IF ( kgen_counter > maxval(kgen_counter_at) ) THEN', d=depth)
        write(f, '    CALL sleep(2)', d=depth)
        write(f, '    PRINT *, "All state data is collected.  Stopping program..."', d=depth)
        write(f, '    CALL mpi_abort( %s, 1, kgen_ierr)'%Config.mpi['comm'], d=depth)
        write(f, 'ELSE', d=depth)
        write(f, '    PRINT *, "kgen_counter = ", kgen_counter, " at rank ", kgen_mpi_rank', d=depth)
        write(f, 'END IF', d=depth)
        write(f, 'kgen_counter = kgen_counter + 1', d=depth)
        write(f, '!$OMP END MASTER', d=depth)
    else:
        write(f, '    ENDFILE kgen_unit', d=depth-1)
        write(f, '    CALL sleep(1)', d=depth-1)
        write(f, '    CLOSE (UNIT=kgen_unit)', d=depth-1)
        write(f, 'END IF', d=depth-1)
        write(f, 'PRINT *, "kgen_counter = ", kgen_counter', d=depth-1)
        write(f, 'IF ( kgen_counter > maxval(kgen_counter_at) ) THEN', d=depth-1)
        write(f, '    CALL sleep(2)', d=depth)
        write(f, '    PRINT *, "kgen_counter is larger than maximum counter. Exit program..."', d=depth-1)
        write(f, '    STOP', d=depth-1)
        write(f, 'END IF', d=depth-1)
        write(f, 'kgen_counter = kgen_counter + 1', d=depth-1)
        write(f, '!$OMP END MASTER', d=depth-1, postn=True)

def write_state_subroutine_module_externs(f, depth, block, mod_name):

    if len(block['names'])==0:
        write(f, '! No module extern variables', d=depth)
    else:
        write(f, '! module extern variables', d=depth, pren=True)
        write(f, 'SUBROUTINE kgen_write_externs_%s(kgen_unit)'%mod_name, d=depth, pren=True)
        write(f, 'INTEGER, INTENT(IN) :: kgen_unit', d=depth+1)
        GenIOStmt.write(f, depth+1, block, readflag=False)
        write(f, 'END SUBROUTINE kgen_write_externs_%s'%mod_name, d=depth, postn=True)

def write_state_usepart_module(f, depth, module):
    write_rw_usepart_module(f, depth, False, module)

def write_state_subroutines_type_write_var(f, depth, dtypelist):
    write_subroutines_type_rw_var(f, depth, False, dtypelist)

class GenVerification(GenBase):

    @classmethod
    def write_specpart(cls, f, depth, varname, res_stmt, var):
        write(f, 'character(*), intent(in) :: varname', d=depth+1)
        write(f, 'type(check_t), intent(inout) :: check_status', d=depth+1)

        #import pdb; pdb.set_trace()
        varptr = ''
        if var.is_pointer():
            varptr = ', POINTER'

        varalloc = ''
        if var.is_allocatable():
            varalloc = ', ALLOCATABLE'

        vardim = ''
        if var.is_array():
            vardim = ', DIMENSION(%s)'%','.join([':']*var.rank)

        if res_stmt.is_derived():
            vartype = 'TYPE'
        else:
            vartype = res_stmt.name

        varkind = ''
        length, kind = res_stmt.selector
        if vartype=='character':
            if length and kind:
                varkind = '(LEN=%s, KIND=%s)' % (length,kind)
            elif length:
                varkind = '(LEN=%s)' % (length)
            elif kind:
                varkind = '(KIND=%s)' % (kind)
        else:
            if res_stmt.is_derived():
                write(f, 'type(check_t) :: dtype_check_status', d=depth+1)
                varkind = '(%s)' % (kind)
            else:
                if length:
                    varkind = '*%s' % (length)
                if kind:
                    varkind = '(KIND=%s)' % (kind)

                
        write(f, '%s%s, intent(in)%s%s%s :: var, ref_var'%(vartype, varkind, vardim, varptr, varalloc), d=depth+1)

        if vardim:
            if vartype in [ 'real', 'double precision', 'complex' ]:
                write(f, '%s%s :: nrmsdiff, rmsdiff'%(vartype, varkind), d=depth+1)
                write(f, '%s%s, allocatable%s :: temp, temp2'%(vartype, varkind, vardim), d=depth+1)
                write(f, 'integer :: n', d=depth+1)
            elif vartype=='TYPE':
                write(f, 'integer :: %s'%','.join([ 'idx%d'%(r+1) for r in range(var.rank)]), d=depth+1)
    @classmethod
    def write_checkpart(cls, f, depth, varname, res_stmt, var, tempblock):

        #import pdb; pdb.set_trace()
        chkptr = ''
        if var.is_pointer():
            chkptr = 'ASSOCIATED(var)'

        chkalloc = ''
        if var.is_allocatable():
            chkalloc = 'ALLOCATED(var)'

        # if allocatable and/or pointer, check
        if chkptr and chkalloc:
            write(f, 'IF ( %s .AND. %s ) THEN'%(chkptr, chkalloc), d=depth+1)
        elif chkptr or chkalloc:
            write(f, 'IF ( %s%s ) THEN'%(chkptr, chkalloc), d=depth+1)

        eqtest = '=='
        if res_stmt.name=='logical':
            eqtest = '.EQV.'

        #if array
        if var.is_array():
            #import pdb; pdb.set_trace()
            allocshape = ','.join(['SIZE(var,dim=%d)'%(dim+1) for dim in range(var.rank)])
            # if dtype
            if res_stmt.is_derived():
                #import pdb; pdb.set_trace()

                #write(f, '! check all elements of var', d=depth+1)
                #write(f, kgen_verify_dtype_array.replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)
                #idx = [ 'idx%d'%(d+1) for d in range(var.rank) ]

                write(f, 'check_status%numTotal = check_status%numTotal + 1', d=depth+1)
                write(f, 'CALL kgen_init_check(dtype_check_status)', d=depth+1)

                # open nested DO loops
                for d in range(var.rank):
                    write(f, 'DO idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1}, d=depth+d+1)

                dtype = res_stmt.get_res_stmt(res_stmt.name)

                arrshape = ','.join([ 'idx%d'%(r+1) for r in range(var.rank)])
                if isinstance(dtype, Use):
                    mod_num = State.modules[dtype.name]['num']
                    write(f, 'CALL kgen_verify_mod%d(varname, dtype_check_status, var(%s), ref_var(%s))'%\
                        (mod_num, arrshape, arrshape), d=depth+d+2)
                elif isinstance(dtype, Type):
                    write(f, 'CALL kgen_verify_%s("%s", dtype_check_status, var(%s), ref_var(%s))'%\
                        (dtype.name, dtype.name, arrshape, arrshape), d=depth+d+2)
                else:
                    raise ProgramException('Unknown type: %s'%dtype.__class__)


                # close nested DO loops
                for d in reversed(range(var.rank)):
                    write(f, 'END DO', d=depth+d+1)

                write(f, 'IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN', d=depth+1)
                write(f, '    check_status%numIdentical = check_status%numIdentical + 1', d=depth+1)
                write(f, 'ELSE IF ( dtype_check_status%numFatal > 0 ) THEN', d=depth+1)
                write(f, '    check_status%numFatal = check_status%numFatal + 1', d=depth+1)
                write(f, 'ELSE IF ( dtype_check_status%numWarning > 0 ) THEN', d=depth+1)
                write(f, '    check_status%numWarning = check_status%numWarning + 1', d=depth+1)
                write(f, 'END IF', d=depth+1)

            else:
                if res_stmt.name in [ 'real', 'double precision', 'complex' ]:
                    write(f, (kgen_verify_numeric_array%{'eqtest':eqtest, \
                        'allocshape':allocshape}).replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)
                else:
                    write(f, (kgen_verify_nonreal_array%{'eqtest':eqtest}).replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)

        else:
            # if dtype
            if res_stmt.is_derived():
                write(f, '')
                write(f, 'check_status%numTotal = check_status%numTotal + 1', d=depth+1)
                write(f, 'CALL kgen_init_check(dtype_check_status)', d=depth+1)

                dtype = res_stmt.get_res_stmt(res_stmt.name)

                if isinstance(dtype, Use):
                    mod_num = State.modules[dtype.name]['num']
                    write(f, 'CALL kgen_verify_mod%d(varname, dtype_check_status, var, ref_var)'%mod_num, d=depth)
                elif isinstance(dtype, Type):
                    for comp, _depth in walk(dtype):
                        if isinstance(comp, TypeDeclarationStatement):
                            decl = Entity_Decl(comp.entity_decls[0])
                            uname = KGName(decl.items[0].string)
                            tempblock['names'].append(uname)
                            tempblock['res_stmt'][uname] = comp

                            var = comp.parent.a.variables[uname.firstpartname()]
                            subrname = GenBase.subrname_stmt(comp, var, call4arr=True, subr4kind=True)
                            
                            if not subrname:
                                subrname = comp.name

                            for entity in comp.entity_decls:
                                decl = Entity_Decl(entity)
                                uname = KGName(decl.items[0].string)
                                n = uname.firstpartname()

                                write(f, 'CALL kgen_verify_%s("%s", dtype_check_status, var%%%s, ref_var%%%s)'%(subrname, n, n, n), d=depth+1)
                            #import pdb; pdb.set_trace()
                else:
                    raise ProgramException('Unknown type: %s'%dtype.__class__)

                #subrname = 'pend'
                #write(f, (kgen_verify_dtype_checkpart%subrname).replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)
                #write(f, 'PRINT *, "NOTE: verification for derived type is not implemented."', d=depth+1)
                write(f, 'IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN', d=depth+1)
                write(f, '    check_status%numIdentical = check_status%numIdentical + 1', d=depth+1)
                write(f, 'ELSE IF ( dtype_check_status%numFatal > 0 ) THEN', d=depth+1)
                write(f, '    check_status%numFatal = check_status%numFatal + 1', d=depth+1)
                write(f, 'ELSE IF ( dtype_check_status%numWarning > 0 ) THEN', d=depth+1)
                write(f, '    check_status%numWarning = check_status%numWarning + 1', d=depth+1)
                write(f, 'END IF', d=depth+1)
            else:
                write(f, (kgen_verify_intrinsic_checkpart%eqtest).replace('\n', '\n%s'%(TAB*(depth+1))), d=depth+1)
        # end if

        # end if alloca...
        if chkptr or chkalloc:
            write(f, 'END IF', d=depth+1)

    @classmethod
    def write(cls, f, depth, srcblock=State.parentblock['output']):
        import re

        tempblock = {}
        tempblock['names'] = []
        tempblock['res_stmt'] = {}

        for block in [ srcblock, tempblock ]:
            for uname in block['names']:
                res_stmt = block['res_stmt'][uname]
                n = uname.firstpartname()
                var = res_stmt.parent.a.variables[n]
                subrname = GenBase.subrname_stmt(res_stmt, var, call4arr=True, subr4kind=True)
                
                if subrname:
                    # remove subroutine contains mod<number>
                    modcall = re.search(r'mod[\d]+$', subrname, re.I)
                    if modcall:
                        continue
                else:
                    subrname = res_stmt.name

                if subrname in verify_subrnames:
                    continue

                write(f, 'SUBROUTINE kgen_verify_%s( varname, check_status, var, ref_var)'%subrname, d=depth+1)
                cls.write_specpart(f, depth+1, n, res_stmt, var)
                cls.write_checkpart(f, depth+1, n, res_stmt, var, tempblock)
                write(f, 'END SUBROUTINE kgen_verify_%s'%subrname, d=depth+1)
                write(f, '')

                verify_subrnames.append(subrname)


