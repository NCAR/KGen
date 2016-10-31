# gencore_write_subr.py

import statements
import block_statements
import typedecl_statements
from gencore_utils import state_gencore_contains, get_dtype_printname, gen_write_istrue, check_class_derived


def create_write_subr(subrname, entity_name, parent, var, stmt, implicit=False):
    checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname

    is_class_derived = check_class_derived(stmt)

    if not part_has_node(parent, SUBP_PART, checks):

        checks = lambda n: n.kgen_match_class==statements.Contains
        if not parent in state_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
            part_append_comment(parent, CONTAINS_PART, '')
            part_append_gensnode(parent, CONTAINS_PART, statements.Contains)
            part_append_comment(parent, CONTAINS_PART, '')
            state_gencore_contains.append(parent)

        part_append_comment(parent, SUBP_PART, 'write state subroutine for %s'%subrname)
        attrs = {'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
        subrobj = part_append_gensnode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(parent, SUBP_PART, '')

        parent.kgen_stmt.top.used4genstate = True

        # variable A
        #import pdb; pdb.set_trace()
        attrspec = ['INTENT(IN)']
        if var.is_pointer(): attrspec.append('POINTER')
        if var.is_allocatable(): attrspec.append('ALLOCATABLE')
        if var.is_array(): attrspec.append('DIMENSION(%s)'% ','.join(':'*var.rank))
        attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, 'selector':stmt.selector, 'entity_decls': ['var']}
        part_append_gensnode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

        # kgen_unit
        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
        part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        # printvar
        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
        part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

        # kgen_istrue
        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
        part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
        part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

        # array index A
        if var.is_array():
            attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank) ]}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        part_append_comment(subrobj, DECL_PART, '')

        pobj = gen_write_istrue(subrobj, var, 'var')
        part_append_comment(subrobj, EXEC_PART, '')

        if var.is_array():
            for dim in range(var.rank):
                attrs = {'items': ['LBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'items': ['UBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

            if stmt.is_derived() or is_class_derived:
                indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
                str_indexes = ','.join(indexes)
                #tostr_indexes = ','.join([ 'kgen_tostr(idx)' for idx in indexes])

                prevobj = pobj
                doobjs = []
                for d in range(var.rank):
                    attrs = {'loopcontrol': 'idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1}}
                    doobj = part_append_gensnode(prevobj, EXEC_PART, block_statements.Do, attrs=attrs)

                    doobjs.append(doobj)
                    prevobj = doobj

                attrs = {'expr': 'PRESENT( printvar )'}
                ifpvarobj = part_append_gensnode(doobjs[-1], EXEC_PART, block_statements.IfThen, attrs=attrs)

                callname = None
                for uname, req in stmt.unknowns.iteritems():
                    if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                    #if uname.firstpartname()==stmt.name:
                        if len(req.res_stmts)>0:
                            res = req.res_stmts[0]
                            callname = get_dtype_printname(res)
                            break
                if callname is None:
                    print 'WARNING: Can not find Type resolver for %s'%stmt.name
                    part_append_comment(ifpvarobj, EXEC_PART, \
                        'ERROR: "%s" is not resolved. Call statements to write "%s" is not created here.'%\
                        (stmt.name, stmt.name))
                else:
                    #attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(", %s, ")"'%tostr_indexes]}
                    attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes]}
                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Else)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        #attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s(", %s, ")"'%(entity_name, tostr_indexes)]}
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s(%s)"'%(entity_name, str_indexes)]}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                    else:
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit']}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

            else: # intrinsic type
                attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'expr': 'PRESENT( printvar )'}
                ifpvarobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A")', 'var']}
                part_append_gensnode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)

        else: # scalar
            if stmt.is_derived() or is_class_derived:

                callname = None
                for uname, req in stmt.unknowns.iteritems():
                    if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                    #if uname.firstpartname()==stmt.name:
                        if len(req.res_stmts)>0:
                            res = req.res_stmts[0]
                            callname = get_dtype_printname(res)
                            break
                if callname is None:
                    print 'WARNING: Can not find Type resolver for %s'%stmt.name
                    part_append_comment(pobj, EXEC_PART, \
                        'ERROR: "%s" is not resolved. Call statements to write "%s" is not created here.'%\
                        (stmt.name, stmt.name))
                else:
                    attrs = {'expr': 'PRESENT( printvar )'}
                    ifpvarobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'designator': callname, 'items': ['var', 'kgen_unit', 'printvar // " %s "'%entity_name]}
                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Else)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'designator': callname, 'items': ['var', 'kgen_unit', '"%s"'%entity_name]}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                    else:
                        attrs = {'designator': callname, 'items': ['var', 'kgen_unit']}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
            else: # intrinsic type
                attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'expr': 'PRESENT( printvar )'}
                ifpvarobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'items': ['"** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A")', 'var']}
                part_append_gensnode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)
