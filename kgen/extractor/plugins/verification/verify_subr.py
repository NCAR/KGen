# gencore_write_subr.py

from parser import statements, block_statements, typedecl_statements
from verify_utils import kernel_verify_contains, kernel_verify_kgenutils, get_dtype_verifyname, check_class_derived

########################## Verbose level table ####################
#  Content #  level <1   #    level 1   #  level 2   # level >2   #
###################################################################
#  P/F     #    O        #      O       #     O      #    O       #
###################################################################
#  Stat    #    X        #      O       #     O      #    O       #
###################################################################
#  Ident   #             #              #            #            #
#  Result  #    X        #      X       #     O      #    O       #
###################################################################
#  Diff.   #             #              #            #            #
#  Result  #    X        #      O       #     O      #    O       #
###################################################################
#  Num.    #             #              #            #            #
#  Outtol. #             #              #            #            #
#  Diff.   #    X        #      O       #     O      #    O       #
#  Detail  #             #              #            #            #
###################################################################
#  Num.    #             #              #            #            #
#  Intol.  #             #              #            #            #
#  Diff.   #    X        #      X       #     O      #    O       #
#  Detail  #             #              #            #            #
###################################################################
#  Num.    #             #              #            #            #
#  Ident.  #    X        #      X       #     X      #    O       #
#  Detail  #             #              #            #            #
###################################################################
#  Comp.   #             #              #            #            #
#  Ident.  #    X        #      X       #     X      #    O       #
#  Result  #             #              #            #            #
###################################################################
#  Comp.   #             #              #            #            #
#  Diff.   #    X        #      X       #     O      #    O       #
#  Result  #             #              #            #            #
###################################################################
#  Comp.   #             #              #            #            #
#  Diff.   #    X        #      X       #     X      #    O       #
#  Detail  #             #              #            #            #
###################################################################

def create_verify_subr(subrname, entity_name, parent, var, stmt):

    def print_numarr_detail(parent, is_identical=False):
        attrs = {'items': ['count( var /= kgenref_var)', '" of "', 'size( var )', '" elements are different."']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Average - kernel "', 'sum(var)/real(size(var))']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"Average - reference "', 'sum(kgenref_var)/real(size(kgenref_var))']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        if is_identical:
            attrs = {'items': ['"RMS of difference is "', '0']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Normalized RMS of difference is "', '0']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        else:
            attrs = {'items': ['"RMS of difference is "', 'rmsdiff']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Normalized RMS of difference is "', 'nrmsdiff']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

    def print_dtypearr_detail(parent, is_identical=False):

        attrs = {'items': [ '"    number of elements         : "', 'comp_check_status%numtotal']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': [ '"    identical                  : "', 'comp_check_status%numidentical']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': [ '"    not identical - out of tol.: "', 'comp_check_status%numouttol']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': [ '"    not identical - within tol.: "', 'comp_check_status%numintol']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

    def print_num_detail(parent, is_identical=False):
        if is_identical:
            attrs = {'items': ['"Difference is "', '0']}
        else:
            attrs = {'items': ['"Difference is "', 'diff']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

    def print_dummy_detail(parent, is_identical=False):
        attrs = {'items': ['"NOT IMPLEMENTED"']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['""']}
        part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

    def get_attrs(attrspec, allowed_attrs):
        attrspec = []
        for attr in stmt.attrspec:
            if any( attr.startswith(allowed_attr) for allowed_attr in allowed_attrs):
                attrspec.append(attr)
        return attrspec


    checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
    is_class_derived = check_class_derived(stmt)

    if  not part_has_node(parent, SUBP_PART, checks):

        checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
        if not parent in kernel_verify_contains and not part_has_node(parent, CONTAINS_PART, checks):
            part_append_comment(parent, CONTAINS_PART, '')
            part_append_genknode(parent, CONTAINS_PART, statements.Contains)
            part_append_comment(parent, CONTAINS_PART, '')
            kernel_verify_contains.append(parent)

        checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Use and n.name=='kgen_utils_mod' and 'check_t' in n.items
        if not parent in kernel_verify_kgenutils and not part_has_node(parent, USE_PART, checks):
            attrs = {'name': 'kgen_utils_mod', 'isonly': True, 'items': ['check_t', 'kgen_init_check', 'kgen_tolerance', \
                'kgen_minvalue', 'CHECK_IDENTICAL', 'CHECK_IN_TOL', 'CHECK_OUT_TOL']}
            part_append_genknode(parent, USE_PART, statements.Use, attrs=attrs)
            kernel_verify_kgenutils.append(parent)

        part_append_comment(parent, SUBP_PART, 'verify state subroutine for %s'%subrname)
        attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['varname', 'check_status', 'var', 'kgenref_var']}
        subrobj = part_append_genknode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
        part_append_comment(parent, SUBP_PART, '')

        # varname
        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)'], 'selector':('*', None), 'entity_decls': ['varname']}
        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

        # check_status
        attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(INOUT)'], 'selector':(None, 'check_t'), 'entity_decls': ['check_status']}
        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

        # variable
        type_spec = stmt.__class__.__name__.upper()
        selector = stmt.selector
        var_class = stmt.__class__
        attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable']) + ['INTENT(IN)']
        if var.is_array():
            attrspec.append('DIMENSION(%s)'%','.join(':'*var.rank))
            if stmt.is_derived() or is_class_derived:
                # comp_check_status
                attrs = {'type_spec': 'TYPE', 'selector':(None, 'check_t'), 'entity_decls': ['comp_check_status']}
                part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)
            if stmt.is_derived():
                type_spec = 'TYPE'
                selector = (None, stmt.name)
                var_class = typedecl_statements.Type
        else:
            if (stmt.is_derived() or is_class_derived) and not var.is_pointer():
                raise Exception('Non array or not pointer of derived type should not be processed here.')
        attrs = {'type_spec': type_spec, 'attrspec': attrspec, 'selector':selector, 'entity_decls': ['var', 'kgenref_var']}
        part_append_genknode(subrobj, DECL_PART, var_class, attrs=attrs)

        # check result
        #jgw#
        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['check_result = -1']}
        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        # print check result
        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_print = .FALSE.']}
        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        part_append_comment(subrobj, DECL_PART, '')

        part_append_comment(subrobj, EXEC_PART, '')

        ifallocobj = None
        ifassocobj = None
        if var.is_allocatable():
            attrs = {'expr': 'ALLOCATED(var)'}
            ifallocobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
            if var.is_pointer():
                attrs = {'expr': 'ASSOCIATED(var)'}
                ifassocobj = part_append_genknode(ifallocobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
        elif var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(var)'}
            ifassocobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        if ifassocobj: topobj = ifassocobj
        elif ifallocobj: topobj = ifallocobj
        else: topobj = subrobj

        attrs = {'variable': 'check_status%numTotal', 'sign': '=', 'expr': 'check_status%numTotal + 1'}
        part_append_genknode(topobj, EXEC_PART, statements.Assignment, attrs=attrs)
        part_append_comment(topobj, EXEC_PART, '')

        if var.is_array():

            # index
            attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank)]}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            dim_shape = ','.join(':'*var.rank)
            get_size = ','.join(['SIZE(var,dim=%d)'%(dim+1) for dim in range(var.rank)])

            if stmt.is_derived() or is_class_derived:

                callname = None
                for uname, req in stmt.unknowns.iteritems():
                    #if uname.firstpartname()==stmt.name:
                    if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                        if len(req.res_stmts)>0:
                            callname = get_dtype_verifyname(req.res_stmts[0])
                            break

                attrs = {'designator': 'kgen_init_check', 'items': ['comp_check_status', 'verboseLevel=check_status%verboseLevel']}
                part_append_genknode(topobj, EXEC_PART, statements.Call, attrs=attrs)

                if callname:
                    pobj = topobj
                    doobjs = []
                    for d in range(var.rank):
                        attrs = {'loopcontrol': ' idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1}}
                        doobj = part_append_genknode(pobj, EXEC_PART, block_statements.Do, attrs=attrs)
                        doobjs.append(doobj)
                        pobj = doobj 

                    # call verify subr
                    indexes = ','.join([ 'idx%d'%(r+1) for r in range(var.rank)])
                    attrs = {'designator': callname, 'items': ['trim(adjustl(varname))', 'comp_check_status', \
                        'var(%s)'%indexes, 'kgenref_var(%s)'%indexes]}
                    part_append_genknode(doobjs[-1], EXEC_PART, statements.Call, attrs=attrs)

                    attrs = {'expr': 'comp_check_status%numTotal == comp_check_status%numIdentical'}
                    ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'variable': 'check_status%numIdentical', 'sign': '=', 'expr': 'check_status%numIdentical + 1'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 1'}
                    ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is IDENTICAL."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'comp_check_status%numOutTol > 0'}
                    part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                    attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(out of tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'comp_check_status%numInTol > 0'}
                    part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                    attrs = {'variable': 'check_status%numInTol', 'sign': '=', 'expr': 'check_status%numInTol + 1'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(within tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)
            else: # intrinsic type

                attrs = {'type_spec': 'INTEGER', 'entity_decls': ['n']}
                part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                if stmt.name=='logical':
                    attrs = {'expr': 'ALL(var .EQV. kgenref_var)'}
                else:
                    attrs = {'expr': 'ALL(var == kgenref_var)'}

                ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'variable': 'check_status%numIdentical', 'sign': '=', 'expr': 'check_status%numIdentical + 1'}
                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                attrs = {'expr': 'check_status%verboseLevel > 1'}
                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'items': ['trim(adjustl(varname))','" is IDENTICAL."']}
                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_genknode(ifidobj, EXEC_PART, statements.Else)

                if stmt.is_numeric():

                    # typececls
                    attrs = {'type_spec': stmt.name, 'selector':stmt.selector, 'entity_decls': ['nrmsdiff', 'rmsdiff']}
                    part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                    attrs = {'type_spec': stmt.name, 'attrspec': ['ALLOCATABLE'], 'selector':stmt.selector, 'entity_decls': \
                        ['buf1(%s)'%dim_shape,'buf2(%s)'%dim_shape]}
                    part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                    attrs = {'items': ['buf1(%s)'%get_size]}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Allocate, attrs=attrs)

                    attrs = {'items': ['buf2(%s)'%get_size]}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Allocate, attrs=attrs)

                    attrs = {'variable': 'n', 'sign': '=', 'expr': 'COUNT(var /= kgenref_var)'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'ABS(kgenref_var) > kgen_minvalue'}
                    whereobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.Where, attrs=attrs)

                    attrs = {'variable': 'buf1', 'sign': '=', 'expr': '((var-kgenref_var)/kgenref_var)**2'}
                    part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'variable': 'buf2', 'sign': '=', 'expr': '(var-kgenref_var)**2'}
                    part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    part_append_genknode(whereobj, EXEC_PART, statements.ElseWhere)

                    attrs = {'variable': 'buf1', 'sign': '=', 'expr': '(var-kgenref_var)**2'}
                    part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'variable': 'buf2', 'sign': '=', 'expr': 'buf1'}
                    part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'variable': 'nrmsdiff', 'sign': '=', 'expr': 'SQRT(SUM(buf1)/DBLE(n))'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'variable': 'rmsdiff', 'sign': '=', 'expr': 'SQRT(SUM(buf2)/DBLE(n))'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'nrmsdiff > kgen_tolerance'}
                    ifvobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(out of tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                    attrs = {'variable': 'check_status%numInTol', 'sign': '=', 'expr': 'check_status%numInTol + 1'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(within tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)
                else: # not numerical

                    if stmt.name=='logical':
                        attrs = {'variable': 'n', 'sign': '=', 'expr': 'COUNT(var .NEQV. kgenref_var)'}
                    else:
                        attrs = {'variable': 'n', 'sign': '=', 'expr': 'COUNT(var /= kgenref_var)'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(out of tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)
        else: # scalar
            if not stmt.is_derived() or is_class_derived:
                # diff
                attrs = {'type_spec': stmt.name, 'selector':stmt.selector, 'entity_decls': ['diff']}
                part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                if stmt.name=='logical':
                    attrs = {'expr': 'var .EQV. kgenref_var'}
                else:
                    attrs = {'expr': 'var == kgenref_var'}
                ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'variable': 'check_status%numIdentical', 'sign': '=', 'expr': 'check_status%numIdentical + 1'}
                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                attrs = {'expr': 'check_status%verboseLevel > 1'}
                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'items': ['trim(adjustl(varname))','" is IDENTICAL."']}
                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_genknode(ifidobj, EXEC_PART, statements.Else)

                if stmt.is_numeric():

                    attrs = {'variable': 'diff', 'sign': '=', 'expr': 'ABS(var - kgenref_var)'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'diff <= kgen_tolerance'}
                    ifvobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'variable': 'check_status%numInTol', 'sign': '=', 'expr': 'check_status%numInTol + 1'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(within tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                    attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL(out of tolerance)."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                    part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)
                else: # not numeric

                    attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 0'}
                    ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL."']}
                    part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                    part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

        print_detail = print_dummy_detail
        if var.is_array(): # array
            if stmt.is_derived() or is_class_derived:
                print_detail = print_dtypearr_detail
            else:
                if stmt.is_numeric():
                    print_detail = print_numarr_detail
                else:
                    pass
        else:
            if stmt.is_derived() or is_class_derived:
                pass
            else:
                if stmt.is_numeric():
                    print_detail = print_num_detail
                else:
                    pass

        attrs = {'expr': 'check_result == CHECK_IDENTICAL'}
        ifchkobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'expr': 'check_status%verboseLevel > 2'}
        iflevel3obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        print_detail(iflevel3obj, is_identical=True)

        attrs = {'expr': 'check_result == CHECK_OUT_TOL'}
        part_append_genknode(ifchkobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

        attrs = {'expr': 'check_status%verboseLevel > 0'}
        iflevel0obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        print_detail(iflevel0obj)

        attrs = {'expr': 'check_result == CHECK_IN_TOL'}
        part_append_genknode(ifchkobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

        attrs = {'expr': 'check_status%verboseLevel > 1'}
        iflevel1obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        print_detail(iflevel1obj)

    part_append_comment(topobj, EXEC_PART, '')

    # create public stmt
#    if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
#        attrs = {'items': [subrname]}
#        part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)
