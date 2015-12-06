# gencore_write_subr.py

#import statements
#import block_statements
#import typedecl_statements
#from gencore_utils import kernel_gencore_contains, state_gencore_contains, get_dtype_writename, get_dtype_readname

def create_verify_subr(subrname, entity_name, parent, var, stmt, allocate=False, ename_prefix=''):
    pass
#
#    checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
#    if not part_has_node(parent, SUBP_PART, checks):
#
#        checks = lambda n: n.kgen_isvalid and isinstance(n.kgen_stmt, statements.Contains)
#        if not parent in kernel_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
#            part_append_comment(parent, CONTAINS_PART, '')
#            part_append_genknode(parent, CONTAINS_PART, statements.Contains)
#            part_append_comment(parent, CONTAINS_PART, '')
#            kernel_gencore_contains.append(parent)
#
#        part_append_comment(parent, SUBP_PART, 'read state subroutine for %s'%subrname)
#        attrs = {'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
#        subrobj = part_append_genknode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
#        part_append_comment(parent, SUBP_PART, '')
#
#        # variable A
#        #import pdb; pdb.set_trace()
#        attrspec = ['INTENT(INOUT)']
#        if var.is_pointer(): attrspec.append('POINTER')
#        if var.is_allocatable() or allocate: attrspec.append('ALLOCATABLE')
#        if var.is_array(): attrspec.append('DIMENSION(%s)'% ','.join(':'*var.rank))
#        attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, 'selector':stmt.selector, 'entity_decls': ['var']}
#        part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)
#
#        # kgen_unit
#        attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
#        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
#
#        # printvar
#        attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
#        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)
#
#        # is_true
#        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_true']}
#        part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
#
#        # array index A
#        if var.is_array():
#            attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank) ]}
#            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
#
#            attrs = {'type_spec': 'INTEGER', 'attrspec': ['DIMENSION(2,%d)'%var.rank], 'entity_decls': [ 'kgen_bound' ]}
#            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
#
#        part_append_comment(subrobj, DECL_PART, '')
#
#        attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
#        part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)
#        part_append_comment(subrobj, EXEC_PART, '')
#
#        attrs = {'expr': 'is_true'}
#        iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#        if var.is_array():
#            bound_args = []
#            for dim in range(var.rank):
#                attrs = {'items': ['kgen_bound(1, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
#                part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)
#
#                attrs = {'items': ['kgen_bound(2, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
#                part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)
#
#                bound_args.append('kgen_bound(2,%d)-kgen_bound(1,%d)+1'%(dim+1, dim+1))
#
#
#            ifalloc = None
#            if var.is_allocatable() or allocate:
#                attrs = {'expr': '.NOT. ALLOCATED( var )'}
#                ifalloc = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#            elif var.is_pointer():
#                attrs = {'expr': '.NOT. ASSOCIATED( var )'}
#                ifalloc = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#            if ifalloc:
#                attrs = {'items': ['var(%s)'%', '.join(bound_args)]}
#                part_append_genknode(ifalloc, EXEC_PART, statements.Allocate, attrs=attrs)
#            elif allocate:
#                attrs = {'items': ['var(%s)'%', '.join(bound_args)]}
#                part_append_genknode(iftrueobj, EXEC_PART, statements.Allocate, attrs=attrs)
#
#            if stmt.is_derived():
#                indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
#                str_indexes = ','.join(indexes)
#
#                prevobj = iftrueobj
#                doobjs = []
#                for d in range(var.rank):
#                    attrs = {'loopcontrol': 'idx%(d)d=kgen_bound(1,%(d)d), kgen_bound(2,%(d)d)'%{'d':d+1}}
#                    doobj = part_append_genknode(prevobj, EXEC_PART, block_statements.Do, attrs=attrs)
#
#                    doobjs.append(doobj)
#                    prevobj = doobj
#
#                attrs = {'expr': 'PRESENT( printvar )'}
#                ifpvarobj = part_append_genknode(doobjs[-1], EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#                callname = None
#                for uname, req in stmt.unknowns.iteritems():
#                    if uname.firstpartname()==stmt.name:
#                        res = req.res_stmts[0]
#                        callname = get_dtype_readname(res)
#                        break
#                if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
#
#                #attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "("//%s//")"'%str_indexes]}
#                attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes]}
#                part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#
#                part_append_genknode(ifpvarobj, EXEC_PART, statements.Else)
#
#                if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
#                    #attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s("//%s//")"'%(entity_name, str_indexes)]}
#                    attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s%s(%s)"'%(ename_prefix, entity_name, str_indexes)]}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#                else:
#                    attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit']}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#
#            else: # intrinsic type
#                attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
#                part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)
#
#                if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
#                    attrs = {'items': ['"** KGEN DEBUG: " // "%s%s **"'%(ename_prefix, entity_name), 'var']}
#                    part_append_genknode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
#                else:
#                    attrs = {'expr': 'PRESENT( printvar )'}
#                    ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#                    attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s%s **"'%(ename_prefix, entity_name), 'var']}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)
#
#
#        else: # scalar
#            if stmt.is_derived():
#                attrs = {'expr': 'PRESENT( printvar )'}
#                ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#                callname = None
#                for uname, req in stmt.unknowns.iteritems():
#                    if uname.firstpartname()==stmt.name:
#                        res = req.res_stmts[0]
#                        callname = get_dtype_readname(res)
#                        break
#                if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
#
#                attrs = {'designator': callname, 'items': ['var', 'kgen_unit', 'printvar // " %s%s "'%(ename_prefix, entity_name)]}
#                part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#
#                part_append_genknode(ifpvarobj, EXEC_PART, statements.Else)
#
#                if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
#                    attrs = {'designator': callname, 'items': ['var', 'kgen_unit', '"%s%s"'%(ename_prefix, entity_name)]}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#                else:
#                    attrs = {'designator': callname, 'items': ['var', 'kgen_unit']}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
#            else: # intrinsic type
#                attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
#                part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)
#
#                if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
#                    attrs = {'items': ['"** KGEN DEBUG: " // "%s%s **"'%(ename_prefix, entity_name), 'var']}
#                    part_append_genknode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
#                else:
#                    attrs = {'expr': 'PRESENT( printvar )'}
#                    ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#                    attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s%s **"'%(ename_prefix, entity_name), 'var']}
#                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)
#
