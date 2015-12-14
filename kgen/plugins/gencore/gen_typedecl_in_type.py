# gen_write_typedecl_in_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_writename, get_typedecl_writename, state_gencore_contains, \
    get_dtype_readname, get_typedecl_readname, kernel_gencore_contains

class Gen_Typedecl_In_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_created_subrs = []
        self.kernel_created_subrs = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_typedecl_in_type, self.create_subr_write_typedecl_in_type) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_typedecl_in_type, self.create_subr_read_typedecl_in_type) 

    def is_typedecl_in_type(self, node):
        parent = node.kgen_parent
        if parent.kgen_match_class is block_statements.Type and parent.kgen_stmt and hasattr(parent.kgen_stmt, 'geninfo') and \
            len(parent.kgen_stmt.geninfo)>0:
            return True
        else: return False

    def create_read_subr(self, subrname, entity_name, parent, var, stmt, allocate=False):
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if subrname not in self.kernel_created_subrs and not part_has_node(parent, SUBP_PART, checks):

            self.kernel_created_subrs.append(subrname)

            checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
            if not parent in kernel_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_genknode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                kernel_gencore_contains.append(parent)

            part_append_comment(parent, SUBP_PART, 'write state subroutine for %s'%subrname)
            attrs = {'name': subrname, 'args': ['var', 'kgen_unit', 'printvar']}
            subrobj = part_append_genknode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            # variable A
            #import pdb; pdb.set_trace()
            attrspec = ['INTENT(INOUT)']
            if var.is_pointer(): attrspec.append('POINTER')
            if var.is_allocatable() or allocate: attrspec.append('ALLOCATABLE')
            if var.is_array(): attrspec.append('DIMENSION(%s)'% ','.join(':'*var.rank))
            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, 'selector':stmt.selector, 'entity_decls': ['var']}
            part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # printvar
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)', 'OPTIONAL'], 'selector':('*', None), 'entity_decls': ['printvar']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            # array index A
            if var.is_array():
                attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank) ]}
                part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'attrspec': ['DIMENSION(2,%d)'%var.rank], 'entity_decls': [ 'kgen_bound' ]}
                part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            part_append_comment(subrobj, DECL_PART, '')


            attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)
            part_append_comment(subrobj, EXEC_PART, '')

            attrs = {'expr': 'kgen_istrue'}
            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if var.is_array():
                bound_args = []
                for dim in range(var.rank):
                    attrs = {'items': ['kgen_bound(1, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)

                    attrs = {'items': ['kgen_bound(2, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)

                    bound_args.append('kgen_bound(2,%d)-kgen_bound(1,%d)+1'%(dim+1, dim+1))


                ifalloc = None
                if var.is_allocatable() or allocate:
                    attrs = {'expr': 'ALLOCATED( var )'}
                    ifalloc = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['var']}
                    part_append_genknode(ifalloc, EXEC_PART, statements.Deallocate, attrs=attrs)

                elif var.is_pointer():
                    attrs = {'expr': 'ASSOCIATED( var )'}
                    ifalloc = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    attrs = {'items': ['var']}
                    part_append_genknode(ifalloc, EXEC_PART, statements.Nullify, attrs=attrs)

                if ifalloc or allocate:
                    attrs = {'items': ['var(%s)'%', '.join(bound_args)]}
                    part_append_genknode(iftrueobj, EXEC_PART, statements.Allocate, attrs=attrs)

                if stmt.is_derived():
                    indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
                    str_indexes = ','.join(indexes)

                    prevobj = iftrueobj
                    doobjs = []
                    for d in range(var.rank):
                        attrs = {'loopcontrol': 'idx%(d)d=kgen_bound(1,%(d)d), kgen_bound(2,%(d)d)'%{'d':d+1}}
                        doobj = part_append_genknode(prevobj, EXEC_PART, block_statements.Do, attrs=attrs)

                        doobjs.append(doobj)
                        prevobj = doobj

                    attrs = {'expr': 'PRESENT( printvar )'}
                    ifpvarobj = part_append_genknode(doobjs[-1], EXEC_PART, block_statements.IfThen, attrs=attrs)

                    callname = None
                    for uname, req in stmt.unknowns.iteritems():
                        if uname.firstpartname()==stmt.name:
                            res = req.res_stmts[0]
                            callname = get_dtype_readname(res)
                            break
                    if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)

                    attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes]}
                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Else)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s(%s)"'%(entity_name, str_indexes)]}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                    else:
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit']}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                else: # intrinsic type
                    attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                    part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        if stmt.is_numeric() and var.is_array():
                            attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, 'SUM(var)']}
                        else:
                            attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_genknode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
                    else:
                        attrs = {'expr': 'PRESENT( printvar )'}
                        ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        if stmt.is_numeric() and var.is_array():
                            attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **"'%entity_name, 'SUM(var)']}
                        else:
                            attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)


            else: # scalar
                if self.stmt.is_derived():
                    attrs = {'expr': 'PRESENT( printvar )'}
                    ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    callname = None
                    for uname, req in stmt.unknowns.iteritems():
                        if uname.firstpartname()==stmt.name:
                            res = req.res_stmts[0]
                            callname = get_dtype_readname(res)
                            break
                    if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)

                    attrs = {'designator': callname, 'items': ['var', 'kgen_unit', 'printvar // " %s "'%entity_name]}
                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                    part_append_genknode(ifpvarobj, EXEC_PART, statements.Else)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'designator': callname, 'items': ['var', 'kgen_unit', '"%s"'%entity_name]}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                    else:
                        attrs = {'designator': callname, 'items': ['var', 'kgen_unit']}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                else: # intrinsic type
                    attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                    part_append_genknode(iftrueobj, EXEC_PART, statements.Read, attrs=attrs)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_genknode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
                    else:
                        attrs = {'expr': 'PRESENT( printvar )'}
                        ifpvarobj = part_append_genknode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_genknode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_write_subr(self, subrname, entity_name, parent, var, stmt):
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if subrname not in self.state_created_subrs and not part_has_node(parent, SUBP_PART, checks):

            self.state_created_subrs.append(subrname)

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
            #if var.is_pointer(): attrspec.append('POINTER')
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

            # array index A
            if var.is_array():
                attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank) ]}
                part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            part_append_comment(subrobj, DECL_PART, '')

            attrs = {'expr': 'SIZE(var)==1'}
            ifarrobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            ifobj = ifarrobj

            if var.is_array():
                attrs = {'expr': 'UBOUND(var, 1)<LBOUND(var, 1)'}
                ifbndobj = part_append_gensnode(ifobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
                part_append_gensnode(ifbndobj, EXEC_PART, statements.Assignment, attrs=attrs)

                attrs = {'expr': 'UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0'}
                part_append_gensnode(ifbndobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
                part_append_gensnode(ifbndobj, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_gensnode(ifbndobj, EXEC_PART, block_statements.Else, attrs=attrs)

                attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
                part_append_gensnode(ifbndobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifobj, EXEC_PART, block_statements.Else, attrs=attrs)

            attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifobj, EXEC_PART, statements.Assignment, attrs=attrs)
            part_append_comment(subrobj, EXEC_PART, '')

            attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(subrobj, EXEC_PART, statements.Write, attrs=attrs)
            part_append_comment(subrobj, EXEC_PART, '')

            attrs = {'expr': 'kgen_istrue'}
            iftrueobj = gensobj(subrobj, block_statements.IfThen, subrobj.kgen_kernel_id, attrs=attrs)
            iftrueobj = part_append_gensnode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            if var.is_array():
                for dim in range(var.rank):
                    attrs = {'items': ['LBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'items': ['UBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)

                if stmt.is_derived():
                    indexes = [ 'idx%d'%(d+1) for d in range(var.rank) ]
                    str_indexes = ','.join(indexes)

                    prevobj = iftrueobj
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
                        if uname.firstpartname()==stmt.name:
                            res = req.res_stmts[0]
                            callname = get_dtype_writename(res)
                            break
                    if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)

                    attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', 'printvar // "(%s)"'%str_indexes]}
                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                    part_append_gensnode(ifpvarobj, EXEC_PART, statements.Else)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit', '"%s(%s)"'%(entity_name, str_indexes)]}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)
                    else:
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit']}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Call, attrs=attrs)

                else: # intrinsic type
                    attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        if stmt.is_numeric() and var.is_array():
                            attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, 'SUM(var)']}
                        else:
                            attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
                    else:
                        attrs = {'expr': 'PRESENT( printvar )'}
                        ifpvarobj = part_append_gensnode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        if stmt.is_numeric() and var.is_array():
                            attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **"'%entity_name, 'SUM(var)']}
                        else:
                            attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **" // NEW_LINE("A")'%entity_name, 'var']}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)


            else: # scalar
                if self.stmt.is_derived():
                    attrs = {'expr': 'PRESENT( printvar )'}
                    ifpvarobj = part_append_gensnode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    callname = None
                    for uname, req in stmt.unknowns.iteritems():
                        if uname.firstpartname()==stmt.name:
                            res = req.res_stmts[0]
                            callname = get_dtype_writename(res)
                            break
                    if callname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)

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
                    part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)

                    if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                        attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, 'var']}
                        part_append_gensnode(iftrueobj, EXEC_PART, statements.Write, attrs=attrs)
                    else:
                        attrs = {'expr': 'PRESENT( printvar )'}
                        ifpvarobj = part_append_gensnode(iftrueobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        attrs = {'items': ['"** KGEN DEBUG: " // printvar // " %s **"'%entity_name, 'var']}
                        part_append_gensnode(ifpvarobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_subr_read_typedecl_in_type(self, node):

        stmt = node.kgen_stmt
        parent = node.kgen_parent.kgen_parent
        entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]

        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_readname(stmt, entity_name)
            if subrname is None: raise ProgramException('Can not get subroutinename')

            if var.is_array():
                if stmt.is_derived():
                    self.create_read_subr(subrname, entity_name, parent, var, stmt)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        pass
                    else: # implicit array
                        self.create_read_subr(subrname, entity_name, parent, var, stmt)
      
    def create_subr_write_typedecl_in_type(self, node):

        stmt = node.kgen_stmt
        parent = node.kgen_parent.kgen_parent
        entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]

        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_writename(stmt, entity_name)
            if subrname is None: raise ProgramException('Can not get subroutinename')

            if var.is_array():
                if stmt.is_derived():
                    self.create_write_subr(subrname, entity_name, parent, var, stmt)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        pass
                    else: # implicit array
                        self.create_write_subr(subrname, entity_name, parent, var, stmt)

