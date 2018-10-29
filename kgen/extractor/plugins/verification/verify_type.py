# gen_write_type.py
 
from parser import statements, block_statements, typedecl_statements
from kgplugin import Kgen_Plugin
from verify_utils import get_dtype_verifyname, get_typedecl_verifyname, kernel_verify_contains, kernel_verify_kgenutils, is_remove_state, is_remove_io, \
    is_zero_array

class Verify_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.kernel_created_use_items = []
        self.kernel_created_public_items = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Type, self.has_state_out_info, self.create_dtype_verify_subr) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, self.has_dtype_res_path, self.add_verifynames_in_use_public) 

    def has_state_out_info(self, node):
        if node.kgen_stmt and 'abstract' not in node.kgen_stmt.specs and \
            hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0:
            return True
        else: return False

    def has_dtype_res_path(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo'):
            if not node.kgen_stmt.isonly: return False
            for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
                if any(len(req.res_stmts)>0 and isinstance(req.res_stmts[0], block_statements.Type) and \
                    'abstract' not in req.res_stmts[0].specs for uname, req in reqlist):
                    return True
        return False

    def add_verifynames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if len(req.res_stmts)>0 and isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_verifyname(req.res_stmts[0])
                    checks = lambda n: n.kgen_match_class==statements.Use and n.kgen_stmt and n.kgen_stmt.isonly and \
                        subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_use_items and not part_has_node(parent, USE_PART, checks):
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        part_append_genknode(parent, USE_PART, statements.Use, attrs=attrs)
                        self.kernel_created_use_items.append((id(parent),subrname))

                    checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_public_items and isinstance(parent.kgen_stmt, block_statements.Module) and \
                        not part_has_node(parent, DECL_PART, checks):
                        attrs = {'items':[subrname]}
                        part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)
                        self.kernel_created_public_items.append((id(parent),subrname))

    # process function
    def create_dtype_verify_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'

        def print_numarr_detail(parent, entity_name):
            attrs = {'items': ['count( var%%%s /= kgenref_var%%%s)'%(entity_name, entity_name), \
                '" of "', 'size( var%%%s )'%entity_name, '" elements are different."']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Average - kernel "', 'sum(var%%%s)/real(size(var%%%s))'%(entity_name, entity_name)]}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Average - reference "', 'sum(kgenref_var%%%s)/real(size(kgenref_var%%%s))'%(entity_name, entity_name)]}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"RMS of difference is "', 'rmsdiff_%s'%entity_name]}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['"Normalized RMS of difference is "', 'nrmsdiff_%s'%entity_name]}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['""']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        def print_dtypearr_detail(parent, entity_name):

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

        def print_dtype_detail(parent, entity_name):

            attrs = {'items': [ '"    number of components       : "', 'comp_check_status%numtotal']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': [ '"    identical                  : "', 'comp_check_status%numidentical']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': [ '"    not identical - out of tol.: "', 'comp_check_status%numouttol']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': [ '"    not identical - within tol.: "', 'comp_check_status%numintol']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['""']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        def print_num_detail(parent, entity_name):
            attrs = {'items': ['"Difference is "', 'diff_%s'%entity_name]}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['""']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        def print_dummy_detail(parent, entity_name):
            attrs = {'items': ['"NOT IMPLEMENTED YET"']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

            attrs = {'items': ['""']}
            part_append_genknode(parent, EXEC_PART, statements.Write, attrs=attrs)

        subrname = get_dtype_verifyname(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
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
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var', 'kgenref_var']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # dtype_check_status
            attrs = {'type_spec': 'TYPE', 'selector':(None, 'check_t'), 'entity_decls': ['dtype_check_status', 'comp_check_status']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # check result
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['check_result']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # print check result
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_print = .FALSE.']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

            part_append_comment(subrobj, EXEC_PART, '')
            attrs = {'variable': 'check_status%numTotal', 'sign': '=', 'expr': 'check_status%numTotal + 1'}
            part_append_genknode(subrobj, EXEC_PART, statements.Assignment, attrs=attrs)
            part_append_comment(subrobj, EXEC_PART, '')

            attrs = {'designator': 'kgen_init_check', 'items': ['dtype_check_status', 'verboseLevel=check_status%verboseLevel']}
            part_append_genknode(subrobj, EXEC_PART, statements.Call, attrs=attrs)

            #comp_part = get_part(node, TYPE_COMP_PART) 
            comp_part = get_part(node, TYPE_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]
                topobj = subrobj
                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
                    var = stmt.get_variable(entity_name)

                    if is_remove_state(entity_name, stmt): continue
                    if is_remove_io(entity_name, stmt): continue
                    if var.is_array() and is_zero_array(var, stmt): continue

                    callname = get_typedecl_verifyname(stmt, entity_name)

                    ifallocobj = None
                    ifassocobj = None
                    if var.is_allocatable():
                        attrs = {'expr': 'ALLOCATED(var%%%s)'%entity_name}
                        ifallocobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
                        if var.is_pointer():
                            attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
                            ifassocobj = part_append_genknode(ifallocobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
                    elif var.is_pointer():
                        attrs = {'expr': 'ASSOCIATED(var%%%s)'%entity_name}
                        ifassocobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    if ifassocobj: topobj = ifassocobj
                    elif ifallocobj: topobj = ifallocobj

                    attrs = {'variable': 'dtype_check_status%numTotal', 'sign': '=', 'expr': 'dtype_check_status%numTotal + 1'}
                    part_append_genknode(topobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    if var.is_array():
                        dim_shape = ','.join(':'*var.rank)
                        get_size = ','.join(['SIZE(var%%%s,dim=%d)'%(entity_name, dim+1) for dim in range(var.rank)])

                        if stmt.is_derived():
                            indexes = [ 'idx%d'%(r+1) for r in range(var.rank)]
                            ename_indexes = [ '%s_%s'%(idx,entity_name) for idx in indexes ]

                            callname = None
                            for uname, req in stmt.unknowns.iteritems():
                                if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                    callname = get_dtype_verifyname(req.res_stmts[0])
                                    break

                            attrs = {'type_spec': 'INTEGER', 'entity_decls': ename_indexes}
                            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                            attrs = {'designator': 'kgen_init_check', 'items': ['comp_check_status', 'verboseLevel=check_status%verboseLevel']}
                            part_append_genknode(topobj, EXEC_PART, statements.Call, attrs=attrs)

                            if callname:
                                pobj = topobj
                                doobjs = []
                                for d in range(var.rank):
                                    attrs = {'loopcontrol': ' idx%(d)d_%(e)s = LBOUND(var%%%(e)s,%(d)d), UBOUND(var%%%(e)s,%(d)d)'%{'e':entity_name, 'd':d+1}}
                                    doobj = part_append_genknode(pobj, EXEC_PART, block_statements.Do, attrs=attrs)
                                    doobjs.append(doobj)
                                    pobj = doobj 

                                # call verify subr
                                attrs = {'designator': callname, 'items': ['trim(adjustl(varname))//"%%%s"'%var.name, 'comp_check_status', \
                                    'var%%%s(%s)'%(entity_name, ','.join(ename_indexes)), 'kgenref_var%%%s(%s)'%(entity_name, ','.join(ename_indexes))]}
                                part_append_genknode(doobjs[-1], EXEC_PART, statements.Call, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numTotal == comp_check_status%numIdentical'}
                                ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 2'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))//"%%%s"'%entity_name,'" is IDENTICAL."']}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numOutTol > 0'}
                                part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(out of tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numInTol > 0'}
                                part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(within tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)
                        else: # intrinsic type

                            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['n_%s'%entity_name]}
                            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                            if stmt.name=='logical':
                                attrs = {'expr': 'ALL(var%%%s .EQV. kgenref_var%%%s)'%(entity_name, entity_name)}
                            else:
                                attrs = {'expr': 'ALL(var%%%s == kgenref_var%%%s)'%(entity_name, entity_name)}

                            ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'expr': 'check_status%verboseLevel > 2'}
                            ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'items': ['trim(adjustl(varname))','"%%%s is IDENTICAL."'%entity_name]}
                            part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                            attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            part_append_genknode(ifidobj, EXEC_PART, statements.Else)

                            if stmt.is_numeric():

                                # typececls
                                attrs = {'type_spec': stmt.name, 'selector':stmt.selector, 'entity_decls': \
                                    ['nrmsdiff_%s'%entity_name, 'rmsdiff_%s'%entity_name ]}
                                part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                                attrs = {'type_spec': stmt.name, 'attrspec': ['ALLOCATABLE'], 'selector':stmt.selector, 'entity_decls': \
                                    ['buf1_%s(%s)'%(entity_name, dim_shape),'buf2_%s(%s)'%(entity_name, dim_shape)]}
                                part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                                attrs = {'items': ['buf1_%s(%s)'%(entity_name, get_size)]}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Allocate, attrs=attrs)

                                attrs = {'items': ['buf2_%s(%s)'%(entity_name, get_size)]}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Allocate, attrs=attrs)

                                attrs = {'variable': 'n_%s'%entity_name, 'sign': '=', 'expr': 'COUNT(var%%%s /= kgenref_var%%%s)'%(entity_name, entity_name)}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'ABS(kgenref_var%%%s) > kgen_minvalue'%entity_name}
                                whereobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.Where, attrs=attrs)

                                attrs = {'variable': 'buf1_%s'%entity_name, 'sign': '=', \
                                    'expr': '((var%%%(e)s-kgenref_var%%%(e)s)/kgenref_var%%%(e)s)**2'%{'e':entity_name}}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'buf2_%s'%entity_name, 'sign': '=', 'expr': '(var%%%s-kgenref_var%%%s)**2'%(entity_name, entity_name)}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                part_append_genknode(whereobj, EXEC_PART, statements.ElseWhere)

                                attrs = {'variable': 'buf1_%s'%entity_name, 'sign': '=', 'expr': '(var%%%s-kgenref_var%%%s)**2'%(entity_name, entity_name)}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'buf2_%s'%entity_name, 'sign': '=', 'expr': 'buf1_%s'%entity_name}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'nrmsdiff_%s'%entity_name, 'sign': '=', 'expr': 'SQRT(SUM(buf1_%s)/REAL(n_%s))'%(entity_name, entity_name)}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'rmsdiff_%s'%entity_name, 'sign': '=', 'expr': 'SQRT(SUM(buf2_%s)/REAL(n_%s))'%(entity_name, entity_name)}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'nrmsdiff_%s > kgen_tolerance'%entity_name}
                                ifvobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(out of tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(within tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)
                            else: # not numerical

                                if stmt.name=='logical':
                                    attrs = {'variable': 'n_%s'%entity_name, 'sign': '=', 'expr': 'COUNT(var%%%s .NEQV. kgenref_var%%%s)'%(entity_name, entity_name)}
                                else:
                                    attrs = {'variable': 'n_%s'%entity_name, 'sign': '=', 'expr': 'COUNT(var%%%s /= kgenref_var%%%s)'%(entity_name, entity_name)}

                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(out of tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)
                    else: # scalar
                        if stmt.is_derived():

                            callname = None
                            for uname, req in stmt.unknowns.iteritems():
                                if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                                    callname = get_dtype_verifyname(req.res_stmts[0])
                                    break
                            #import pdb; pdb.set_trace()
                            if callname:

                                attrs = {'designator': 'kgen_init_check', 'items': ['comp_check_status', 'verboseLevel=check_status%verboseLevel']}
                                part_append_genknode(topobj, EXEC_PART, statements.Call, attrs=attrs)

                                attrs = {'designator': callname, 'items': ['"%s"'%entity_name, 'comp_check_status', 'var%%%s'%entity_name, 'kgenref_var%%%s'%entity_name]}
                                part_append_genknode(topobj, EXEC_PART, statements.Call, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numTotal == comp_check_status%numIdentical'}
                                ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 2'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))//"%%%s"'%entity_name,'" is IDENTICAL."']}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numOutTol > 0'}
                                part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(out of tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'comp_check_status%numInTol > 0'}
                                part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(within tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                        else:
                            # diff
                            attrs = {'type_spec': stmt.name, 'selector':stmt.selector, 'entity_decls': ['diff_%s'%entity_name]}
                            part_append_genknode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

                            if stmt.name=='logical':
                                attrs = {'expr': 'var%%%s .EQV. kgenref_var%%%s'%(entity_name, entity_name)}
                            else:
                                attrs = {'expr': 'var%%%s == kgenref_var%%%s'%(entity_name, entity_name)}
                            ifidobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'expr': 'check_status%verboseLevel > 2'}
                            ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'items': ['trim(adjustl(varname))','"%%%s is IDENTICAL."'%entity_name]}
                            part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                            attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            part_append_genknode(ifidobj, EXEC_PART, statements.Else)

                            if stmt.is_numeric():

                                attrs = {'variable': 'diff_%s'%entity_name, 'sign': '=', 'expr': 'ABS(var%%%s - kgenref_var%%%s)'%(entity_name, entity_name)}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'diff_%s <= kgen_tolerance'%entity_name}
                                ifvobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(within tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifvobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL(out of tolerance)."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)
                            else: # not numeric

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'check_status%verboseLevel > 1'}
                                ifvlobj = part_append_genknode(ifidobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','"%%%s is NOT IDENTICAL."'%entity_name]}
                                part_append_genknode(ifvlobj, EXEC_PART, statements.Write, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

                    print_detail = print_dummy_detail
                    if var.is_array(): # array
                        if stmt.is_derived():
                            print_detail = print_dtypearr_detail
                            pass
                        else:
                            if stmt.is_numeric():
                                print_detail = print_numarr_detail
                            else:
                                pass
                    else:
                        if stmt.is_derived():
                            print_detail = print_dtype_detail
                        else:
                            if stmt.is_numeric():
                                print_detail = print_num_detail
                            else:
                                pass

                    attrs = {'expr': 'check_result == CHECK_IDENTICAL'}
                    ifchkobj = part_append_genknode(topobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    part_append_genknode(ifchkobj, EXEC_PART, statements.Continue)

#                    attrs = {'expr': 'check_status%verboseLevel > 2'}
#                    iflevel3obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)
#
#                    print_detail(iflevel3obj, entity_name)

                    attrs = {'expr': 'check_result == CHECK_OUT_TOL'}
                    part_append_genknode(ifchkobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 2'}
                    iflevel0obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    print_detail(iflevel0obj, entity_name)

                    attrs = {'expr': 'check_result == CHECK_IN_TOL'}
                    part_append_genknode(ifchkobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                    attrs = {'expr': 'check_status%verboseLevel > 2'}
                    iflevel1obj = part_append_genknode(ifchkobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                    print_detail(iflevel1obj, entity_name)

                part_append_comment(topobj, EXEC_PART, '')


            attrs = {'expr': 'dtype_check_status%numTotal == dtype_check_status%numIdentical'}
            ifidobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'check_status%numIdentical', 'sign': '=', 'expr': 'check_status%numIdentical + 1'}
            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'expr': 'dtype_check_status%numOutTol > 0'}
            part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

            attrs = {'variable': 'check_status%numOutTol', 'sign': '=', 'expr': 'check_status%numOutTol + 1'}
            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'expr': 'dtype_check_status%numInTol > 0'}
            part_append_genknode(ifidobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

            attrs = {'variable': 'check_status%numInTol', 'sign': '=', 'expr': 'check_status%numInTol + 1'}
            part_append_genknode(ifidobj, EXEC_PART, statements.Assignment, attrs=attrs)

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)

