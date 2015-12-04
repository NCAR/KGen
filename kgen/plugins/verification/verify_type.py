# gen_write_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
#
from verify_utils import get_dtype_verifyname, get_typedecl_verifyname, kernel_verify_contains

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
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and len(node.kgen_stmt.geninfo)>0:
            return True
        else: return False

    def has_dtype_res_path(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo'):
            if not node.kgen_stmt.isonly: return False
            for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
                if any(isinstance(req.res_stmts[0], block_statements.Type) for uname, req in reqlist):
                    return True
        return False

    def add_verifynames_in_use_public(self, node):
        parent = node.kgen_parent

        for gentype, reqlist in node.kgen_stmt.geninfo.iteritems():
            for uname, req in reqlist:
                if isinstance(req.res_stmts[0], block_statements.Type):
                    subrname = get_dtype_verifyname(req.res_stmts[0])
                    checks = lambda n: isinstance(n.kgen_stmt, statements.Use) and n.kgen_stmt.isonly and \
                        subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_use_items and not part_has_node(parent, USE_PART, checks):
                        attrs = {'name':node.kgen_stmt.name, 'isonly': True, 'items':[subrname]}
                        part_append_genknode(parent, USE_PART, statements.Use, attrs=attrs)
                        self.kernel_created_use_items.append((id(parent),subrname))

                    checks = lambda n: isinstance(n.kgen_stmt, statements.Public) and n.kgen_stmt.items and subrname in n.kgen_stmt.items
                    if (id(parent),subrname) not in self.kernel_created_public_items and not part_has_node(parent, DECL_PART, checks):
                        attrs = {'items':[subrname]}
                        part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)
                        self.kernel_created_public_items.append((id(parent),subrname))

    def create_verify_intrinsic(self, subrobj, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        attrs = {'items': ['var%%%s'%entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'expr': 'PRESENT( printvar )'}
            ifobj = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            attrs = {'items': ['"** KGEN DEBUG: " // printvar // "%%%s **"'%entity_name, 'var%%%s'%entity_name]}
            part_append_genknode(ifobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_verify_call(self, subrobj, callname, entity_name, stmt, var):
        pobj = subrobj
        if var.is_pointer():
            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Read, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        attrs = {'expr': 'PRESENT( printvar )'}
        ifobj = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', 'printvar // "%%%s"'%entity_name]}
        part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

        part_append_genknode(ifobj, EXEC_PART, statements.Else)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit', '"%%%s"'%entity_name]}
            part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': ['var%%%s'%entity_name, 'kgen_unit']}
            part_append_genknode(ifobj, EXEC_PART, statements.Call, attrs=attrs)

    # process function
    def create_dtype_verify_subr(self, node):
        assert node.kgen_stmt, 'None kgen statement'


        subrname = get_dtype_verifyname(node.kgen_stmt)
        if subrname is None: return

        parent = node.kgen_parent
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if  not part_has_node(parent, SUBP_PART, checks):

            checks = lambda n: n.kgen_isvalid and isinstance(n.kgen_stmt, statements.Contains)
            if not parent in kernel_verify_contains and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_genknode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                kernel_verify_contains.append(parent)

            part_append_comment(parent, SUBP_PART, 'verify state subroutine for %s'%subrname)
            attrs = {'prefix': 'RECURSIVE', 'name': subrname, 'args': ['varname', 'check_status', 'var', 'ref_var']}
            subrobj = part_append_genknode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            # varname
            attrs = {'type_spec': 'CHARACTER', 'attrspec': ['INTENT(IN)'], 'selector':('*', None), 'entity_decls': ['varname']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Character, attrs=attrs)

            # check_status
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(INOUT)'], 'selector':(None, 'check_t'), 'entity_decls': ['check_status']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # variable
            attrs = {'type_spec': 'TYPE', 'attrspec': ['INTENT(IN)'], 'selector':(None, node.name), 'entity_decls': ['var', 'ref_var']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # dtype_check_status
            attrs = {'type_spec': 'TYPE', 'selector':(None, 'check_t'), 'entity_decls': ['dtype_check_status']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Type, attrs=attrs)

            # check result
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['check_result']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # print check result
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['is_print = .FALSE.']}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)
            part_append_comment(subrobj, DECL_PART, '')

            attrs = {'variable': 'check_status%numTotal', 'sign': '=', 'expr': 'check_status%numTotal + 1'}
            part_append_genknode(subrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'designator': kgen_init_check, 'items': ['dtype_check_status']}
            part_append_genknode(subrobj, EXEC_PART, statements.Call, attrs=attrs)

            comp_part = get_part(node, TYPE_COMP_PART) 
            for item in comp_part:
                if not hasattr(item, 'kgen_stmt'): continue
                if not isinstance(item.kgen_stmt, typedecl_statements.TypeDeclarationStatement): continue

                stmt = item.kgen_stmt
                entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]
                for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
                    var = stmt.get_variable(entity_name)
                    callname = get_typedecl_verifyname(stmt, entity_name)

                    if var.is_array():
                        dim_shape = ','.join(':'*var.rank)
                        get_size = ','.join(['SIZE(var,dim=%d)'%(dim+1) for dim in range(var.rank)])

                        if stmt.is_derived():

                            pobj = subrobj
                            doobjs = []
                            for d in range(var.rank):
                                attrs = {'loopcontrol': ' idx%(d)d=LBOUND(var,%(d)d), UBOUND(var,%(d)d)'%{'d':d+1}}
                                doobj = part_append_genknode(pobj, EXEC_PART, block_statements.Do, attrs=attrs)
                                pobj = doobj 

                            # call verify subr
                            indexes = ','.join([ 'idx%d'%(r+1) for r in range(var.rank)])
                            attrs = {'designator': callname, 'items': ['"%s"'%var.name, 'dtype_dtype_check_status', 'var(%s)'%indexes, 'ref_var(%s)'%indexes]}
                            part_append_genknode(doobjs[-1], EXEC_PART, statements.Call, attrs=attrs)

                            attrs = {'expr': 'dtype_dtype_check_status%numTotal == dtype_dtype_check_status%numIdentical'}
                            iftrueobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                            part_append_genknode(iftrueobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'expr': 'dtype_dtype_check_status%numOutTol > 0'}
                            part_append_genknode(iftrueobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                            part_append_genknode(iftrueobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'expr': 'dtype_dtype_check_status%numInTol > 0'}
                            part_append_genknode(iftrueobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                            part_append_genknode(iftrueobj, EXEC_PART, statements.Assignment, attrs=attrs)

                        else: # intrinsic type
                
                            if stmt.name=='logical':
                                attrs = {'expr': 'ALL(var .EQV. ref_var)'}
                            else:
                                attrs = {'expr': 'ALL(var == ref_var)'}
                            ifcmpobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'items': ['trim(adjustl(varname))','" is IDENTICAL."']}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Write, attrs=attrs)

                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Else)

                            if stmt.is_numeric():

                                attrs = {'items': ['temp(%s)'%get_size]}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Allocate, attrs=attrs)

                                attrs = {'items': ['temp2(%s)'%get_size]}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Allocate, attrs=attrs)

                                attrs = {'variable': 'n', 'sign': '=', 'expr': 'COUNT(var /= ref_var)'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'ABS(ref_var) > dtype_check_status%minvalue'}
                                whereobj = part_append_genknode(ifcmpobj, EXEC_PART, block_statements.Where, attrs=attrs)

                                attrs = {'variable': 'temp', 'sign': '=', 'expr': '((var-ref_var)/ref_var)**2'}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'temp2', 'sign': '=', 'expr': '(var-ref_var)**2'}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                part_append_genknode(whereobj, EXEC_PART, statements.ElseWhere)

                                attrs = {'variable': 'temp', 'sign': '=', 'expr': '(var-ref_var)**2'}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'temp2', 'sign': '=', 'expr': 'temp'}
                                part_append_genknode(whereobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'nrmsdiff', 'sign': '=', 'expr': 'SQRT(SUM(temp)/REAL(n))'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'rmsdiff', 'sign': '=', 'expr': 'SQRT(SUM(temp2)/REAL(n))'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'nrmsdiff > dtype_check_status%tolerance'}
                                ifvobj = part_append_genknode(ifcmpobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL out of tolerance."']}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Write, attrs=attrs)

                                part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL within tolerance."']}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Write, attrs=attrs)
                            else: # not numerical
                                attrs = {'variable': 'n', 'sign': '=', 'expr': 'COUNT(var /= ref_var)'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL out of tolerance."']}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Write, attrs=attrs)

                    else: # scalar
                        if not stmt.is_derived():
                            if stmt.name=='logical':
                                attrs = {'expr': 'var .EQV. ref_var'}
                            else:
                                attrs = {'expr': 'var == ref_var'}
                            ifcmpobj = part_append_genknode(subrobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                            attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IDENTICAL'}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'variable': 'dtype_check_status%numIdentical', 'sign': '=', 'expr': 'dtype_check_status%numIdentical + 1'}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                            attrs = {'items': ['trim(adjustl(varname))','" is IDENTICAL."']}
                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Write, attrs=attrs)

                            part_append_genknode(ifcmpobj, EXEC_PART, statements.Else)

                            if stmt.is_numeric():

                                attrs = {'variable': 'diff', 'sign': '=', 'expr': 'ABS(var - ref_var)'}
                                part_append_genknode(ifcmpobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'expr': 'diff <= dtype_check_status%tolerance'}
                                ifvobj = part_append_genknode(ifcmpobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_IN_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numInTol', 'sign': '=', 'expr': 'dtype_check_status%numInTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL within tolerance."']}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Write, attrs=attrs)

                                part_append_genknode(ifvobj, EXEC_PART, statements.Else)

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'variable': 'dtype_check_status%numOutTol', 'sign': '=', 'expr': 'dtype_check_status%numOutTol + 1'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                                attrs = {'items': ['trim(adjustl(varname))','" is NOT IDENTICAL out of tolerance."']}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Write, attrs=attrs)
                            else: # not numeric

                                attrs = {'variable': 'check_result', 'sign': '=', 'expr': 'CHECK_OUT_TOL'}
                                part_append_genknode(ifvobj, EXEC_PART, statements.Assignment, attrs=attrs)

                part_append_comment(subrobj, EXEC_PART, '')

            # create public stmt
            if parent.kgen_match_class in [ block_statements.Program, block_statements.Module ]:
                attrs = {'items': [subrname]}
                part_append_genknode(parent, DECL_PART, statements.Public, attrs=attrs)

#
#    def gen_verify_subrs_print(self, subrobj, topobj, var):
#
#        def print_numarr_detail(parent):
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['count( var /= ref_var)', '" of "', 'size( var )', '" elements are different."'])
#
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"Average - kernel "', 'sum(var)/real(size(var))'])
#
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"Average - reference "', 'sum(ref_var)/real(size(ref_var))'])
#
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"RMS of difference is "', 'rmsdiff'])
#
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"Normalized RMS of difference is "', 'nrmsdiff'])
#
#        def print_num_detail(parent):
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"Difference is "', 'diff'])
#
#        def print_dummy_detail(parent):
#            obj = parent.create_write()
#            obj.set_attr('item_list', ['"NOT IMPLEMENTED"'])
#
#        print_detail = print_dummy_detail
#        if var.is_array(): # array
#            if self.stmt.is_derived():
#                pass
#            else:
#                if self.stmt.is_numeric():
#                    print_detail = print_numarr_detail
#                else:
#                    pass
#        else:
#            if self.stmt.is_derived():
#                pass
#            else:
#                if self.stmt.is_numeric():
#                    print_detail = print_num_detail
#                else:
#                    pass
#
#
#        ifvobj = topobj.create_ifthen()
#        ifvobj.set_attr('expr', 'check_status%verboseLevel > 2')
#
#        incobj = ifvobj.create_assignstmt()
#        incobj.set_attr('lhs', 'is_print')
#        incobj.set_attr('rhs', '.TRUE.')
#
#        ifelse1obj = ifvobj.create_elseif()
#        ifelse1obj.set_attr('expr', 'check_status%verboseLevel == 2')
#
#        ifot2obj = ifvobj.create_ifthen()
#        ifot2obj.set_attr('expr', 'check_result /= CHECK_IDENTICAL')
#
#        incobj = ifot2obj.create_assignstmt()
#        incobj.set_attr('lhs', 'is_print')
#        incobj.set_attr('rhs', '.TRUE.')
#
#        endifot2obj = ifot2obj.create_endobj()
#        endifot2obj.set_attr('blockname', 'IF')
#
#        ifelse1obj = ifvobj.create_elseif()
#        ifelse1obj.set_attr('expr', 'check_status%verboseLevel == 1')
#
#        ifot3obj = ifvobj.create_ifthen()
#        ifot3obj.set_attr('expr', 'check_result == CHECK_OUT_TOL')
#
#        incobj = ifot3obj.create_assignstmt()
#        incobj.set_attr('lhs', 'is_print')
#        incobj.set_attr('rhs', '.TRUE.')
#
#        endifot3obj = ifot3obj.create_endobj()
#        endifot3obj.set_attr('blockname', 'IF')
#
#        ifelse1obj = ifvobj.create_elseif()
#        ifelse1obj.set_attr('expr', 'check_status%verboseLevel < 1')
#
#        incobj = ifvobj.create_assignstmt()
#        incobj.set_attr('lhs', 'is_print')
#        incobj.set_attr('rhs', '.FALSE.')
#
#        endifvobj = ifvobj.create_endobj()
#        endifvobj.set_attr('blockname', 'IF')
#
#        topobj.add_line(topobj.insert_in_exe_part)
#
#        ifprintobj = topobj.create_ifthen()
#        ifprintobj.set_attr('expr', 'is_print')
#
#        print_detail(ifprintobj)
#
#        endifprintobj = ifprintobj.create_endobj()
#        endifprintobj.set_attr('blockname', 'IF')


#
#                #### exec. part - print ####
#                self.gen_verify_subrs_print(subrobj, topobj, var)

