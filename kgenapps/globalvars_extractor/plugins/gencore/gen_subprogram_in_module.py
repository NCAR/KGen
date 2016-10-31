# gen_write_typedecl_in_module.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import check_class_derived, get_typedecl_printname, get_dtype_printname

class Gen_SubProgram_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.SubProgramStatement, self.has_globalvars_in_module, self.create_globalvar_status) 

    def has_globalvars_in_module(self, node):
        if hasattr(node, 'kgen_stmt') and hasattr(node.kgen_stmt, 'globalvars'):
            return True
        return False

    def create_print_intrinsic(self, namelist, subpnode, var):
        namelist = list(namelist)
        resstmt = var.parent

        attrs = {'items': ['"Global variable: %s"'%namelist[-1]]}
        part_insert_gensnode(subpnode, EXEC_PART, statements.Write, attrs=attrs, index=0)

        attrs = {'items': ['"   used at %s"'%str(namelist)]}
        part_insert_gensnode(subpnode, EXEC_PART, statements.Write, attrs=attrs, index=1)

        resnamelist = [ a.name.lower() for a in resstmt.ancestors() ]
        attrs = {'items': ['"   declared at %s"'%str(resnamelist)]}
        part_insert_gensnode(subpnode, EXEC_PART, statements.Write, attrs=attrs, index=2)

        if resstmt.is_numeric() and var.is_array():
            attrs = {'items': ['"   REAL(SUM(%s), 8) = "'%namelist[-1], 'REAL(SUM(%s), 8)'%namelist[-1] ]}
        else:
            attrs = {'items': ['"   value = ", %s'%namelist[-1]]}
        part_insert_gensnode(subpnode, EXEC_PART, statements.Write, attrs=attrs, index=3)

    def get_usestmts(self, var, orgstmt):
        usestmts = []

        for u, r in orgstmt.unknowns.items():
            if u.firstpartname() == var.name:
                for s in r.res_stmts:
                    if isinstance(s, statements.Use):
                        usestmts.append(s)
        return usestmts

    def update_usestmt(self, ustmts, ostmt):
        pass

    def create_print_call(self, namelist, subrname,  subpnode, respair, var):

        orgstmt = respair[0]
        resstmt = respair[1]

        usestmts = get_usestmts(var, orgstmt)
        import pdb; pdb.set_trace()
        if usestmts:
            update_usestmt(usestmts, orgstmt):

        attrs = {'designator': subrname, 'items': [var.name, '"%s"'%(var.name)]}
        part_insert_gensnode(subpnode, EXEC_PART, statements.Call, attrs=attrs, index=0)

    def create_globalvar_status(self, node):
        node.kgen_stmt.top.used4genstate = True

        for namelist, res in node.kgen_stmt.globalvars.items():
            entity_name = namelist[-1]
            orgstmt = res[0]
            resstmt = res[1]
            is_class_derived = check_class_derived(resstmt)
            #attrs = {'items': ['"%s"'%str(namelist)] }
            #part_insert_gensnode(node, EXEC_PART, statements.Write, attrs=attrs, index=0)

            var = resstmt.get_variable(entity_name)
            subrname = get_typedecl_printname(resstmt, entity_name)
            if var.is_array():
                pass
                if resstmt.is_derived() or is_class_derived:
                    pass
#                    self.create_print_call(node.kgen_kernel_id, partid, subrname, entity_name, resstmt, var)
#                    if subrname not in self.state_created_subrs:
#                        create_write_subr(subrname, entity_name, node.kgen_parent, var, resstmt)
#                        self.state_created_subrs.append(subrname)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        if var.is_pointer():
                            pass
#                            self.create_print_call(node.kgen_kernel_id, partid, subrname, entity_name, resstmt, var)
#                            if subrname not in self.state_created_subrs:
#                                create_write_subr(subrname, entity_name, node.kgen_parent, var, resstmt)
#                                self.state_created_subrs.append(subrname)
                        else:
                            self.create_print_intrinsic(namelist, node, var)
                    else: # implicit array
                        pass
#                        self.create_print_call(node.kgen_kernel_id, partid, subrname, entity_name, resstmt, var)
#                        if subrname not in self.state_created_subrs:
#                            create_write_subr(subrname, entity_name, node.kgen_parent, var, resstmt)
#                            self.state_created_subrs.append(subrname)
            else: # scalar
                if resstmt.is_derived() or is_class_derived or var.is_pointer():
                    if var.is_allocatable() or var.is_pointer() or var.is_pointer():
                        pass
#                        self.create_print_call(node.kgen_kernel_id, partid, subrname, entity_name, resstmt, var)
#                        if subrname not in self.state_created_subrs:
#                            create_write_subr(subrname, entity_name, node.kgen_parent, var, resstmt)
#                            self.state_created_subrs.append(subrname)
                    else:
                        subrname = None
                        for uname, req in resstmt.unknowns.iteritems():
                            if uname.firstpartname()==resstmt.name and len(req.res_stmts)>0:
                                res = req.res_stmts[0]
                                subrname = get_dtype_printname(res)
                                break
                        if subrname is None:
                            print 'WARNING: Can not find Type resolver for %s'%resstmt.name
                            namedpart_append_comment(node.kgen_kernel_id, partid, \
                                'ERROR: "%s" is not resolved. Call statements to write "%s" is not created here.'%\
                                (resstmt.name, resstmt.name))
                        else:
                            self.create_print_call(namelist, subrname, node, res, var)
                else: # intrinsic type
                    self.create_print_intrinsic(namelist, node, var)
