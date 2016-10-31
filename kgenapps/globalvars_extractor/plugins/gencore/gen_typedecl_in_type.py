# gen_print_typedecl_in_type.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_dtype_printname, get_typedecl_printname, state_gencore_contains, \
    gen_print_istrue, is_zero_array, is_excluded, is_remove_state

class Gen_Typedecl_In_Type(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.state_created_subrs = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.is_typedecl_in_type, self.create_subr_print_typedecl_in_type) 

    def is_typedecl_in_type(self, node):
        parent = node.kgen_parent
        if parent.kgen_match_class is block_statements.Type and parent.kgen_stmt and node.kgen_stmt and \
            hasattr(parent.kgen_stmt, 'geninfo') and len(parent.kgen_stmt.geninfo)>0: 
            return True
        else: return False

    def create_print_subr(self, subrname, entity_name, parent, var, stmt):

        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==subrname
        if subrname not in self.state_created_subrs and not part_has_node(parent, SUBP_PART, checks):

            if is_remove_state(entity_name, stmt): return

            self.state_created_subrs.append(subrname)

            checks = lambda n: n.kgen_match_class==statements.Contains
            if not parent in state_gencore_contains and not part_has_node(parent, CONTAINS_PART, checks):
                part_append_comment(parent, CONTAINS_PART, '')
                part_append_gensnode(parent, CONTAINS_PART, statements.Contains)
                part_append_comment(parent, CONTAINS_PART, '')
                state_gencore_contains.append(parent)

            part_append_comment(parent, SUBP_PART, 'print state subroutine for %s'%subrname)
            attrs = {'name': subrname, 'args': ['var']}
            subrobj = part_append_gensnode(parent, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(parent, SUBP_PART, '')

            parent.kgen_stmt.top.used4genstate = True

            # variable A
            #import pdb; pdb.set_trace()
            attrspec = ['INTENT(IN)']
            if var.is_allocatable(): attrspec.append('ALLOCATABLE')
            if var.is_pointer(): attrspec.append('POINTER')
            if var.is_array(): attrspec.append('DIMENSION(%s)'% ','.join(':'*var.rank))
            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, 'selector':stmt.selector, 'entity_decls': ['var']}
            part_append_gensnode(subrobj, DECL_PART, stmt.__class__, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            # array index A
            if var.is_array():
                attrs = {'type_spec': 'INTEGER', 'entity_decls': [ 'idx%d'%(d+1) for d in range(var.rank) ]}
                part_append_gensnode(subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
            part_append_genknode(subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

            part_append_comment(subrobj, DECL_PART, '')

            pobj = gen_print_istrue(subrobj, var, 'var')

            if var.is_array():

                for dim in range(var.rank):
                    attrs = {'items': ['LBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'items': ['UBOUND(var, %d)'%(dim+1)], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

                if stmt.is_derived():
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
                    innerdo = doobjs[-1]

                    callname = None
                    for uname, req in stmt.unknowns.iteritems():
                        if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                            res = req.res_stmts[0]
                            callname = get_dtype_printname(res)
                            break
                    if callname is None:
                        print 'WARNING: Can not find Type resolver for %s'%stmt.name
                        part_append_comment(innerdo, EXEC_PART, \
                            'ERROR: "%s" is not resolved. Call statements to print "%s" is not created here.'%\
                            (stmt.name, stmt.name))
                    else:
                        attrs = {'designator': callname, 'items': ['var(%s)'%str_indexes, 'kgen_unit']}
                        part_append_gensnode(innerdo, EXEC_PART, statements.Call, attrs=attrs)

                else: # intrinsic type
                    attrs = {'items': ['var'], 'specs': ['UNIT = kgen_unit']}
                    part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

            else: # scalar
                if stmt.is_derived():
                    callname = None
                    for uname, req in stmt.unknowns.iteritems():
                        if uname.firstpartname()==stmt.name and len(req.res_stmts)>0:
                            res = req.res_stmts[0]
                            callname = get_dtype_printname(res)
                            break
                    if callname is None:
                        print 'WARNING: Can not find Type resolver for %s'%stmt.name
                        part_append_comment(pobj, EXEC_PART, \
                            'ERROR: "%s" is not resolved. Call statements to print "%s" is not created here.'%\
                            (stmt.name, stmt.name))
                    else:
                        attrs = {'designator': callname, 'items': ['var']}
                        part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
                else: # intrinsic type
                    attrs = {'items': ['var'], 'format': '*'}
                    part_append_gensnode(pobj, EXEC_PART, statements.Print, attrs=attrs)
     
    def create_subr_print_typedecl_in_type(self, node):

        stmt = node.kgen_stmt
        parent = node.kgen_parent.kgen_parent
        entity_names = [ get_entity_name(decl) for decl in stmt.entity_decls ]

        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_printname(stmt, entity_name)
            if subrname is None: raise Exception('Can not get subroutinename')

            if var.is_array():
                if is_zero_array(var, stmt): continue

                if stmt.is_derived():
                    self.create_print_subr(subrname, entity_name, parent, var, stmt)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        pass
                    else: # implicit array
                        self.create_print_subr(subrname, entity_name, parent, var, stmt)
            else:
                if stmt.is_derived():
                    if var.is_allocatable() or var.is_pointer():
                        self.create_print_subr(subrname, entity_name, parent, var, stmt)
