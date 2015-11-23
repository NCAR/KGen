# gen_write_typedecl_in_module.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import PARENTBLOCK_WRITE_IN_ARGS, PARENTBLOCK_WRITE_IN_LOCALS, PARENTBLOCK_WRITE_OUT_LOCALS, \
    get_typedecl_writename, get_dtype_writename, gencore_contains
from gencore_write_subr import create_write_subr

class Gen_S_Typedecl_In_Parentblock(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.created_subrs = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            typedecl_statements.TypeDeclarationStatement, self.typedecl_has_state_parentblock, self.create_subr_write_typedecl_in_parentblock) 

    def typedecl_has_state_parentblock(self, node):
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state(node.kgen_stmt.geninfo) \
            and "parameter" not in node.kgen_stmt.attrspec and node.kgen_parent.kgen_stmt==getinfo('parentblock_stmt'):
            return True
        return False

    def create_subr_write_typedecl_in_parentblock(self, node):
        stmt = node.kgen_stmt

        argintype = []
        localintype = []
        localouttype = []
        for uname, req in KGGenType.get_state_in(stmt.geninfo):
            entity_name = uname.firstpartname()
            if entity_name not in argintype and any( attr.startswith('intent') for attr in stmt.attrspec ):
                argintype.append((uname.firstpartname(), PARENTBLOCK_WRITE_IN_ARGS))
            elif entity_name not in localintype:
                localintype.append((uname.firstpartname(), PARENTBLOCK_WRITE_IN_LOCALS))
        for uname, req in KGGenType.get_state_out(stmt.geninfo):
            entity_name = uname.firstpartname()
            if entity_name not in localouttype:
                localouttype.append((uname.firstpartname(), PARENTBLOCK_WRITE_OUT_LOCALS))
        vartypes = [ argintype, localintype, localouttype ]

        for vartype in vartypes:
            for entity_name, partid in vartype:
                var = stmt.get_variable(entity_name)
                subrname = get_typedecl_writename(stmt, entity_name)
                if var.is_array():
                    if stmt.is_derived():
                        self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                        if subrname not in self.created_subrs:
                            create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                            self.created_subrs.append(subrname)
                    else: # intrinsic type
                        if var.is_explicit_shape_array():
                            self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)
                        else: # implicit array
                            self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                            if subrname not in self.created_subrs:
                                create_write_subr(subrname, entity_name, node.kgen_parent, var, stmt)
                                self.created_subrs.append(subrname)
                else: # scalar
                    if stmt.is_derived():
                        subrname = None
                        # TODO : add use and public statement??? in bridge modules?
                        for uname, req in stmt.unknowns.iteritems():
                            if uname.firstpartname()==stmt.name:
                                res = req.res_stmts[0]
                                subrname = get_dtype_writename(res)
                                break
                        if subrname is None: raise ProgramException('Can not find Type resolver for %s'%stmt.name)
                        self.create_write_call(node.kgen_kernel_id, partid, subrname, entity_name, stmt, var)
                    else: # intrinsic type
                        self.create_write_intrinsic(node.kgen_kernel_id, partid, entity_name, stmt, var)

    def create_write_intrinsic(self, kernel_id, partid, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
            ifptrobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)
        else:
            attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **"'%entity_name, entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)


    def create_write_call(self, kernel_id, partid, callname, entity_name, stmt, var):
        pobj = None
        if var.is_pointer():
            attrs = {'expr': 'ASSOCIATED(%s)'%entity_name}
            ifptrobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.TRUE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            part_append_gensnode(ifptrobj, EXEC_PART, statements.Else)

            attrs = {'variable': 'is_true', 'sign': '=', 'expr': '.FALSE.'}
            part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'items': ['is_true'], 'specs': ['UNIT = kgen_unit']}
            namedpart_append_gensnode(kernel_id, partid, statements.Write, attrs=attrs)

            attrs = {'expr': 'is_true'}
            iftrueobj = namedpart_append_gensnode(kernel_id, partid, block_statements.IfThen, attrs=attrs)

            pobj = iftrueobj

        if pobj:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                part_append_gensnode(pobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%entity_name]}
                namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)
            else:
                attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
                namedpart_append_gensnode(kernel_id, partid, statements.Call, attrs=attrs)

