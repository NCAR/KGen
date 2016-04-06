# gen_write_typedecl_in_parentblock.py

import os 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

def has_state(node):
    if not hasattr(node, 'kgen_stmt'): return False

    if hasattr(node.kgen_stmt, 'geninfo') or hasattr(node.kgen_stmt, 'unknowns'):
        return True
    else:
        return False

class Debug_Add_Sum_Subprogram(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per function 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Subroutine, has_state, self.add_sum_funcargs) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Subroutine, has_state, self.add_sum_funcargs) 

    def add_sum_funcargs(self, node):
        if not hasattr(node, 'kgen_stmt') or not hasattr(node.kgen_stmt, 'unknowns'): return
        if node.kgen_stmt.is_pure() or node.kgen_stmt.is_elemental(): return

        for uname, req in node.kgen_stmt.unknowns.items():
            if req.res_stmts and req.res_stmts[0]:
                varname = uname.firstpartname()
                stmt = req.res_stmts[0]
                if not isinstance(stmt, typedecl_statements.TypeDeclarationStatement): continue

                var = stmt.get_variable(varname)
                path = os.path.basename(stmt.item.reader.id)
                if stmt.is_numeric() and (var.is_intent_in() or var.is_intent_inout()):
                    node.kgen_stmt.top.used4genstate = True

                    attrs = {'expr': 'kgen_region_started .AND. kgen_collect_log'}
                    ifobj = part_insert_gensnode(node, EXEC_PART, block_statements.IfThen, attrs=attrs, index=0)

                    if var.is_array():
                        attrs = {'items': ['"SUM(%s) at function %s in %s: "'%(varname, node.name, path),'SUM(%s)'%varname]}
                        part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)
                    else:
                        attrs = {'items': ['"%s at function %s in %s: "'%(varname, node.name, path), varname]}
                        part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)


class Debug_Add_Sum_Assignment(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.BeginSource, self.has_topblock, self.create_common_block) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.BeginSource, self.has_topblock, self.create_common_block) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Function, None, self.add_common_stmt) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Function, has_state, self.add_common_stmt) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Subroutine, None, self.add_common_stmt) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Subroutine, has_state, self.add_common_stmt) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            statements.Assignment, None, self.add_write_stmt) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            statements.Assignment, has_state, self.add_write_stmt) 

    def has_topblock(self, node):
        checks = lambda n: n.kgen_stmt and n.kgen_stmt==getinfo('topblock_stmt')
        if part_has_node(node, UNIT_PART, checks):
            return True
        else:
            return False

    def create_common_block(self, node):
        # add block data
        attrs = {'name': 'kgen_block_data'}
        bdata = part_append_genknode(node, UNIT_PART, block_statements.BlockData, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_region_started = .FALSE.', 'kgen_collect_log = .FALSE.']}
        part_append_genknode(bdata, DECL_PART, typedecl_statements.Logical, attrs=attrs) 

        attrs = {'items': [('kgen_common_region', ['kgen_region_started', 'kgen_collect_log'])]}
        part_append_genknode(bdata, DECL_PART, statements.Common, attrs=attrs) 

        # register event for topblock file 
        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
        #    block_statements.BeginSource, self.has_topblock, self.create_common_block) 

        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
        #    block_statements.BeginSource, self.has_topblock, self.create_common_block) 

    def add_common_stmt(self, node):

        if node.kgen_stmt is None: return

        node.kgen_stmt.top.used4genstate = True

        if not (node.kgen_stmt.is_pure() or node.kgen_stmt.is_elemental()):
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_invoc_count = 0']}
            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_region_started', 'kgen_collect_log']}
        part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs) 

        attrs = {'items': [('kgen_common_region', ['kgen_region_started', 'kgen_collect_log'])]}
        part_append_gensnode(node, DECL_PART, statements.Common, attrs=attrs)

    def add_write_stmt(self, node):
        if node.kgen_stmt is None: return

        # find typedecl for lhs
        typedecl_stmt = None
        varname = None
        for uname, req in node.kgen_stmt.unknowns.items():
            if node.kgen_stmt.variable.startswith(uname.firstpartname()):
                if isinstance(req.res_stmts[0], typedecl_statements.TypeDeclarationStatement):
                    typedecl_stmt = req.res_stmts[0]
                    varname = uname.firstpartname()
                    break

        if typedecl_stmt and typedecl_stmt.is_numeric():
            var = typedecl_stmt.get_variable(varname)
            path = os.path.basename(node.kgen_stmt.item.reader.id)
            index, partid, part = get_part_index(node)
            upperblock_stmt = node.kgen_stmt.ancestors()[-1]
            if isinstance(upperblock_stmt, block_statements.SubProgramStatement) and (upperblock_stmt.is_pure() or \
                upperblock_stmt.is_elemental()):
                return

            # reduce output log size
            if not node.kgen_stmt.parent is upperblock_stmt: return

            node.kgen_stmt.top.used4genstate = True

            attrs = {'expr': 'kgen_region_started .AND. kgen_collect_log'}
            ifobj = part_insert_gensnode(node.kgen_parent, partid, block_statements.IfThen, attrs=attrs, index=(index+1))

            if var.is_array():
                attrs = {'items': ['"SUM(%s) at function %s in %s: "'%(varname, upperblock_stmt.name, path),'SUM(%s)'%varname]}
                part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)
            else:
                attrs = {'items': ['"%s at function %s in %s: "'%(varname, upperblock_stmt.name, path), varname]}
                part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)


