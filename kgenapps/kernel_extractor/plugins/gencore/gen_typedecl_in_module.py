# gen_write_typedecl_in_module.py
 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

from gencore_utils import get_topname, get_typedecl_writename, get_dtype_writename, get_module_in_writename, STATE_PBLOCK_WRITE_IN_EXTERNS, \
    STATE_PBLOCK_USE_PART, kernel_gencore_contains, state_gencore_contains, get_typedecl_readname, get_dtype_readname, get_module_in_readname, \
    KERNEL_PBLOCK_USE_PART, DRIVER_READ_IN_EXTERNS, process_spec_stmts, get_module_out_writename, get_module_out_readname, \
    KERNEL_PBLOCK_READ_OUT_EXTERNS, STATE_PBLOCK_WRITE_OUT_EXTERNS, gen_write_istrue, gen_read_istrue, is_excluded, \
    is_remove_state, is_zero_array, DRIVER_USE_PART, check_class_derived
from gencore_subr import create_write_subr, create_read_subr

class Gen_Typedecl_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

        self.state_externs_subrs = {}
        self.kernel_externs_subrs = {}

        self.state_callsite_use_stmts = []
        self.kernel_callsite_use_stmts = []

        self.state_callsite_call_stmts = []
        self.kernel_callsite_call_stmts = []

        self.state_created_subrs = []
        self.kernel_created_subrs = []

        self.state_extern_writes = []
        self.kernel_extern_reads = []

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.has_externs_in_module, self.create_state_module_parts) 

        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
        #    block_statements.Module, None, self.use_ieee_module) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, self.has_externs_in_module, self.create_kernel_module_parts) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Module, None, self.add_default_stmts) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.Module, self.has_specstmts_in_module, self.process_specstmts_in_module) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Use, None, self.process_use_in_module) 

    def has_externs_in_module(self, node):
        for stmt in node.kgen_stmt.content:
            if isinstance(stmt, typedecl_statements.TypeDeclarationStatement) and \
                "parameter" not in stmt.attrspec and  hasattr(stmt, 'geninfo') and \
                any(len(v) > 0 for v in stmt.geninfo.values()):
                for entity_name in [ get_entity_name(decl) for decl in stmt.entity_decls ]:
                    var = stmt.get_variable(entity_name)
                    if not var.is_parameter():
                        return True
        return False

    def has_specstmts_in_module(self, node):
        if not node.kgen_stmt: return False
        if hasattr(node.kgen_stmt, 'spec_stmts'): return True
        else: return False

    def is_extern_in_kernel_module(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and any(len(v) > 0 for v in node.kgen_stmt.geninfo.values()) and \
            isinstance(node.kgen_parent.kgen_stmt, block_statements.Module) and 'parameter' not in node.kgen_stmt.attrspec:
            for entity_name in [ get_entity_name(decl) for decl in node.kgen_stmt.entity_decls ]:
                var = node.kgen_stmt.get_variable(entity_name)
                if not var.is_parameter():
                    return True
        return False

    def is_extern_in_state_module(self, node):
        if node.kgen_stmt and hasattr(node.kgen_stmt, 'geninfo') and any(len(v) > 0 for v in node.kgen_stmt.geninfo.values()) and \
            isinstance(node.kgen_parent.kgen_stmt, block_statements.Module) and 'parameter' not in node.kgen_stmt.attrspec:
            for entity_name in [ get_entity_name(decl) for decl in node.kgen_stmt.entity_decls ]:
                var = node.kgen_stmt.get_variable(entity_name)
                if not var.is_parameter():
                    return True
        return False

    def process_specstmts_in_module(self, node):
        process_spec_stmts(node.kgen_stmt)

    def process_use_in_module(self, node):
        if not node.kgen_isvalid: return
        if not node.kgen_stmt: return
        if not hasattr(node.kgen_stmt, 'geninfo'):
            node.kgen_isvalid = False
            return
      
        new_items = []
        unames = list(set([ uname.firstpartname() for uname, req in KGGenType.get_state(node.kgen_stmt.geninfo) ]))
        for item in node.kgen_stmt.items:
            if item.split('=>')[0].strip() in unames:
                new_items.append(item)
        node.items = new_items
        node.nature = node.kgen_stmt.nature
        node.isonly = node.kgen_stmt.isonly
        node.kgen_use_tokgen = True

#    def use_ieee_module(self, node):
#
#        attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
#        part_append_gensnode(node, USE_PART, statements.Use, attrs=attrs)


    def add_default_stmts(self, node):

        attrs = {'name':'kgen_utils_mod', 'isonly': True, 'items':['kgen_dp', 'kgen_array_sumcheck']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        attrs = {'name':'tprof_mod', 'isonly': True, 'items':['tstart', 'tstop', 'tnull', 'tprnt']}
        part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

        #attrs = {'name':'IEEE_ARITHMETIC', 'nature': 'INTRINSIC', 'isonly': True, 'items':['ieee_is_normal']}
        #part_append_genknode(node, USE_PART, statements.Use, attrs=attrs)

    def create_kernel_module_parts(self, node):
        in_subrobj = None
        in_subrname = get_module_in_readname(node.kgen_stmt)
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==in_subrname
        if not part_has_node(node, SUBP_PART, checks):

            checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
            if not node in kernel_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
                part_append_comment(node, CONTAINS_PART, '')
                part_append_genknode(node, CONTAINS_PART, statements.Contains)
                part_append_comment(node, CONTAINS_PART, '')
                kernel_gencore_contains.append(node)

            attrs = {'name': in_subrname, 'args': ['kgen_unit']}
            part_append_comment(node, SUBP_PART, 'read state subroutine for %s'%in_subrname)
            in_subrobj = part_append_genknode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(node, SUBP_PART, '')

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_genknode(in_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_genknode(in_subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
            part_append_genknode(in_subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

            part_append_comment(in_subrobj, DECL_PART, '')

            # add public stmt
            attrs = {'items':[in_subrname]}
            part_append_genknode(node, DECL_PART, statements.Public, attrs=attrs)

        out_subrobj = None
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state_out(node.kgen_stmt.geninfo):
            out_subrname = get_module_out_readname(node.kgen_stmt)
            checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==out_subrname
            if not part_has_node(node, SUBP_PART, checks):

                checks = lambda n: n.kgen_isvalid and n.kgen_match_class==statements.Contains
                if not node in kernel_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
                    part_append_comment(node, CONTAINS_PART, '')
                    part_append_genknode(node, CONTAINS_PART, statements.Contains)
                    part_append_comment(node, CONTAINS_PART, '')
                    kernel_gencore_contains.append(node)

                attrs = {'name': out_subrname, 'args': ['kgen_unit']}
                part_append_comment(node, SUBP_PART, 'read state subroutine for %s'%out_subrname)
                out_subrobj = part_append_genknode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
                part_append_comment(node, SUBP_PART, '')

                # kgen_unit
                attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
                part_append_genknode(out_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)
                part_append_comment(out_subrobj, DECL_PART, '')

                # kgen_istrue
                attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
                part_append_genknode(out_subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
                part_append_genknode(out_subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

                # add public stmt
                attrs = {'items':[out_subrname]}
                part_append_genknode(node, DECL_PART, statements.Public, attrs=attrs)


        if in_subrobj or out_subrobj:
            self.kernel_externs_subrs[node] = ( in_subrobj, out_subrobj )

            # register event per typedecl 
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
                typedecl_statements.TypeDeclarationStatement, self.is_extern_in_kernel_module, self.create_subr_read_typedecl_in_module) 

            # register event per module
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
                block_statements.Module, self.has_externs_in_module, self.create_kernel_stmts_in_callsite) 

    def create_state_module_parts(self, node):

        in_subrname = get_module_in_writename(node.kgen_stmt)
        in_subrobj = None
        checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==in_subrname
        if not part_has_node(node, SUBP_PART, checks):

            checks = lambda n: n.kgen_match_class==statements.Contains
            if not node in state_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
                part_append_comment(node, CONTAINS_PART, '')
                part_append_gensnode(node, CONTAINS_PART, statements.Contains)
                part_append_comment(node, CONTAINS_PART, '')
                state_gencore_contains.append(node)

            attrs = {'name': in_subrname, 'args': ['kgen_unit']}
            part_append_comment(node, SUBP_PART, 'write in state subroutine for %s'%in_subrname)
            in_subrobj = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
            part_append_comment(node, SUBP_PART, '')

            # kgen_unit
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
            part_append_gensnode(in_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            # kgen_istrue
            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
            part_append_gensnode(in_subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
            part_append_gensnode(in_subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

            part_append_comment(in_subrobj, DECL_PART, '')

            # add public stmt
            attrs = {'items':[in_subrname]}
            part_append_gensnode(node, DECL_PART, statements.Public, attrs=attrs)

        out_subrobj = None
        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state_out(node.kgen_stmt.geninfo):
            out_subrname = get_module_out_writename(node.kgen_stmt)
            checks = lambda n: isinstance(n.kgen_stmt, block_statements.Subroutine) and n.name==out_subrname
            if not part_has_node(node, SUBP_PART, checks):

                checks = lambda n: n.kgen_match_class==statements.Contains
                if not node in state_gencore_contains and not part_has_node(node, CONTAINS_PART, checks):
                    part_append_comment(node, CONTAINS_PART, '')
                    part_append_gensnode(node, CONTAINS_PART, statements.Contains)
                    part_append_comment(node, CONTAINS_PART, '')
                    state_gencore_contains.append(node)

                attrs = {'name': out_subrname, 'args': ['kgen_unit']}
                part_append_comment(node, SUBP_PART, 'write out state subroutine for %s'%out_subrname)
                out_subrobj = part_append_gensnode(node, SUBP_PART, block_statements.Subroutine, attrs=attrs)
                part_append_comment(node, SUBP_PART, '')

                # kgen_unit
                attrs = {'type_spec': 'INTEGER', 'attrspec': ['INTENT(IN)'], 'entity_decls': ['kgen_unit']}
                part_append_gensnode(out_subrobj, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                # kgen_istrue
                attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_istrue']}
                part_append_gensnode(out_subrobj, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'REAL', 'entity_decls': ['kgen_array_sum'], 'selector': (None, '8')}
                part_append_gensnode(out_subrobj, DECL_PART, typedecl_statements.Real, attrs=attrs)

                part_append_comment(out_subrobj, DECL_PART, '')

                # add public stmt
                attrs = {'items':[out_subrname]}
                part_append_gensnode(node, DECL_PART, statements.Public, attrs=attrs)

        if in_subrobj or out_subrobj:
            self.state_externs_subrs[node] = (in_subrobj, out_subrobj)

            node.kgen_stmt.top.used4genstate = True

            # register event per typedecl 
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
                typedecl_statements.TypeDeclarationStatement, self.is_extern_in_state_module, self.create_subr_write_typedecl_in_module) 

            # register event per module
            self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
                block_statements.Module, self.has_externs_in_module, self.create_state_stmts_in_callsite) 
        else:
            raise Exception('Dupulicated state extern subroutine name for module: %s. Please ensure that KGen-generated source file NOT re-used.'%node.name)

    def create_kernel_stmts_in_callsite(self, node):
        if not self.kernel_externs_subrs[node][0] in self.kernel_callsite_use_stmts:
            attrs = {'name':node.name, 'isonly': True, 'items':[self.kernel_externs_subrs[node][0].name]}
            namedpart_append_genknode(node.kgen_kernel_id, DRIVER_USE_PART, statements.Use, attrs=attrs)
            self.kernel_callsite_use_stmts.append(self.kernel_externs_subrs[node][0])

        if not self.kernel_externs_subrs[node][0] in self.kernel_callsite_call_stmts:
            attrs = {'designator': self.kernel_externs_subrs[node][0].name, 'items': ['kgen_unit']}
            namedpart_append_genknode(node.kgen_kernel_id, DRIVER_READ_IN_EXTERNS, statements.Call, attrs=attrs)
            self.kernel_callsite_call_stmts.append(self.kernel_externs_subrs[node][0])

        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state_out(node.kgen_stmt.geninfo):
            if not self.kernel_externs_subrs[node][1] in self.kernel_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
                attrs = {'name':node.name, 'isonly': True, 'items':[self.kernel_externs_subrs[node][1].name]}
                namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_USE_PART, statements.Use, attrs=attrs)
                self.kernel_callsite_use_stmts.append(self.kernel_externs_subrs[node][1])

            if not self.kernel_externs_subrs[node][1] in self.kernel_callsite_call_stmts:
                attrs = {'designator': self.kernel_externs_subrs[node][1].name, 'items': ['kgen_unit']}
                namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, statements.Call, attrs=attrs)
                self.kernel_callsite_call_stmts.append(self.kernel_externs_subrs[node][1])

#
#        if not self.kernel_externs_subrs[node][0] in self.kernel_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
#            attrs = {'name':node.name, 'isonly': True, 'items':[self.kernel_externs_subrs[node][0].name]}
#            namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_USE_PART, statements.Use, attrs=attrs)
#            self.kernel_callsite_use_stmts.append(self.kernel_externs_subrs[node][0])
#
#        if not self.kernel_externs_subrs[node][0] in self.kernel_callsite_call_stmts:
#            attrs = {'designator': self.kernel_externs_subrs[node][0].name, 'items': ['kgen_unit']}
#            namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_READ_IN_EXTERNS, statements.Call, attrs=attrs)
#            self.kernel_callsite_call_stmts.append(self.kernel_externs_subrs[node][0])
#
#        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state_out(node.kgen_stmt.geninfo):
#            if not self.kernel_externs_subrs[node][1] in self.kernel_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
#                attrs = {'name':node.name, 'isonly': True, 'items':[self.kernel_externs_subrs[node][1].name]}
#                namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_USE_PART, statements.Use, attrs=attrs)
#                self.kernel_callsite_use_stmts.append(self.kernel_externs_subrs[node][1])
#
#            if not self.kernel_externs_subrs[node][1] in self.kernel_callsite_call_stmts:
#                attrs = {'designator': self.kernel_externs_subrs[node][1].name, 'items': ['kgen_unit']}
#                namedpart_append_genknode(node.kgen_kernel_id, KERNEL_PBLOCK_READ_OUT_EXTERNS, statements.Call, attrs=attrs)
#                self.kernel_callsite_call_stmts.append(self.kernel_externs_subrs[node][1])

    def create_state_stmts_in_callsite(self, node):
        kgenunit = 'kgen_unit'

        if not self.state_externs_subrs[node][0] in self.state_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
            attrs = {'name':node.name, 'isonly': True, 'items':[self.state_externs_subrs[node][0].name]}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, statements.Use, attrs=attrs)
            self.state_callsite_use_stmts.append(self.state_externs_subrs[node][0])

        if not self.state_externs_subrs[node][0] in self.state_callsite_call_stmts:
            attrs = {'designator': self.state_externs_subrs[node][0].name, 'items': [kgenunit]}
            namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_WRITE_IN_EXTERNS, statements.Call, attrs=attrs)
            self.state_callsite_call_stmts.append(self.state_externs_subrs[node][0])

        if hasattr(node.kgen_stmt, 'geninfo') and KGGenType.has_state_out(node.kgen_stmt.geninfo):
            if not self.state_externs_subrs[node][1] in self.state_callsite_use_stmts and node.name!=getinfo('topblock_stmt').name:
                attrs = {'name':node.name, 'isonly': True, 'items':[self.state_externs_subrs[node][1].name]}
                namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_USE_PART, statements.Use, attrs=attrs)
                self.state_callsite_use_stmts.append(self.state_externs_subrs[node][1])

            if not self.state_externs_subrs[node][1] in self.state_callsite_call_stmts:
                attrs = {'designator': self.state_externs_subrs[node][1].name, 'items': [kgenunit]}
                namedpart_append_gensnode(node.kgen_kernel_id, STATE_PBLOCK_WRITE_OUT_EXTERNS, statements.Call, attrs=attrs)
                self.state_callsite_call_stmts.append(self.state_externs_subrs[node][1])

    def create_subr_read_typedecl_in_module(self, node):

        parent = node.kgen_parent
        stmt = node.kgen_stmt
        raw_entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state(stmt.geninfo)])
        entity_names = [ e for e in raw_entity_names if not stmt.get_variable(e).is_parameter() ]
        
        raw_out_entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])
        out_entity_names = [ e for e in raw_out_entity_names if not stmt.get_variable(e).is_parameter() ]

        def get_attrs(attrspec, allowed_attrs):
            attrspec = []
            for attr in stmt.attrspec:
                if any( attr.startswith(allowed_attr) for allowed_attr in allowed_attrs):
                    attrspec.append(attr)
            return attrspec

        def get_decls(names, decls, prefix=''):
            import re
            entity_decls = []
            for decl in decls:
                ename = re.split('\(|\*|=', decl)[0].strip()
                if ename in names:
                    entity_decls.append(prefix+decl)
            return entity_decls

        if len(entity_names)==0:
            node.kgen_forced_line = False
        elif len(entity_names)!=len(stmt.entity_decls):
            attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable', 'dimension', 'public', 'target'])
            entity_decls = get_decls(entity_names, stmt.entity_decls)

            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, \
                'selector':stmt.selector, 'entity_decls': entity_decls}

            if stmt.is_derived():
                node.type_spec = 'TYPE'
            else:
                node.type_spec = stmt.__class__.__name__.upper()
            node.attrspec = attrspec
            node.selector = stmt.selector
            node.entity_decls = entity_decls
            node.kgen_use_tokgen = True
            #part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)
            #part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)

        if len(out_entity_names)>0:
            attrspec = get_attrs(stmt.attrspec, ['pointer', 'allocatable', 'dimension'])

            entity_decls = get_decls(out_entity_names, stmt.entity_decls, prefix='kgenref_')

            attrs = {'type_spec': stmt.__class__.__name__.upper(), 'attrspec': attrspec, \
                'selector':stmt.selector, 'entity_decls': entity_decls}
            part_append_genknode(node.kgen_parent, DECL_PART, stmt.__class__, attrs=attrs)

        is_class_derived = check_class_derived(stmt)

        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            if node.kgen_parent.name+entity_name in self.kernel_extern_reads: continue

            if is_remove_state(entity_name, stmt): continue

            self.kernel_extern_reads.append(node.kgen_parent.name+entity_name)

            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_readname(stmt, entity_name)

            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                    if entity_name in out_entity_names:
                        self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var, prefix='kgenref_')
                    if subrname not in self.kernel_created_subrs:
                        create_read_subr(subrname, entity_name, parent, var, stmt)
                        self.kernel_created_subrs.append(subrname)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        self.create_read_intrinsic(self.kernel_externs_subrs[node.kgen_parent][0], entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_read_intrinsic(self.kernel_externs_subrs[node.kgen_parent][1], entity_name, stmt, var, prefix='kgenref_')
                    else: # implicit array
                        self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var, prefix='kgenref_')
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, parent, var, stmt)
                            self.kernel_created_subrs.append(subrname)
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    if var.is_allocatable() or var.is_pointer():
                        self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var, prefix='kgenref_')
                        if subrname not in self.kernel_created_subrs:
                            create_read_subr(subrname, entity_name, parent, var, stmt)
                            self.kernel_created_subrs.append(subrname)
                    else:
                        subrname = None
                        for uname, req in stmt.unknowns.iteritems():
                            if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                            #if uname.firstpartname()==stmt.name:
                                if len(req.res_stmts)>0:
                                    res = req.res_stmts[0]
                                    subrname = get_dtype_readname(res)
                                    break
                        if subrname is None:
                            print 'WARNING: Can not find Type resolver for %s'%stmt.name
                            namedpart_append_comment(self.kernel_externs_subrs[node.kgen_parent][0], EXEC_PART, \
                                'ERROR: "%s" is not resolved. Call statements to read "%s" is not created here.'%\
                                (stmt.name, stmt.name))
                        else:
                            self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                            if entity_name in out_entity_names:
                                self.create_read_call(self.kernel_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var, prefix='kgenref_')
                else: # intrinsic type
                    self.create_read_intrinsic(self.kernel_externs_subrs[node.kgen_parent][0], entity_name, stmt, var)
                    if entity_name in out_entity_names:
                        self.create_read_intrinsic(self.kernel_externs_subrs[node.kgen_parent][1], entity_name, stmt, var, prefix='kgenref_')

    def create_subr_write_typedecl_in_module(self, node):
        parent = node.kgen_parent
        stmt = node.kgen_stmt

        raw_entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state(stmt.geninfo)])
        entity_names = [ e for e in raw_entity_names if not stmt.get_variable(e).is_parameter() ]
        
        raw_out_entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])
        out_entity_names = [ e for e in raw_out_entity_names if not stmt.get_variable(e).is_parameter() ]

        #entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state(stmt.geninfo)])
        #out_entity_names = set([ uname.firstpartname() for uname, req in KGGenType.get_state_out(stmt.geninfo)])

        is_class_derived = check_class_derived(stmt)

        for entity_name, entity_decl in zip(entity_names, stmt.entity_decls):
            if node.kgen_parent.name+entity_name in self.state_extern_writes: continue
            if is_remove_state(entity_name, stmt): continue

            self.state_extern_writes.append(node.kgen_parent.name+entity_name)

            var = stmt.get_variable(entity_name)
            subrname = get_typedecl_writename(stmt, entity_name)

            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    self.create_write_call(self.state_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                    if entity_name in out_entity_names:
                        self.create_write_call(self.state_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var)
                    if subrname not in self.state_created_subrs:
                        create_write_subr(subrname, entity_name, parent, var, stmt)
                        self.state_created_subrs.append(subrname)
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        self.create_write_intrinsic(self.state_externs_subrs[node.kgen_parent][0], entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_write_intrinsic(self.state_externs_subrs[node.kgen_parent][1], entity_name, stmt, var)
                    else: # implicit array
                        self.create_write_call(self.state_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_write_call(self.state_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var)
                        if subrname not in self.state_created_subrs:
                            create_write_subr(subrname, entity_name, parent, var, stmt)
                            self.state_created_subrs.append(subrname)
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    if var.is_allocatable() or var.is_pointer():
                        self.create_write_call(self.state_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                        if entity_name in out_entity_names:
                            self.create_write_call(self.state_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var)
                        if subrname not in self.state_created_subrs:
                            create_write_subr(subrname, entity_name, parent, var, stmt)
                            self.state_created_subrs.append(subrname)
                    else:
                        subrname = None
                        for uname, req in stmt.unknowns.iteritems():
                            if ( is_class_derived and uname.firstpartname()==stmt.selector[1]) or uname.firstpartname()==stmt.name:
                            #if uname.firstpartname()==stmt.name:
                                if len(req.res_stmts)>0:
                                    res = req.res_stmts[0]
                                    subrname = get_dtype_writename(res)
                                    break
                        if subrname is None:
                            print 'WARNING: Can not find Type resolver for %s'%stmt.name
                            namedpart_append_comment(self.state_externs_subrs[node.kgen_parent][0], EXEC_PART, \
                                'ERROR: "%s" is not resolved. Call statements to write "%s" is not created here.'%\
                                (stmt.name, stmt.name))
                        else:
                            self.create_write_call(self.state_externs_subrs[node.kgen_parent][0], subrname, entity_name, stmt, var)
                            if entity_name in out_entity_names:
                                self.create_write_call(self.state_externs_subrs[node.kgen_parent][1], subrname, entity_name, stmt, var)
                else: # intrinsic type
                    self.create_write_intrinsic(self.state_externs_subrs[node.kgen_parent][0], entity_name, stmt, var)
                    if entity_name in out_entity_names:
                        self.create_write_intrinsic(self.state_externs_subrs[node.kgen_parent][1], entity_name, stmt, var)

    def create_read_intrinsic(self, subrobj, entity_name, stmt, var, prefix=''):

        pobj = gen_read_istrue(subrobj, var, entity_name)

        if var.is_allocatable() or var.is_pointer():
            attrs = {'items': ['%s'%(prefix+entity_name)]}
            part_append_genknode(pobj, EXEC_PART, statements.Allocate, attrs=attrs)

        attrs = {'items': [prefix+entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

        if var.is_array() and stmt.is_numeric():
            if isinstance(stmt, typedecl_statements.Real):
                attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(prefix+entity_name), \
                    'kgen_array_sum', 'REAL(SUM(%s, mask=(%s .eq. %s)), 8)'%(prefix+entity_name, prefix+entity_name, prefix+entity_name), '.TRUE.']}
            else:
                attrs = {'designator': 'kgen_array_sumcheck', 'items': ['"%s"'%(prefix+entity_name), \
                    'kgen_array_sum', 'REAL(SUM(%s), 8)'%(prefix+entity_name), '.TRUE.']}
            part_append_genknode(pobj, EXEC_PART, statements.Call, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%(prefix+entity_name), 'REAL(SUM(%s), 8)'%(prefix+entity_name)]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%(prefix+entity_name), prefix+entity_name]}
            part_append_genknode(pobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_write_intrinsic(self, subrobj, entity_name, stmt, var):

        pobj = gen_write_istrue(subrobj, var, entity_name)

        attrs = {'items': [entity_name], 'specs': ['UNIT = kgen_unit']}
        part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            if stmt.is_numeric() and var.is_array():
                attrs = {'items': ['"** KGEN DEBUG: " // "REAL(SUM(%s), 8) **"'%(prefix+entity_name), 'REAL(SUM(%s), 8)'%entity_name]}
            else:
                attrs = {'items': ['"** KGEN DEBUG: " // "%s **" // NEW_LINE("A")'%(prefix+entity_name), entity_name]}
            part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

    def create_read_call(self, subrobj, callname, entity_name, stmt, var, prefix=''):

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [prefix+entity_name, 'kgen_unit', '"%s"'%(prefix+entity_name)]}
            part_append_genknode(subrobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [prefix+entity_name, 'kgen_unit']}
            part_append_genknode(subrobj, EXEC_PART, statements.Call, attrs=attrs)

    def create_write_call(self, subrobj, callname, entity_name, stmt, var, prefix=''):

        if any(match_namepath(pattern, pack_exnamepath(stmt, entity_name), internal=False) for pattern in getinfo('print_var_names')):
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit', '"%s"'%(prefix+entity_name)]}
            part_append_gensnode(subrobj, EXEC_PART, statements.Call, attrs=attrs)
        else:
            attrs = {'designator': callname, 'items': [entity_name, 'kgen_unit']}
            part_append_gensnode(subrobj, EXEC_PART, statements.Call, attrs=attrs)

