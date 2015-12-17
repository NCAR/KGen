# gen_core_utils.py

shared_objects = {}
shared_objects['driver_object'] = None

state_gencore_parts = {}
state_gencore_contains = []

kernel_gencore_parts = {}
kernel_gencore_contains = []

DRIVER_USE_PART = 'DUP'
DRIVER_DECL_PART = 'DDP'
DRIVER_EXEC_PART ='DEP'
DRIVER_CONTAINS_PART = 'DCP'
DRIVER_SUBP_PART = 'DSP'
DRIVER_ALLOC_PART = 'DAP'
DRIVER_DEALLOC_PART = 'DDAP'
DRIVER_READ_IN_ARGS = 'DRIA'
DRIVER_CALLSITE_PART = 'DCP'

STATE_PBLOCK_USE_PART = 'SPBUP'
STATE_PBLOCK_DECL_PART = 'SPBDP'
STATE_PBLOCK_EXEC_PART ='SPBEP'
STATE_PBLOCK_CONTAINS_PART = 'SPBCP'
STATE_PBLOCK_SUBP_PART = 'SPBSP'
STATE_PBLOCK_WRITE_IN_ARGS = 'SPBWIA'
STATE_PBLOCK_WRITE_IN_EXTERNS = 'SPBWIE'
STATE_PBLOCK_WRITE_IN_LOCALS ='SPBWIL'
STATE_PBLOCK_WRITE_OUT_EXTERNS = 'SPBWOE'
STATE_PBLOCK_WRITE_OUT_LOCALS = 'SPBWOL'

STATE_TBLOCK_USE_PART = 'STBUP'
STATE_TBLOCK_DECL_PART = 'STBDP'
STATE_TBLOCK_CONTAINS_PART = 'STBCP'
STATE_TBLOCK_SUBP_PART = 'STBSP'

KERNEL_PBLOCK_USE_PART = 'KPBUP'
KERNEL_PBLOCK_DECL_PART = 'KPBDP'
KERNEL_PBLOCK_EXEC_PART ='KPBEP'
KERNEL_PBLOCK_CONTAINS_PART = 'KPBCP'
KERNEL_PBLOCK_SUBP_PART = 'KPBSP'
KERNEL_PBLOCK_READ_IN_EXTERNS = 'KPBRIE'
KERNEL_PBLOCK_READ_IN_LOCALS ='KPBRIL'
KERNEL_PBLOCK_READ_OUT_EXTERNS = 'KPBROE'
KERNEL_PBLOCK_READ_OUT_LOCALS = 'KPBROL'

KERNEL_TBLOCK_USE_PART = 'KTBUP'
KERNEL_TBLOCK_DECL_PART = 'KTBDP'
KERNEL_TBLOCK_CONTAINS_PART = 'KTBCP'
KERNEL_TBLOCK_SUBP_PART = 'KTBSP'

rprefix = 'kr'
wprefix = 'kw'
vprefix = 'kv'

MAXLEN_SUBPNAME = 55

def get_ancestor_name(stmt, generation):
    assert stmt and hasattr(stmt, 'parent'), 'Given stmt does not have parent attribute.'
    assert isinstance(generation, int), 'Not integer type of generation.'

    ancestor = stmt.ancestors()[generation]
    if hasattr(ancestor, 'name'):
        return ancestor.name 
    else: return ''

def get_topname(stmt):
    return get_ancestor_name(stmt, 0)

def get_parentname(stmt):
    return get_ancestor_name(stmt, -1)

def get_dtype_subpname(typestmt):
    assert typestmt, 'None type of typestmt'
    return '%s_%s'%(get_topname(typestmt), typestmt.name)

def get_typedecl_subpname(stmt, entity_name):
    import typedecl_statements
    if not hasattr(get_typedecl_subpname, 'kgen_subpname_index'):
        get_typedecl_subpname.kgen_subpname_index = 0

    assert isinstance(stmt, typedecl_statements.TypeDeclarationStatement), 'None type of typedecl stmt'
    assert entity_name, 'No entity name is provided.'

    var = stmt.get_variable(entity_name)
    if var is None: return 'Unknown_name'

    prefix = [ get_parentname(stmt), stmt.name ] + list(stmt.selector)
    l = []
    if var.is_array(): l.append('dim%d'%var.rank)
    if var.is_pointer(): l.append('ptr')

    subpname = '_'.join(prefix+l)
    if len(subpname)<MAXLEN_SUBPNAME:
        return '_'.join(prefix+l)
    else:
        get_typedecl_subpname.kgen_subpname_index += 1
        return 'kgen_subpname_%d'%get_typedecl_subpname.kgen_subpname_index

def get_dtype_writename(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(wprefix, subpname)

def get_dtype_readname(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(rprefix, subpname)

def get_module_in_writename(modstmt):
    if modstmt is None: return
    return '%s_externs_in_%s'%(wprefix, modstmt.name)

def get_module_out_writename(modstmt):
    if modstmt is None: return
    return '%s_externs_out_%s'%(wprefix, modstmt.name)

def get_module_in_readname(modstmt):
    if modstmt is None: return
    return '%s_externs_in_%s'%(rprefix, modstmt.name)

def get_module_out_readname(modstmt):
    if modstmt is None: return
    return '%s_externs_out_%s'%(rprefix, modstmt.name)

def get_typedecl_writename(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(wprefix, subpname)

def get_typedecl_readname(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(rprefix, subpname)

def get_typedecl_verifyename(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(vprefix, subpname)

def process_spec_stmts(stmt):
    if not stmt: return
    if not hasattr(stmt, 'spec_stmts'): return

    for spec_stmt in stmt.spec_stmts:
        node = spec_stmt.genkpair
        if not node: continue
        if not node.kgen_isvalid: continue
        if not hasattr(spec_stmt, 'geninfo') or len(spec_stmt.geninfo)==0: continue

        def is_uname(item, unames):
            import re
            iname = re.split('\(|\*|=', item)[0].strip()
            if iname in unames: return True
            else: return False


        if hasattr(spec_stmt, 'items'):
            new_items = []
            unames = list(set([ uname.firstpartname() for uname, req in KGGenType.get_state(spec_stmt.geninfo) ]))
            for item in spec_stmt.items:
                if is_uname(item, unames):
                    new_items.append(item)
            node.new_items = new_items
            node.kgen_use_tokgen = True
        else:
            pass
            # maybe specific handling per classes
