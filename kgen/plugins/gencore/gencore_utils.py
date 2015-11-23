# gencore_utils.py

gencore_parts = {}

DRIVER_IN_LOCAL_PART = 'driver_in_local'
DRIVER_CALLSITE_PART = 'callsite'
MODULE_EXTERNS_PART = 'module_externs'
CALLSITE_PART = 'callsite_part'
PARENTBLOCK_USE_PART = 'parentblock_use_part'
PARENTBLOCK_DECL_PART = 'parentblock_decl_part'
PARENTBLOCK_EXEC_PART ='parentblock_exec_part'
PARENTBLOCK_CONTAINS_PART = 'parentblock_contains_part'
PARENTBLOCK_SUBP_PART = 'parentblock_subp_part'
PARENTBLOCK_WRITE_IN_EXTERNS = 'parentblock_write_in_externs'
PARENTBLOCK_WRITE_IN_LOCALS ='parentblock_write_in_locals'
PARENTBLOCK_WRITE_OUT_EXTERNS = 'parentblock_write_out_externs'
PARENTBLOCK_WRITE_OUT_LOCALS = 'parentblock_write_out_locals'
TOPBLOCK_USE_PART = 'topblock_use_part'
TOPBLOCK_DECL_PART = 'topblock_decl_part'
TOPBLOCK_CONTAINS_PART = 'topblock_contains_part'
TOPBLOCK_SUBP_PART = 'topblock_subp_part'

rprefix = 'kr'
wprefix = 'kw'
vprefix = 'kv'

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

    assert isinstance(stmt, typedecl_statements.TypeDeclarationStatement), 'None type of typedecl stmt'
    assert entity_name, 'No entity name is provided.'

    prefix = [ get_parentname(stmt), stmt.name ] + list(stmt.selector)

    var = stmt.get_variable(entity_name)
    if var is None: return 'Unknown_name'

    l = []
    if var.is_array(): l.append('dim%d'%var.rank)
    if var.is_pointer(): l.append('ptr')

    return '_'.join(prefix+l)

def get_dtype_writename(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(wprefix, subpname)

def get_module_writename(modstmt):
    if modstmt is None: return
    return '%s_externs_%s'%(wprefix, modstmt.name)

