# gencore_utils.py

DRIVER_IN_LOCAL_PART = 'driver_in_local'
DRIVER_CALLSITE_PART = 'callsite'

gencore_parts = {}

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
