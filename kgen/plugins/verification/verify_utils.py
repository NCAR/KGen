# gen_core_utils.py

shared_objects = {}

kernel_verify_parts = {}
kernel_verify_contains = []

VERIFY_PBLOCK_USE_PART = 'VPBUP'
VERIFY_PBLOCK_DECL_PART = 'VPBDP'
VERIFY_PBLOCK_EXEC_PART ='VPBEP'
VERIFY_PBLOCK_CONTAINS_PART = 'VPBCP'
VERIFY_PBLOCK_SUBP_PART = 'VPBSP'
VERIFY_PBLOCK_INIT = 'VPBI'
VERIFY_PBLOCK_EXTERNS = 'VPBE'
VERIFY_PBLOCK_LOCALS ='VPBL'

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

def get_module_verifyname(modstmt):
    if modstmt is None: return
    return '%s_externs_%s'%(vprefix, modstmt.name)

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

def get_dtype_verifyname(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(vprefix, subpname)

def get_typedecl_verifyname(typestmt, entity_name):
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

        if hasattr(spec_stmt, 'items'):
            new_items = []
            unames = list(set([ uname.firstpartname() for uname, req in KGGenType.get_state(spec_stmt.geninfo) ]))
            for item in spec_stmt.items:
                if any(item.startswith(uname) for uname in unames):
                    new_items.append(item)
            node.new_items = new_items
            node.kgen_use_tokgen = True
        else:
            pass
            # maybe specific handling per classes
