# gen_core_utils.py

from collections import OrderedDict
from parser import typedecl_statements, block_statements

shared_objects = OrderedDict()

kernel_verify_kgenutils = []
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
MAXLEN_SUBPNAME = 40

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
    if not hasattr(get_dtype_subpname, 'kgen_subpname_cache'):
        get_dtype_subpname.kgen_subpname_cache = OrderedDict()

    assert typestmt, 'None type of typestmt'

    subpname = '%s_%s'%(get_topname(typestmt), typestmt.name)

    if len(subpname)<MAXLEN_SUBPNAME:
        return subpname
    else:
        if subpname in get_dtype_subpname.kgen_subpname_cache:
            return 'kgen_%s_typesubp%d'%(get_topname(typestmt), get_dtype_subpname.kgen_subpname_cache[subpname])
        else:
            subpindex = len(get_dtype_subpname.kgen_subpname_cache)
            get_dtype_subpname.kgen_subpname_cache[subpname] = subpindex
            return 'kgen_%s_typesubp%d'%(get_topname(typestmt), subpindex)

def get_module_verifyname(modstmt):
    if modstmt is None: return
    return '%s_externs_%s'%(vprefix, modstmt.name)

def get_typedecl_subpname(stmt, entity_name):
    if not hasattr(get_typedecl_subpname, 'kgen_subpname_cache'):
        get_typedecl_subpname.kgen_subpname_cache = OrderedDict()

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
        if subpname in get_typedecl_subpname.kgen_subpname_cache:
            return 'kgen_%s_subp%d'%(get_parentname(stmt), get_typedecl_subpname.kgen_subpname_cache[subpname])
        else:
            subpindex = len(get_typedecl_subpname.kgen_subpname_cache)
            get_typedecl_subpname.kgen_subpname_cache[subpname] = subpindex
            return 'kgen_%s_subp%d'%(get_parentname(stmt), subpindex)

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

def is_excluded(ename, stmt):
    if hasattr(stmt, 'exclude_names'):
        for name, actions in stmt.exclude_names.iteritems():
            if ename==name: return True
    return False

def is_remove_state(ename, stmt):
    if hasattr(stmt, 'exclude_names'):
        for name, actions in stmt.exclude_names.iteritems():
            if ename==name and 'remove_state' in actions:
                return True
    return False

def is_remove_io(ename, stmt):
    if hasattr(stmt, 'exclude_names'):
        for name, actions in stmt.exclude_names.iteritems():
            if ename==name and 'remove_io' in actions:
                return True
    return False

def is_param_zero(length, stmt):

    if hasattr(stmt, 'unknowns'):
        for uname, req in stmt.unknowns.iteritems():
            if uname.firstpartname()==length and len(req.res_stmts)>0:
                res_stmt = req.res_stmts[0]
                if isinstance(res_stmt, typedecl_statements.Integer) and 'parameter' in res_stmt.attrspec:
                    for decl in res_stmt.entity_decls:
                        vname, value = decl.split('=')            
                        if vname.strip()==length:
                            try:
                                intlen = int(value)
                                if intlen == 0: return True
                            except:
                                if stmt is res_stmt:
                                    print ('WARNING: recursive size check of following statement\n%s'%str(res_stmt))                                
                                else:
                                    return is_param_zero(length, res_stmt)
    return False

def is_zero_array(var, stmt):
    # Temporary turn off
    #return False
    if var.is_explicit_shape_array():
        for length in var.shape:
            try:
                intlen = int(length)
                if intlen==0: return True
            except:
                if is_param_zero(length, stmt):
                    return True
    return False

def check_class_derived(stmt):

    if not stmt.is_class(): return False

    # check if the name of kind in unknows, and the last res of the unknown is derivied type
    dtypename = stmt.selector[1]
    if hasattr(stmt, 'unknowns'):
        for uname, req in stmt.unknowns.items():
            if uname.firstpartname()==dtypename and len(req.res_stmts)>0:
                if isinstance(req.res_stmts[0], (block_statements.Type, block_statements.TypeDecl)):
                    return True
    return False
