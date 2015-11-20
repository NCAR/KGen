# gencore_utils.py

DRIVER_IN_LOCAL_PART = 'driver_in_local'
DRIVER_CALLSITE_PART = 'callsite'

gencore_parts = {}

rprefix = 'kr'
wprefix = 'kw'
vprefix = 'kv'

def get_topname(stmt):
    assert stmt and hasattr(stmt, 'parent')
    topstmt = stmt.ancestors()[0]
    if hasattr(topstmt, 'name'):
        return topstmt.name 
    else: return ''

def get_dtype_subpname(typestmt):
    assert typestmt, 'None type of typestmt'
    return '%s_%s'%(get_topname(typestmt), typestmt.name)


def get_dtype_writename(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(wprefix, subpname)
