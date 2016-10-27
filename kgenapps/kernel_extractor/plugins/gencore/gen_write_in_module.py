# gen_write_typedecl_in_module.py

import os 
import re 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin
from gencore_utils import is_zero_array, check_class_derived

class Gen_Write_In_Module(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register initial events
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.FINISH_PROCESS, \
            statements.Comment, self.is_write_directive, self.write_state) 

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            statements.Comment, self.is_write_directive, self.read_state) 

    def is_write_directive(self, node):
        return hasattr(node.kgen_stmt, 'write_state')

    def get_orgstmt(self, parts, stmt):

        if len(parts) == 0:
            raise Exception('Wrong number of parts.')

        match = re.match(r'\w+[^\w]*', parts[0]) 
        if match:
            varname = match.group(0)
            for unknown, res in stmt.unknowns.items():
                if unknown.firstpartname() == stmt.name and isinstance(res.res_stmts[0], block_statements.Type):
                    res_stmt = res.res_stmts[0]
                    if len(parts) == 1:
                        return res_stmt.a.variables[varname]
                    else:
                        return self.get_orgstmt(parts[1:], res_stmt)
        raise Exception('No matched statment is found.')

    def add_useonlyname(self, ancs, unk, res_stmts):
        raise Exception('TODO: support for importing selector')

    def write_state(self, node):
        index, partname, part = get_part_index(node)
        pstmt = node.kgen_stmt.ancestors()[-1]
        pnode = pstmt.genspair
        node.kgen_stmt.top.used4genstate = True
        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        if not hasattr(node, '__write_commonpart_statewrite'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_write_unit']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'DIMENSION(0:1023)' ], 'entity_decls': ['kgen_resetinvoke']}
                part_append_genknode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:1023)' ], 'entity_decls': ['kgen_openmp_issave']}
                part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
                part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            else:
                attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_resetinvoke']}
                part_append_genknode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_openmp_issave']}
                part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmp_issave', 'kgen_resetinvoke') ) ]}
            part_append_genknode(pnode, DECL_PART, statements.Common, attrs=attrs)

            part_append_comment(pnode, DECL_PART, '')
            node.__write_commonpart_statewrite = True

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_writesubp_invoke_L%d = 0'%lineno]}
        part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_write_filepath_L%d'%lineno], 'selector':('1024', None)}
        part_append_genknode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)

        idx = index + 1

        if getinfo('is_openmp_app'):
            attrs = {'expr': 'kgen_resetinvoke(OMP_GET_THREAD_NUM())'}
            ifreset = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
            idx += 1

            attrs = {'expr': 'kgen_openmp_issave(OMP_GET_THREAD_NUM()) .GE. 0'}
            l = [ 'kgen_mpirank', '"."', 'OMP_GET_THREAD_NUM()', '"."', 'kgen_openmp_issave(OMP_GET_THREAD_NUM())', '"."', 'kgen_writesubp_invoke_L%d'%lineno]
        else:
            attrs = {'expr': 'kgen_resetinvoke'}
            ifreset = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
            idx += 1

            attrs = {'expr': 'kgen_openmp_issave .GE. 0'}
            l = [ 'kgen_mpirank', '"."', '0', '"."', 'kgen_openmp_issave', '"."', 'kgen_writesubp_invoke_L%d'%lineno]

        ifsave = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)

        # file open
        attrs = {'specs': ['kgen_write_filepath_L%d'%lineno, 'FMT="(A,I0,A,I0,A,I0,A,I0)"' ], 'items': [ '"%s/%s.L%d."'%(getinfo('kernel_path'), filename, lineno) ] + l}
        part_append_genknode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=kgen_write_unit', 'FILE=kgen_write_filepath_L%d'%lineno, 'STATUS="NEW"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_genknode(ifsave, EXEC_PART, statements.Open, attrs=attrs)


        vars = []
        for varstr in node.kgen_stmt.write_state:
            parts = varstr.split('%')
            if len(parts) > 1:
                # search through use stmts until the leaf stmt
                partstmt = node.kgen_stmt

                match = re.match(r'\w+[^\w]*', parts[0]) 
                if match:
                    varname = match.group(0)
                    for unknown, res in partstmt.unknowns.items():
                        if unknown.firstpartname() == varname and res.res_stmts[0].is_derived():
                            partstmt = res.res_stmts[0]
                            break

                var = self.get_orgstmt(parts[1:], partstmt)
                vars.append((varstr, var))
            else:
                match = re.match(r'\w+[^\w]*', varstr.strip()) 
                if match:
                    varname = match.group(0)
                    for unknown, res in node.kgen_stmt.unknowns.items():
                        if unknown.firstpartname() == varname: 
                            res_stmt = res.res_stmts[0]
                            var = res_stmt.get_variable(varname)
                            vars.append((varstr, var))
                            break

        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)
            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    parts = varstr.split('%')
                    parts[-1] = var.name
                    newvarstr = '%'.join(parts)

                    for rank in range(var.rank):
                        attrs = {'items': [ 'LBOUND( %s, %d )'%(newvarstr, rank+1) ], 'specs': ['UNIT = kgen_write_unit']}
                        part_append_genknode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

                        attrs = {'items': [ 'UBOUND( %s, %d )'%(newvarstr, rank+1) ], 'specs': ['UNIT = kgen_write_unit']}
                        part_append_genknode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

                    attrs = {'items': [ newvarstr ] , 'specs': ['UNIT = kgen_write_unit']}
                    part_append_genknode(ifsave, EXEC_PART, statements.Write, attrs=attrs)
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    attrs = {'items': [var.name], 'specs': ['UNIT = kgen_write_unit']}
                    part_append_genknode(ifsave, EXEC_PART, statements.Write, attrs=attrs)


        # file close
        attrs = {'specs': ['UNIT=kgen_write_unit']}
        part_append_genknode(ifsave, EXEC_PART, statements.Close, attrs=attrs)

        attrs = {'variable': 'kgen_writesubp_invoke_L%d'%lineno, 'sign': '=', 'expr': 'kgen_writesubp_invoke_L%d + 1'%lineno}
        part_append_genknode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

        part_insert_comment(node.kgen_parent, EXEC_PART, idx+1, '')

    def read_state(self, node):
        index, partname, part = get_part_index(node)
        ancs = node.kgen_stmt.ancestors()
        pstmt = ancs[-1]
        pnode = pstmt.genkpair
        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        if not hasattr(node, '__write_commonpart_stateread'):
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_read_unit = -1', 'cur_mpirank = -1']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)


            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_evalstage', 'kgen_warmupstage', 'kgen_mainstage']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

            attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke', 'kgen_evalstage', 'kgen_warmupstage', 'kgen_mainstage') ) ]}
            part_append_genknode(pnode, DECL_PART, statements.Common, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ldim1', 'kgen_udim1', 'kgen_ldim2', 'kgen_udim2', \
                'kgen_ldim3', 'kgen_udim3', 'kgen_ldim4', 'kgen_udim4', 'kgen_ldim5', 'kgen_udim5' ]}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], \
                'entity_decls': ['kgen_mindim1 = HUGE(0)', 'kgen_maxdim1 = 0', 'kgen_mindim2 = HUGE(0)', 'kgen_maxdim2 = 0', \
                'kgen_mindim3 = HUGE(0)', 'kgen_maxdim3 = 0', 'kgen_mindim4 = HUGE(0)', 'kgen_maxdim4 = 0', 'kgen_mindim5 = HUGE(0)', 'kgen_maxdim5 = 0' ]}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            node.__write_commonpart_stateread = True

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_readsubp_invokepath_L%d = 0'%lineno, \
            'kgen_readsubp_invoke_L%d = 0'%lineno, 'kgen_readsubp_maxinvoke_L%d = 0'%lineno ]}
        part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_read_filepath_L%d'%lineno], 'selector':('128', None)}
        part_append_genknode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)

        # collect variables for manual state generation
        vars = []
        for varstr in node.kgen_stmt.write_state:
            parts = varstr.split('%')
            if len(parts) > 1:
                # search through use stmts until the leaf stmt
                partstmt = node.kgen_stmt

                match = re.match(r'\w+[^\w]*', parts[0]) 
                if match:
                    varname = match.group(0)
                    for unknown, res in partstmt.unknowns.items():
                        if unknown.firstpartname() == varname and res.res_stmts[0].is_derived():
                            partstmt = res.res_stmts[0]
                            break

                var = self.get_orgstmt(parts[1:], partstmt)
                vars.append((varstr, var))
            else:
                match = re.match(r'\w+[^\w]*', varstr.strip()) 
                if match:
                    varname = match.group(0)
                    for unknown, res in node.kgen_stmt.unknowns.items():
                        if unknown.firstpartname() == varname: 
                            res_stmt = res.res_stmts[0]
                            var = res_stmt.get_variable(varname)
                            vars.append((varstr, var))
                            break

        # create typedecls for array of variable pointer
        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)

            type_spec = stmt.name.upper()
            attrspec = list(stmt.attrspec)
            if 'save' not in attrspec:
                attrspec.append('SAVE')
            if 'allocatable' not in attrspec:
                attrspec.append('ALLOCATABLE')
            excludes = []
            for spec in attrspec:
                if spec.startswith('dimension'):
                    excludes.append(spec)
                if spec.startswith('pointer'):
                    excludes.append(spec)
            for exclude in excludes:
                attrspec.remove(exclude)
            selector = tuple(stmt.selector)
            entity_decls = [ 'kgen_arr_%s_L%d'%(var.name, lineno) ]

            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    if var.is_explicit_shape_array():
                        attrspec.append('DIMENSION(%s)'%(', '.join([':']*(var.rank+1))))
                    else: # implicit array
                        attrspec.append('DIMENSION(%s)'%(', '.join([':']*(var.rank+1))))
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    attrspec.append('DIMENSION(:)')

            if hasattr(stmt, 'unknowns'):
                for unk, res in stmt.unknowns.items():
                    res_parent_stmt = res.res_stmts[0].ancestors()[-1]
                    if res_parent_stmt not in ancs:
                        if unk.firstpartname() == selector[0]:
                            self.add_useonlyname(ancs, unk, res.res_stmts)
                        elif unk.firstpartname() == selector[1]:
                            self.add_useonlyname(ancs, unk, res.res_stmts)

            attrs = {'type_spec': type_spec, 'attrspec': attrspec, 'selector': selector, 'entity_decls': entity_decls}
            part_append_genknode(pnode, DECL_PART, stmt.__class__, attrs=attrs)

        idx = index + 1

        # if eval stage
        # - count max number of local invocations
        attrs = {'expr': 'kgen_evalstage .AND. .NOT. kgen_warmupstage .AND. .NOT. kgen_mainstage'}
        ifeval = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        idx += 1

        attrs = {'expr': 'kgen_mpirank .NE. cur_mpirank' }
        ifnewstate = part_append_genknode(ifeval, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'cur_mpirank', 'sign': '=', 'expr': 'kgen_mpirank'}
        part_append_genknode(ifnewstate, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invokepath_L%d'%lineno, 'sign': '=', 'expr': '0'}
        part_append_genknode(ifnewstate, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'ALLOCATED(kgen_arr_%s_L%d)'%(var.name, lineno) }
        ifalloc = part_append_genknode(ifnewstate, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['kgen_arr_%s_L%d'%(var.name, lineno)]}
        part_append_genknode(ifalloc, EXEC_PART, statements.Deallocate, attrs=attrs)

        l = [ 'kgen_mpirank', '"."', 'kgen_openmptid', '"."', 'kgen_kernelinvoke', '"."', 'kgen_readsubp_invokepath_L%d'%lineno]
        attrs = {'specs': ['kgen_read_filepath_L%d'%lineno, 'FMT="(A,I0,A,I0,A,I0,A,I0)"' ], 'items': [ '"%s.L%d."'%(filename, lineno) ] + l}
        part_append_genknode(ifeval, EXEC_PART, statements.Write, attrs=attrs)

        # file open
        attrs = {'specs': ['NEWUNIT=kgen_read_unit', 'FILE=kgen_read_filepath_L%d'%lineno, 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_genknode(ifeval, EXEC_PART, statements.Open, attrs=attrs)

        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)
            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    parts = varstr.split('%')
                    parts[-1] = var.name
                    newvarstr = '%'.join(parts)

                    bounds = []
                    for rank in range(var.rank):
                        attrs = {'items': [ 'kgen_ldim%d'%(rank+1) ], 'specs': ['UNIT = kgen_read_unit']}
                        part_append_genknode(ifeval, EXEC_PART, statements.Read, attrs=attrs)

                        attrs = {'expr': 'kgen_ldim%d .LT. kgen_mindim%d'%(rank+1, rank+1) }
                        ifmindim = part_append_genknode(ifeval, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        attrs = {'variable': 'kgen_mindim%d'%(rank+1), 'sign': '=', 'expr': 'kgen_ldim%d'%(rank+1)}
                        part_append_genknode(ifmindim, EXEC_PART, statements.Assignment, attrs=attrs)

                        attrs = {'items': [ 'kgen_udim%d'%(rank+1) ], 'specs': ['UNIT = kgen_read_unit']}
                        part_append_genknode(ifeval, EXEC_PART, statements.Read, attrs=attrs)
                        bounds.append('kgen_ldim%d:kgen_udim%d'%(rank+1, rank+1))

                        attrs = {'expr': 'kgen_udim%d .GT. kgen_maxdim%d'%(rank+1, rank+1) }
                        ifmaxdim = part_append_genknode(ifeval, EXEC_PART, block_statements.IfThen, attrs=attrs)

                        attrs = {'variable': 'kgen_maxdim%d'%(rank+1), 'sign': '=', 'expr': 'kgen_udim%d'%(rank+1)}
                        part_append_genknode(ifmaxdim, EXEC_PART, statements.Assignment, attrs=attrs)

                    attrs = {'items': [ '%s(%s)'%(newvarstr, ', '.join(bounds)) ] , 'specs': ['UNIT = kgen_read_unit']}
                    part_append_genknode(ifeval, EXEC_PART, statements.Read, attrs=attrs)

            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    attrs = {'specs': ['UNIT=kgen_read_unit'], 'items': [ var.name ]}
                    part_append_genknode(ifeval, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'specs': ['UNIT=kgen_read_unit']}
        part_append_genknode(ifeval, EXEC_PART, statements.Close, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invokepath_L%d'%lineno, 'sign': '=', 'expr': 'kgen_readsubp_invokepath_L%d + 1'%lineno}
        part_append_genknode(ifeval, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_maxinvoke_L%d'%lineno, 'sign': '=', 'expr': 'kgen_readsubp_maxinvoke_L%d + 1'%lineno}
        part_append_genknode(ifeval, EXEC_PART, statements.Assignment, attrs=attrs)

        # if warmup stage
        # - allocate an array and save data variable into the array
        attrs = {'expr': '.NOT. kgen_evalstage .AND. kgen_warmupstage .AND. .NOT. kgen_mainstage'}
        ifwarmup = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        idx += 1

        attrs = {'expr': '.NOT. ALLOCATED(kgen_arr_%s_L%d)'%(var.name, lineno) }
        ifnoalloc = part_append_genknode(ifwarmup, EXEC_PART, block_statements.IfThen, attrs=attrs)

        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)
            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    parts = varstr.split('%')
                    parts[-1] = var.name
                    newvarstr = '%'.join(parts)

                    bounds = []
                    for rank in range(var.rank):
                        bounds.append('kgen_mindim%d:kgen_maxdim%d'%(rank+1, rank+1))

                    attrs = {'items': ['kgen_arr_%s_L%d(kgen_readsubp_maxinvoke_L%d, %s)'%(var.name, lineno, lineno, ', '.join(bounds))]}
                    part_append_genknode(ifnoalloc, EXEC_PART, statements.Allocate, attrs=attrs)
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    pass
                attrs = {'items': ['kgen_arr_%s_L%d(kgen_readsubp_maxinvoke_L%d)'%(var.name, lineno, lineno)]}
                part_append_genknode(ifnoalloc, EXEC_PART, statements.Allocate, attrs=attrs)


        attrs = {'variable': 'cur_mpirank', 'sign': '=', 'expr': '-1'}
        part_append_genknode(ifnoalloc, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invoke_L%d'%lineno, 'sign': '=', 'expr': '0'}
        part_append_genknode(ifnoalloc, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'kgen_mpirank .NE. cur_mpirank' }
        ifnewstate = part_append_genknode(ifwarmup, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'cur_mpirank', 'sign': '=', 'expr': 'kgen_mpirank'}
        part_append_genknode(ifnewstate, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invokepath_L%d'%lineno, 'sign': '=', 'expr': '0'}
        part_append_genknode(ifnewstate, EXEC_PART, statements.Assignment, attrs=attrs)

        l = [ 'kgen_mpirank', '"."', 'kgen_openmptid', '"."', 'kgen_kernelinvoke', '"."', 'kgen_readsubp_invokepath_L%d'%lineno]
        attrs = {'specs': ['kgen_read_filepath_L%d'%lineno, 'FMT="(A,I0,A,I0,A,I0,A,I0)"' ], 'items': [ '"%s.L%d."'%(filename, lineno) ] + l}
        part_append_genknode(ifwarmup, EXEC_PART, statements.Write, attrs=attrs)

        # file open
        attrs = {'specs': ['NEWUNIT=kgen_read_unit', 'FILE=kgen_read_filepath_L%d'%lineno, 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_genknode(ifwarmup, EXEC_PART, statements.Open, attrs=attrs)

        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)
            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    parts = varstr.split('%')
                    parts[-1] = var.name
                    newvarstr = '%'.join(parts)

                    bounds = []
                    for rank in range(var.rank):
                        attrs = {'items': [ 'kgen_ldim%d'%(rank+1) ], 'specs': ['UNIT = kgen_read_unit']}
                        part_append_genknode(ifwarmup, EXEC_PART, statements.Read, attrs=attrs)

                        attrs = {'items': [ 'kgen_udim%d'%(rank+1) ], 'specs': ['UNIT = kgen_read_unit']}
                        part_append_genknode(ifwarmup, EXEC_PART, statements.Read, attrs=attrs)
                        bounds.append('kgen_ldim%d:kgen_udim%d'%(rank+1, rank+1))

                    attrs = {'items': [ '%s(%s)'%(newvarstr, ', '.join(bounds)) ] , 'specs': ['UNIT = kgen_read_unit']}
                    part_append_genknode(ifwarmup, EXEC_PART, statements.Read, attrs=attrs)

                    attrs = {'variable': 'kgen_arr_%s_L%d(kgen_readsubp_invoke_L%d + 1, %s)'%(var.name, lineno, lineno, ', '.join(bounds)), \
                        'sign': '=', 'expr': '%s( %s )'%(newvarstr, ', '.join(bounds)) }
                    part_append_genknode(ifwarmup, EXEC_PART, statements.Assignment, attrs=attrs)

            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    attrs = {'specs': ['UNIT=kgen_read_unit'], 'items': [ var.name ]}
                    part_append_genknode(ifwarmup, EXEC_PART, statements.Read, attrs=attrs)

                attrs = {'variable': 'kgen_arr_%s_L%d(kgen_readsubp_invoke_L%d + 1)'%(var.name, lineno, lineno), 'sign': '=', 'expr': var.name}
                part_append_genknode(ifwarmup, EXEC_PART, statements.Assignment, attrs=attrs)


        # file close
        attrs = {'specs': ['UNIT=kgen_read_unit']}
        part_append_genknode(ifwarmup, EXEC_PART, statements.Close, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invokepath_L%d'%lineno, 'sign': '=', 'expr': 'kgen_readsubp_invokepath_L%d + 1'%lineno}
        part_append_genknode(ifwarmup, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invoke_L%d'%lineno, 'sign': '=', 'expr': 'kgen_readsubp_invoke_L%d + 1'%lineno}
        part_append_genknode(ifwarmup, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'kgen_readsubp_invoke_L%d .EQ. kgen_readsubp_maxinvoke_L%d'%(lineno,lineno) }
        ifmax = part_append_genknode(ifwarmup, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invoke_L%d'%lineno, 'sign': '=', 'expr': '0'}
        part_append_genknode(ifmax, EXEC_PART, statements.Assignment, attrs=attrs)

        # if main stage
        # - copy data from array to data variable

        attrs = {'expr': '.NOT. kgen_evalstage .AND. .NOT. kgen_warmupstage .AND. kgen_mainstage'}
        ifmain = part_insert_genknode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        idx += 1

        for varstr, var in vars:
            stmt = var.parent
            is_class_derived = check_class_derived(stmt)
            if var.is_array():
                if is_zero_array(var, stmt): continue
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    parts = varstr.split('%')
                    parts[-1] = var.name
                    newvarstr = '%'.join(parts)

                    bounds = []
                    for rank in range(var.rank):
                        bounds.append('LBOUND(%s, %d):UBOUND(%s, %d)'%(newvarstr, rank+1, newvarstr, rank+1))

                    attrs = {'variable': newvarstr, 'sign': '=', 'expr': 'kgen_arr_%s_L%d(kgen_readsubp_invoke_L%d + 1, %s)'%(var.name, lineno, lineno, ', '.join(bounds))}
                    part_append_genknode(ifmain, EXEC_PART, statements.Assignment, attrs=attrs)
            else: # scalar
                if stmt.is_derived() or is_class_derived:
                    raise Exception('Derived type is not supported for manual state generation yet.')
                else: # intrinsic type
                    pass

                attrs = {'variable': var.name, 'sign': '=', 'expr': 'kgen_arr_%s_L%d(kgen_readsubp_invoke_L%d)'%(var.name, lineno, lineno)}
                part_append_genknode(ifmain, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_readsubp_invoke_L%d'%lineno, 'sign': '=', 'expr': 'kgen_readsubp_invoke_L%d + 1'%lineno}
        part_append_genknode(ifmain, EXEC_PART, statements.Assignment, attrs=attrs)

        part_insert_comment(node.kgen_parent, EXEC_PART, idx, '')
