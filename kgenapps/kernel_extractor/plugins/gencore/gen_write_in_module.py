# gen_write_typedecl_in_module.py

import os 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin

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

    def write_state(self, node):
        index, partname, part = get_part_index(node)
        pstmt = node.kgen_stmt.ancestors()[-1]
        pnode = pstmt.genspair
        node.kgen_stmt.top.used4genstate = True

        if 'kgen_write_unit' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_write_unit']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            if getinfo('is_openmp_app'):
                attrs = {'type_spec': 'LOGICAL', 'attrspec': [ 'DIMENSION(0:1023)' ], 'entity_decls': ['kgen_resetinvoke']}
                part_append_gensnode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:1023)' ], 'entity_decls': ['kgen_openmp_issave']}
                part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'entity_decls': ['OMP_GET_THREAD_NUM']}
                part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            else:
                attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_resetinvoke']}
                part_append_gensnode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

                attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_openmp_issave']}
                part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmp_issave', 'kgen_resetinvoke') ) ]}
            part_append_gensnode(pnode, DECL_PART, statements.Common, attrs=attrs)

            pstmt.a.variable_names.append('kgen_write_unit')

        #if 'kgen_writesubp_invoke' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_writesubp_invoke = 0']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            #pstmt.a.variable_names.append('kgen_writesubp_invoke')

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_write_filepath'], 'selector':('1024', None)}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)

        part_append_comment(pnode, DECL_PART, '')

        idx = index + 1


        if getinfo('is_openmp_app'):
            attrs = {'expr': 'kgen_resetinvoke(OMP_GET_THREAD_NUM())'}
            ifreset = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
            idx += 1

            attrs = {'variable': 'kgen_writesubp_invoke', 'sign': '=', 'expr': '0'}
            part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_resetinvoke(OMP_GET_THREAD_NUM())', 'sign': '=', 'expr': '.FALSE.'}
            part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'expr': 'kgen_openmp_issave(OMP_GET_THREAD_NUM()) .GE. 0'}
            l = [ 'kgen_mpirank', '"."', 'OMP_GET_THREAD_NUM()', '"."', 'kgen_openmp_issave(OMP_GET_THREAD_NUM())', '"."', 'kgen_writesubp_invoke']
        else:
            attrs = {'expr': 'kgen_resetinvoke'}
            ifreset = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
            idx += 1

            attrs = {'variable': 'kgen_writesubp_invoke', 'sign': '=', 'expr': '0'}
            part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'variable': 'kgen_resetinvoke', 'sign': '=', 'expr': '.FALSE.'}
            part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

            attrs = {'expr': 'kgen_openmp_issave .GE. 0'}
            l = [ 'kgen_mpirank', '"."', '0', '"."', 'kgen_openmp_issave', '"."', 'kgen_writesubp_invoke']

        ifsave = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)

        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        # file open
        attrs = {'specs': ['kgen_write_filepath', 'FMT="(A,I0,A,I0,A,I0,A,I0)"' ], 'items': [ '"%s/%s.L%d."'%(getinfo('kernel_path'), filename, lineno) ] + l}
        part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'specs': ['NEWUNIT=kgen_write_unit', 'FILE=kgen_write_filepath', 'STATUS="NEW"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_append_gensnode(ifsave, EXEC_PART, statements.Open, attrs=attrs)

        # write var
        for var in node.kgen_stmt.write_state:
            attrs = {'specs': ['UNIT=kgen_write_unit'], 'items': [var]}
            part_append_gensnode(ifsave, EXEC_PART, statements.Write, attrs=attrs)

        # file close
        attrs = {'specs': ['UNIT=kgen_write_unit']}
        part_append_gensnode(ifsave, EXEC_PART, statements.Close, attrs=attrs)

        attrs = {'variable': 'kgen_writesubp_invoke', 'sign': '=', 'expr': 'kgen_writesubp_invoke + 1'}
        part_append_gensnode(ifsave, EXEC_PART, statements.Assignment, attrs=attrs)

        part_insert_comment(node.kgen_parent, EXEC_PART, idx+1, '')

    def read_state(self, node):
        index, partname, part = get_part_index(node)
        pstmt = node.kgen_stmt.ancestors()[-1]
        pnode = pstmt.genkpair

        if 'kgen_read_unit' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_ierr']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_read_unit']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

            pstmt.a.variable_names.append('kgen_read_unit')

        #if 'kgen_readsubp_invoke' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_readsubp_invoke = 0']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            #pstmt.a.variable_names.append('kgen_readsubp_invoke')

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_read_filepath'], 'selector':('128', None)}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)


        attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke']}
        part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_resetinvoke']}
        part_append_gensnode(pnode, DECL_PART, typedecl_statements.Logical, attrs=attrs)

        attrs = {'items': [ ( 'state', ('kgen_mpirank', 'kgen_openmptid', 'kgen_kernelinvoke', 'kgen_resetinvoke') ) ]}
        part_append_gensnode(pnode, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(pnode, DECL_PART, '')

        idx = index + 1

        attrs = {'expr': 'kgen_resetinvoke'}
        ifreset = part_insert_gensnode(node.kgen_parent, EXEC_PART, block_statements.IfThen, attrs=attrs, index=idx)
        idx += 1

        attrs = {'variable': 'kgen_readsubp_invoke', 'sign': '=', 'expr': '0'}
        part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'variable': 'kgen_resetinvoke', 'sign': '=', 'expr': '.FALSE.'}
        part_append_genknode(ifreset, EXEC_PART, statements.Assignment, attrs=attrs)

        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        l = [ 'kgen_mpirank', '"."', 'kgen_openmptid', '"."', 'kgen_kernelinvoke', '"."', 'kgen_readsubp_invoke']
        attrs = {'specs': ['kgen_read_filepath', 'FMT="(A,I0,A,I0,A,I0,A,I0)"' ], 'items': [ '"%s.L%d."'%(filename, lineno) ] + l}
        part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Write, attrs=attrs, index=idx)
        idx += 1

        #attrs = {'specs': ['kgen_read_filepath', '"(I16)"'], 'items': [ 'kgen_subp_invoke' ]}
        #part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Write, attrs=attrs, index=idx)

        #TODO: might have a bug in kgen_rankthread

        # file open
        attrs = {'specs': ['NEWUNIT=kgen_read_unit', 'FILE=kgen_read_filepath', 'STATUS="OLD"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="READ"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Open, attrs=attrs, index=idx)
        idx += 1

        # write var
        for var in node.kgen_stmt.write_state:
            attrs = {'specs': ['UNIT=kgen_read_unit'], 'items': [var]}
            part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Read, attrs=attrs, index=idx)
            idx += 1

        # file close
        attrs = {'specs': ['UNIT=kgen_read_unit']}
        part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Close, attrs=attrs, index=idx)
        idx += 1

        attrs = {'variable': 'kgen_readsubp_invoke', 'sign': '=', 'expr': 'kgen_readsubp_invoke + 1'}
        part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Assignment, attrs=attrs,index=idx)
        idx += 1

        part_insert_comment(node.kgen_parent, EXEC_PART, idx, '')
