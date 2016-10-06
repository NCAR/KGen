# gen_write_typedecl_in_module.py

import os 
import statements
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

        if 'kgen_write_unit' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_write_unit', 'kgen_ierr']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            pstmt.a.variable_names.append('kgen_write_unit')

        if 'kgen_write_invoke' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_write_invoke = 0']}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            pstmt.a.variable_names.append('kgen_write_invoke')

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_write_invoke_str'], 'selector':('16', None)}
            part_append_gensnode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)

        part_append_comment(pnode, DECL_PART, '')

        idx = index + 1

        attrs = {'specs': ['kgen_write_invoke_str', '"(I16)"'], 'items': [ 'kgen_write_invoke' ]}
        part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Write, attrs=attrs, index=idx)
        idx += 1

        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        # file open
        attrs = {'specs': ['UNIT=kgen_write_unit', 'FILE="%s/%s.L%d." // TRIM(ADJUSTL(kgen_write_invoke_str))'%\
            (getinfo('kernel_path'), filename,lineno), 'STATUS="NEW"', 'ACCESS="STREAM"', \
            'FORM="UNFORMATTED"', 'ACTION="WRITE"', 'CONVERT="BIG_ENDIAN"', 'IOSTAT=kgen_ierr']}
        part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Open, attrs=attrs, index=idx)
        idx += 1

        # write var
        for var in node.kgen_stmt.write_state:
            attrs = {'specs': ['UNIT=kgen_write_unit'], 'items': [var]}
            part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Write, attrs=attrs, index=idx)
            idx += 1

        # file close
        attrs = {'specs': ['UNIT=kgen_write_unit']}
        part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Close, attrs=attrs, index=idx)
        idx += 1

        attrs = {'variable': 'kgen_write_invoke', 'sign': '=', 'expr': 'kgen_write_invoke + 1'}
        part_insert_gensnode(node.kgen_parent, EXEC_PART, statements.Assignment, attrs=attrs,index=idx)
        idx += 1

        part_insert_comment(node.kgen_parent, EXEC_PART, idx, '')

    def read_state(self, node):
        index, partname, part = get_part_index(node)
        pstmt = node.kgen_stmt.ancestors()[-1]
        pnode = pstmt.genkpair

        if 'kgen_read_unit' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'entity_decls': ['kgen_read_unit', 'kgen_ierr']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            pstmt.a.variable_names.append('kgen_read_unit')

        if 'kgen_read_invoke' not in pstmt.a.variable_names:
            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_read_invoke = 0']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            pstmt.a.variable_names.append('kgen_read_invoke')

            attrs = {'type_spec': 'CHARACTER', 'entity_decls': ['kgen_read_invoke_str'], 'selector':('16', None)}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Character, attrs=attrs)

        part_append_comment(pnode, DECL_PART, '')

        idx = index + 1

        attrs = {'specs': ['kgen_read_invoke_str', '"(I16)"'], 'items': [ 'kgen_read_invoke' ]}
        part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Write, attrs=attrs, index=idx)
        idx += 1

        filename = os.path.splitext(os.path.basename(node.kgen_stmt.reader.id))[0]
        lineno = node.kgen_stmt.item.span[0]

        # file open
        attrs = {'specs': ['UNIT=kgen_read_unit', 'FILE="%s.L%d." // TRIM(ADJUSTL(kgen_read_invoke_str))'%(filename,lineno), 'STATUS="OLD"', 'ACCESS="STREAM"', \
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

        attrs = {'variable': 'kgen_read_invoke', 'sign': '=', 'expr': 'kgen_read_invoke + 1'}
        part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Assignment, attrs=attrs,index=idx)
        idx += 1

        part_insert_comment(node.kgen_parent, EXEC_PART, idx, '')
