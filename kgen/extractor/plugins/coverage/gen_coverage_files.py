import os
import random
import json
from kgplugin import Kgen_Plugin
from parser import block_statements, statements, typedecl_statements
import collections

RECL = 10
META = 'metadata.json'

# TODO: gen_coverage subroutine should contains MPI communicator if MPI is enabled

class Gen_Coverage_File(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None
        self.logger = getinfo('logger')
        self.files = None
        self.lines = None
        self.inv_files = None
        self.inv_lines = None

    # registration
    def register(self, msg):

        self.frame_msg = msg

        # read map
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            getinfo('topblock_stmt'), None, self.read_maps)

        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            block_statements.IfThen, None, self.addstmt_ifthen)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.ElseIf, None, self.addstmt_elseif)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Else, None, self.addstmt_else)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            statements.Case, None, self.addstmt_case)

        # when finish process
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.FINISH_PROCESS, \
            getinfo('topblock_stmt'), None, self.add_blockdata)
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
            block_statements.Program, self.is_driver_name, self.show_coverage)

    def is_driver_name(self, node):
        if node.name==getinfo('kernel_driver_name'): return True
        else: return False

    def show_coverage(self, node):
        part_append_comment(node, DECL_PART, "#IFDEF KGEN_COVERAGE", style="cpp")

        maxlines = max([len(l) for l in self.lines.values()])
        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d, 0:%d)'%(len(self.files)-1, maxlines-1) ], 'entity_decls': ['kgen_visits']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'coverage', ('kgen_visits', ) ) ]}
        part_append_genknode(node, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(node, DECL_PART, "#ENDIF", style="cpp")

        part_append_comment(node, DECL_PART, '')

        part_append_comment(node, EXEC_PART, "#IFDEF KGEN_COVERAGE", style="cpp")


        attrs = {'items': ['"****************************************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"kernel coverage using input files on ""kgen_statefile.lst"""'], 'specs': [ '*', '"(4X,A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        for fid, fpath in self.inv_files.items():

            attrs = {'items': ['""'], 'specs': [ '*', '"(A)"' ]}
            part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

            if len(fpath) > 50:
                fpath = ".." + fpath[-50:] 
            attrs = {'specs': ['*', '*'], 'items': [ '"In ""%s"" (file id=%s),"'%(fpath,fid) ]}
            part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

            for lid in sorted(self.inv_lines[fid]):
                lnum = self.inv_lines[fid][lid]
                attrs = {'specs': ['*', '"(A,I,A)"'], 'items': ['"  Kernel visited "', \
                    'kgen_visits(%s,%s)'%(fid, lid), '" times at line_id=%s (near original line=%s)"'%(lid, lnum)]}
                    #'kgen_visits(%s,%s)'%(fid, lid), '" times near the line # of %s (line id=%s)"'%(lnum, lid)]}
                part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'items': ['"****************************************************************************"'], 'specs': [ '*', '"(A)"' ]}
        part_append_genknode(node, EXEC_PART, statements.Write, attrs=attrs)

        part_append_comment(node, EXEC_PART, "#ENDIF", style="cpp")

        #import pdb; pdb.set_trace()

    ##################################
    # reading coverage maps
    ##################################

    def read_maps(self, node):

        coverage_files = '%s/__data__/%s/files'%(getinfo('model_path'), getinfo('coverage_typeid'))
        if os.path.exists(coverage_files):
            with open(coverage_files, 'r') as fm:
                self.inv_files = json.load(fm)
                self.files = {}
                for fid, fpath in self.inv_files.items():
                    self.files[fpath] = fid
        else:
            self.logger.warn('Coverage file data does not exist: %s'%coverage_files)

        coverage_lines = '%s/__data__/%s/lines'%(getinfo('model_path'), getinfo('coverage_typeid'))
        if os.path.exists(coverage_lines):
            with open(coverage_lines, 'r') as fm:
                self.inv_lines = json.load(fm)
                self.lines = {}
                for fid, lmap in self.inv_lines.items():
                    self.lines[fid] = linemap = {}
                    for lid, lnum in lmap.items():
                        self.lines[fid][int(lnum)] = int(lid)
        else:
            self.logger.warn('Coverage file data does not exist: %s'%coverage_lines)


    ##################################
    # adding common stmt
    ##################################

    def add_commonstmt(self, node):
        pstmt = node.kgen_stmt.ancestors()[-1]
        pnode = pstmt.genkpair
        if not hasattr(pnode, '__kgen__coverage__common_stmt__'):
            part_append_comment(node, DECL_PART, "#IFDEF KGEN_COVERAGE", style="cpp")
            maxlines = max([len(l) for l in self.lines.values()])
            attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d, 0:%d)'%(len(self.files)-1, maxlines-1) ], 'entity_decls': ['kgen_visits']}
            part_append_genknode(pnode, DECL_PART, typedecl_statements.Integer, attrs=attrs)
            attrs = {'items': [ ( 'coverage', ('kgen_visits', ) ) ]}
            part_append_genknode(pnode, DECL_PART, statements.Common, attrs=attrs)
            part_append_comment(node, DECL_PART, "#ENDIF", style="cpp")
            pnode.__kgen__coverage__common_stmt__ = True

    ##################################
    # adding coverage statements
    ##################################

    def ispure(self, node):
        if hasattr(node, 'kgen_stmt') and node.kgen_stmt and \
            isinstance(node.kgen_stmt, block_statements.SubProgramStatement):
            if node.kgen_stmt.is_pure() or node.kgen_stmt.is_elemental():
                return True
        if hasattr(node, 'kgen_parent'):
            return self.ispure(node.kgen_parent)
        return False

    def lineid(self, node):
        if not hasattr(node, 'kgen_stmt') or not hasattr(node.kgen_stmt, 'reader')  or \
            not hasattr(node.kgen_stmt, 'item'):
            return False, False

        path = node.kgen_stmt.reader.id
        lineno = node.kgen_stmt.item.span[0]
        fid = self.files.get(path, False)
        linemap = self.lines.get(fid, {})
        return fid, linemap.get(lineno, False)

    def add_stmt_block(self, node):
        fid, lid = self.lineid(node)
        if fid is not False and lid is not False and not self.ispure(node) and \
            hasattr(node.kgen_stmt, 'unknowns'):
            self.add_commonstmt(node)
            part_insert_comment(node, EXEC_PART, 0, "#IFDEF KGEN_COVERAGE", style="cpp")
            attrs = {'variable': 'kgen_visits(%s, %d)'%(fid, lid), 'sign': '=', 'expr': 'kgen_visits(%s, %d) + 1'%(fid, lid)}
            part_insert_genknode(node, EXEC_PART, statements.Assignment, index=1, attrs=attrs)
            part_insert_comment(node, EXEC_PART, 2, "#ENDIF", style="cpp")

    def add_stmt(self, node):
        fid, lid = self.lineid(node)
        if fid is not False and lid is not False and not self.ispure(node) and \
            hasattr(node.kgen_stmt, 'unknowns'):
            self.add_commonstmt(node)
            idx, name, part = get_part_index(node)
            part_insert_comment(node.kgen_parent, EXEC_PART, idx+1, "#IFDEF KGEN_COVERAGE", style="cpp")
            attrs = {'variable': 'kgen_visits(%s, %d)'%(fid, lid), 'sign': '=', 'expr': 'kgen_visits(%s, %d) + 1'%(fid, lid)}
            part_insert_genknode(node.kgen_parent, EXEC_PART, statements.Assignment, index=(idx+2), attrs=attrs)
            part_insert_comment(node.kgen_parent, EXEC_PART, idx+3, "#ENDIF", style="cpp")

    def addstmt_ifthen(self, node):
        self.add_stmt_block(node)

    def addstmt_elseif(self, node):
        self.add_stmt(node)

    def addstmt_else(self, node):
        self.add_stmt(node)

    def addstmt_case(self, node):
        self.add_stmt(node)

    ##################################
    # adding common block data
    ##################################

    def add_blockdata(self, node):

        maxlines = max([len(l) for l in self.lines.values()])

        part_append_comment(node.kgen_parent, UNIT_PART, '')
        part_append_comment(node.kgen_parent, UNIT_PART, "#IFDEF KGEN_COVERAGE", style="cpp")

        attrs = {'name': 'kgen_coverage_data'}
        cblock = part_append_genknode(node.kgen_parent, UNIT_PART, block_statements.BlockData, attrs=attrs)

        attrs = {'type_spec': 'INTEGER', 'attrspec': [ 'DIMENSION(0:%d, 0:%d)'%(len(self.files)-1, maxlines-1) ], 'entity_decls': ['kgen_visits = 0']}
        part_append_genknode(cblock, DECL_PART, typedecl_statements.Integer, attrs=attrs)

        attrs = {'items': [ ( 'coverage', ('kgen_visits',) ) ]}
        part_append_genknode(cblock, DECL_PART, statements.Common, attrs=attrs)

        part_append_comment(node.kgen_parent, UNIT_PART, "#ENDIF", style="cpp")

