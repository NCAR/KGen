### kgen_genfile.py ###
# funcaaa = A.__dict__['aaa']
# b.aaa = types.MethodType( funcaaa, b )

import os 
import re
import sys
import inspect
import base_classes
import statements
import block_statements
import typedecl_statements
from kgen_utils import Config, KGGenType, ProgramException, traverse, match_namepath, pack_innamepath
from kgen_state import State
from collections import OrderedDict

########### Common ############

plugin_modules = OrderedDict()
event_register = OrderedDict()
event_locations = (NODE_CREATED, BEGIN_PROCESS, FINISH_PROCESS, BEGIN_TOSTR, END_TOSTR) = range(5)

TAB = ' '*4
KERNEL_CHAR, STATE_CHAR = ( 'K', 'S' )
DUMMY_KERNEL_ID = 0

UNIT_PART = 'unit'
USE_PART = 'use'
IMPORT_PART = 'import'
IMPLICIT_PART = 'implicit'
DECL_PART = 'decl'
CONTAINS_PART = 'contains'
SUBP_PART = 'subp'
EXEC_PART = 'exec'
TYPE_PD_PART = 'type_pd'
TYPE_POS_PART = 'type_pos'
TYPE_COMP_PART = 'type_comp'
TYPE_BP_PART = 'type_bp'
INTF_SPEC_PART = 'intf_spec'

match_classes = {
    block_statements.BeginSource: [ UNIT_PART ],
    block_statements.Program: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, EXEC_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Module: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Subroutine: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, EXEC_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Type: [ TYPE_PD_PART, TYPE_POS_PART, TYPE_COMP_PART, TYPE_BP_PART ],
    block_statements.BlockData: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART ],
    block_statements.Interface: [ INTF_SPEC_PART ]
}

default_part_names = [ EXEC_PART ]

part_classes = {
    UNIT_PART: [ block_statements.Program, block_statements.Module ] + block_statements.internal_subprogram,
    USE_PART: [ statements.Use ],
    IMPORT_PART: [ statements.Import ],
    IMPLICIT_PART: block_statements.implicit_part,
    DECL_PART: block_statements.declaration_construct,
    CONTAINS_PART: [ statements.Contains ],
    SUBP_PART: block_statements.internal_subprogram,
    EXEC_PART: block_statements.execution_part,
    TYPE_PD_PART: [ typedecl_statements.Integer ],
    TYPE_POS_PART: block_statements.private_or_sequence,
    TYPE_COMP_PART: block_statements.component_part,
    TYPE_BP_PART: block_statements.type_bound_procedure_part,
    INTF_SPEC_PART: block_statements.internal_subprogram + [ statements.ModuleProcedure ]
}

def get_indent(line):
    return re.match(r"\s*", line).group()

def _genobj_from_obj(gentype, parent, node, kernel_id, tokgen_attr=None):

    obj = None
    try:
        if isinstance(node, base_classes.BeginStatement):
            if gentype==KERNEL_CHAR:
                obj = GenK_BeginStatement(parent, node, kernel_id, tokgen_attr=tokgen_attr)
            elif gentype==STATE_CHAR:
                obj = GenS_BeginStatement(parent, node, kernel_id, tokgen_attr=tokgen_attr)
            else: raise ProgramException('Unknown gentype: %s'%gentype)

            obj.construct_name = node.construct_name
            obj.blocktype = node.__class__.__name__.lower()
        elif isinstance(node, base_classes.Statement):
            if gentype==KERNEL_CHAR:
                obj = GenK_Statement(parent, node, kernel_id, tokgen_attr=tokgen_attr)
            elif gentype==STATE_CHAR:
                obj = GenS_Statement(parent, node, kernel_id, tokgen_attr=tokgen_attr)
            else: raise ProgramException('Unknown gentype: %s'%gentype)
        else: raise ProgramException('Unknown class: %s'%node.__class__)
    except:
        raise

    if obj:
        event_point(NODE_CREATED, obj)

    return obj

def _genobj_from_cls(gentype, parent, node, kernel_id, tokgen_attr=None):
    import types

    obj = None
    try:
        if issubclass(node, base_classes.BeginStatement):
            if gentype==KERNEL_CHAR:
                obj = GenK_BeginStatement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
                end_obj = GenK_Statement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
            elif gentype==STATE_CHAR:
                obj = GenS_BeginStatement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
                end_obj = GenS_Statement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
            else: raise ProgramException('Unknown gentype: %s'%gentype)
            obj.blocktype = node.__name__.lower()
            obj.end_obj = end_obj
            end_obj.blocktype = node.__name__.lower()
            end_obj.match_class = base_classes.EndStatement
        elif issubclass(node, base_classes.Statement):
            if gentype==KERNEL_CHAR:
                obj = GenK_Statement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
            elif gentype==STATE_CHAR:
                obj = GenS_Statement(parent, None, kernel_id, tokgen_attr=tokgen_attr)
            else: raise ProgramException('Unknown gentype: %s'%gentype)
        else: raise ProgramException('Unknown class: %s'%node.__class__)
        obj.match_class = node
    except:
        raise

    mro_classes = inspect.getmro(node)
    for cls in mro_classes:
        if cls.__dict__.has_key('tokgen'):
            tokgen = cls.__dict__['tokgen']
            obj.tokgen = types.MethodType( tokgen, obj )
            break
    if not hasattr(obj, 'tokgen') or obj.tokgen is None:
        raise ProgramException('%s does not have tokgen function'%node)

    if obj:
        event_point(NODE_CREATED, obj)

    return obj

def genkobj(parent, node, kernel_id, tokgen_attr=None):
    if node is None: return

    if inspect.isclass(node):
        obj = _genobj_from_cls(KERNEL_CHAR, parent, node, kernel_id, tokgen_attr=tokgen_attr)
        if obj: obj.isvalid = True
        return obj
    else:
        return _genobj_from_obj(KERNEL_CHAR, parent, node, kernel_id, tokgen_attr=tokgen_attr)

def gensobj(parent, node, kernel_id, tokgen_attr=None):
    if node is None: return

    if inspect.isclass(node):
        obj = _genobj_from_cls(STATE_CHAR, parent, node, kernel_id, tokgen_attr=tokgen_attr)
        if obj: obj.isvalid = True
        return obj
    else:
        return _genobj_from_obj(STATE_CHAR, parent, node, kernel_id, tokgen_attr=tokgen_attr)


########### Plugin ############
class PluginMsg(object):
    (NODE_CREATED, BEGIN_PROCESS, FINISH_PROCESS, BEGIN_TOSTR, END_TOSTR) = event_locations

    def __init__(self, event):
        self.event = event

    def add_event(self, event_loc, cls, matchfunc, callbackfunc):
        if not self.event.has_key(event_loc):
            self.event[event_loc] = OrderedDict()
        if not self.event[event_loc].has_key(cls):
            self.event[event_loc][cls] = []
        self.event[event_loc][cls].append((matchfunc, callbackfunc))

def event_point(event_loc, node):
    for plugin, loc_events in event_register.iteritems():
        for loc, cls_events in loc_events.iteritems():
            if loc==event_loc:
                for cls, funclist in cls_events.iteritems():
                    for matchfunc, cbfunc in funclist:
                        if node.stmt:
                            if node.stmt.__class__==cls and matchfunc(node):
                                cbfunc(node)
                        elif node.match_class:
                            #if node.match_class==cls and matchfunc(pobj, node):
                            if node.match_class==cls and matchfunc(node):
                                #cbfunc(pobj, node)
                                cbfunc(node)
                        else: raise ProgramException('Not correct gen object')

def init_plugins():
    global plugin_register

    plugin_home = os.path.dirname(os.path.realpath(__file__))+'/plugins'
    sys.path.insert(0, plugin_home)
    kgen_plugin = __import__('kgen_plugin')
    plugin_dirs = next(os.walk(plugin_home))[1]
    for plugin_dir in plugin_dirs:
        plugin_path = '%s/%s'%(plugin_home, plugin_dir)
        sys.path.insert(0, plugin_path)
        plugin_files = [x[:-3] for x in os.listdir(plugin_path) if x.endswith(".py")]
        for plugin in plugin_files:
            mod = __import__(plugin)
            for name, cls in inspect.getmembers(mod): 
                if inspect.isclass(cls) and cls is not kgen_plugin.Kgen_Plugin and \
                    issubclass(cls, kgen_plugin.Kgen_Plugin):
                    obj = cls()
                    if not event_register.has_key(cls.__module__):
                        event_register[cls.__module__] = OrderedDict() 
                    obj.register(PluginMsg(event_register[cls.__module__]))

########### Statement ############
class Gen_Statement(object):
    gen_attrs = {'indent': ''}

    def __init__(self, parent, stmt, kernel_id, tokgen_attr=None):
        self.isvalid = True
        self.parent = parent
        self.stmt = stmt
        self.match_class = stmt.__class__
        self.kernel_id = kernel_id
        self.tokgen_attrs = {}
        if tokgen_attr: self.tokgen_attrs.update(tokgen_attr)

        self.name = getattr(stmt, 'name', None)
        if self.name is None and self.tokgen_attrs.has_key('name'):
            self.name = self.tokgen_attrs['name']
        if self.name is None: self.name = ''

    def set_tokgen_attr(self, tokgen_attr):
        self.tokgen_attrs.update(tokgen_attr)

    def append_tokgen_attr(self, tokgen_attr):
        for k, v in tokgen_attr:
            self.tokgen_attrs[k].append(v)

    def extend_tokgen_attr(self, tokgen_attr):
        for k, v in tokgen_attr:
            self.tokgen_attrs[k].extend(v)

    def process(self):
        pass

    def tostr(self):

        if isinstance(self.stmt, statements.Comment):
            if not self.stmt.item.comment.startswith('!KGEN#'):
                start = self.stmt.item.span[0]-1
                end = self.stmt.item.span[1]
                lines = self.stmt.top.prep[start:end]
                lines_str = '\n'.join(lines)
                if lines_str.strip().startswith(self.stmt.item.comment.strip()):
                    return lines_str
        elif self.isvalid:
            cur_indent = self.gen_attrs['indent']
            if self.stmt:
                lines_str = None
                if hasattr(self, 'forced_line') and self.forced_line:
                    lines_str = self.forced_line
                elif hasattr(self.stmt.item, 'span'):
                    start = self.stmt.item.span[0]-1
                    end = self.stmt.item.span[1]
                    lines = self.stmt.top.prep[start:end]
                    lines_str = '\n'.join(lines)

                if len(self.tokgen_attrs)>0:
                    if isinstance(self.stmt, block_statements.BeginStatement):
                        self.indent = cur_indent
                        self.gen_attrs['indent'] = cur_indent + TAB
                    elif isinstance(self.stmt, base_classes.EndStatement):
                        self.gen_attrs['indent'] = self.parent.indent
                    return cur_indent + self.stmt.tokgen(**self.tokgen_attrs)
                else:
                    if lines_str:
                        cur_indent = get_indent(lines_str)
                        if isinstance(self.stmt, base_classes.BeginStatement):
                            self.indent = cur_indent
                            self.gen_attrs['indent'] = cur_indent + TAB
                        else:
                            self.gen_attrs['indent'] = cur_indent
                        return lines_str
                    elif isinstance(self.stmt, block_statements.BeginSource):
                        pass
                    else:
                        import pdb; pdb.set_trace()
                        raise ProgramException('Wrong path for tostr')
            elif self.match_class:
                if issubclass(self.match_class, base_classes.BeginStatement):
                    self.indent = cur_indent
                    self.gen_attrs['indent'] = cur_indent + TAB
                elif issubclass(self.match_class, base_classes.EndStatement):
                    self.gen_attrs['indent'] = self.parent.indent
                    cur_indent = self.parent.indent

                if self.match_class in [statements.ElseIf, statements.Else, statements.ElseWhere]:
                    kgenstr = self.tokgen(**self.tokgen_attrs)
                    if not kgenstr is None: return self.parent.indent + kgenstr
                else:
                    kgenstr = self.tokgen(**self.tokgen_attrs)
                    if not kgenstr is None: return cur_indent + kgenstr
            else: raise ProgramException('Class does not have stmt nor match_class')

    def tokgen(self, **kwargs):
        #import pdb; pdb.set_trace()
        raise ProgramException('Inherited class should implement tokgen().')

class GenK_Statement(Gen_Statement):
    gentype = KERNEL_CHAR
    def __init__(self, parent, stmt, kernel_id, tokgen_attr=None):

        def process_exclude(node, bag, depth):
            from Fortran2003 import Name
            if isinstance(node, Name):
                for namepath, actions in bag['excludes'].iteritems():
                    if match_namepath(namepath, pack_innamepath(bag['stmt'], node.string)):
                        bag['matched'] = True
                        return True

        super(GenK_Statement, self).__init__(parent, stmt, kernel_id, tokgen_attr=tokgen_attr)

        if stmt: stmt.genkpair = self

        if not hasattr(stmt, 'geninfo') and not hasattr(stmt, 'unknowns'):
            self.isvalid = False
        elif stmt and hasattr(stmt, 'f2003') and Config.exclude.has_key('namepath'):
            bag = {'excludes': Config.exclude['namepath'], 'matched': False, 'stmt':stmt}
            traverse(stmt.f2003, process_exclude, bag)
            if bag['matched']:
                self.isvalid = False

class GenS_Statement(Gen_Statement):
    gentype = STATE_CHAR
    def __init__(self, parent, stmt, kernel_id, tokgen_attr=None):
        super(GenS_Statement, self).__init__(parent, stmt, kernel_id, tokgen_attr=tokgen_attr)

        if stmt: stmt.genspair = self

########### BeginStatement ############
class Gen_BeginStatement(object):
    def append_item(self, item):
        self.items.append(item)

    def process(self):
        # process head

        # process children
        for item in self.items:
            item.process()

    def tostr_items(self):
        lines = []
        # tostr children
        for item in self.items:
            l = item.tostr()
            if l is not None: lines.append(l)
        return lines

    def append_in_part(self, part_name, item):
        part = getattr(self, 'part_%s'%part_name)
        part.append(item)

    def insert_in_part(self, part_name, idx, item):
        part = getattr(self, 'part_%s'%part_name)
        part.insert(idx, item)

    def insert_in_order(self, item, insert_order):

        if item.stmt.__class__ is statements.Comment:
            self.append_in_part(insert_order[0], item)
            return insert_order

        new_order = []
        matched = False
        for part_name in insert_order:
            if not matched and item.stmt.__class__ in part_classes[part_name]:
                matched = True
                self.append_in_part(part_name, item)
            if matched:
                new_order.append(part_name)

        if not matched:
            import pdb; pdb.set_trace()
            raise ProgramException('Wrong sequence of stmt type: %s'%item.stmt.__class__)

        return new_order

class GenK_BeginStatement(GenK_Statement, Gen_BeginStatement):
    def __init__(self, parent, stmt, kernel_id, tokgen_attr=None):

        super(GenK_BeginStatement, self).__init__(parent, stmt, kernel_id, tokgen_attr=tokgen_attr)

        self.items = []
        self.end_obj = None

        if self.isvalid:
            self.part_order = []
            if match_classes.has_key(stmt.__class__):
                # add partition
                for part_name in match_classes[stmt.__class__]:
                    part = 'part_%s'%part_name
                    self.part_order.append(part_name)
                    setattr(self, part, [])
            else:
                # add default partition
                for part_name in default_part_names:
                    part = 'part_%s'%part_name
                    self.part_order.append(part_name)
                    setattr(self, part, [])

            if stmt:
                insert_order = self.part_order[:]
                for node in stmt.content[:-1]:
                    item = genkobj(self, node, kernel_id, tokgen_attr=tokgen_attr)
                    self.items.append(item)

                    if node.__class__ is typedecl_statements.Integer and insert_order[0]==TYPE_PD_PART:
                        if any(attr in ['kind', 'len'] for attr in node.attrspec) and all(decl.find('=')>0 for decl in node.entity_decls):
                            insert_order = self.insert_in_order(item, insert_order)
                        else:
                            insert_order = self.insert_in_order(item, insert_order[1:])
                    else:
                        insert_order = self.insert_in_order(item, insert_order)

                if isinstance(stmt.content[-1], base_classes.EndStatement):
                    self.end_obj = genkobj(self, stmt.content[-1], kernel_id, tokgen_attr=tokgen_attr)
                else:
                    item = genkobj(self, stmt.content[-1], kernel_id, tokgen_attr=tokgen_attr)
                    self.items.append(item)
                    self.insert_in_order(item, insert_order)

    def tostr(self):
        lines = []

        # tostr head
        l = super(GenK_BeginStatement, self).tostr()
        if l is not None: lines.append(l)

        lines.extend(self.tostr_items())

        if self.end_obj:
            l = self.end_obj.tostr()
            if l is not None: lines.append(l)

        return '\n'.join(lines)

class GenS_BeginStatement(GenS_Statement, Gen_BeginStatement):
    def __init__(self, parent, stmt, kernel_id, tokgen_attr=None):

        super(GenS_BeginStatement, self).__init__(parent, stmt, kernel_id, tokgen_attr=tokgen_attr)

        self.items = []
        self.end_obj = None
        if stmt:
            if isinstance(stmt.content[-1], base_classes.EndStatement):
                self.end_obj = gensobj(self, stmt.content[-1], kernel_id, tokgen_attr=tokgen_attr)
                
                for node in stmt.content[:-1]:
                    self.items.append(gensobj(self, node, kernel_id, tokgen_attr=tokgen_attr))
            else:
                for node in stmt.content:
                    self.items.append(gensobj(self, node, kernel_id, tokgen_attr=tokgen_attr))

    def tostr(self):
        lines = []

        # tostr head
        l = super(GenS_BeginStatement, self).tostr()
        if l is not None: lines.append(l)

        lines.extend(self.tostr_items())

        if self.end_obj:
            l = self.end_obj.tostr()
            if l is not None: lines.append(l)

        return '\n'.join(lines)


########### functions ############

def generate_kgen_utils(k_id):
    from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr, \
        kgen_get_newunit, kgen_error_stop

    with open('%s/kgen_utils.f90'%Config.path['kernel'], 'wb') as f:
        f.write('MODULE kgen_utils_mod')
        f.write(kgen_utils_file_head)
        f.write('CONTAINS\n')
        f.write(kgen_utils_file_checksubr)
        f.write(kgen_get_newunit)
        f.write(kgen_error_stop)
        f.write('END MODULE kgen_utils_mod\n')

def generate_srcfiles():
    """Generate files."""
    import os

    # create state directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

    # setup plugin framework
    init_plugins()

    # generate kgen_driver.f90 in kernel directory
    driver = genkobj(None, block_statements.BeginSource, DUMMY_KERNEL_ID)
    program = genkobj(driver, block_statements.Program, DUMMY_KERNEL_ID, tokgen_attr={'name':'kernel_driver'})
    driver.append_item(program)

    # construct a generation tree
    genfiles = []
    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
            kfile = genkobj(None, srcobj.tree, DUMMY_KERNEL_ID)
            sfile = gensobj(None, srcobj.tree, DUMMY_KERNEL_ID)
            if kfile is None or sfile is None:
                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
            genfiles.append((kfile, sfile, filepath))

    # process each nodes in the tree
    for kfile, sfile, filepath in genfiles:
        kfile.process()
        sfile.process()
    driver.process()

    # generate source files from each node of the tree
    for kfile, sfile, filepath in genfiles:
        filename = os.path.basename(filepath)
        klines = kfile.tostr()
        if klines is not None:
            with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                fd.write(klines)

        slines = sfile.tostr()
        if slines is not None:
            with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                fd.write(slines)

    with open('%s/kernel_driver.f90'%Config.path['kernel'], 'wb') as fd:
        #import pdb; pdb.set_trace()
        lines = driver.tostr()
        if lines is not None: fd.write(lines)

    # generate kgen_utils.f90 in kernel directory
    generate_kgen_utils(DUMMY_KERNEL_ID)

    State.state = State.STATE_GENERATED

