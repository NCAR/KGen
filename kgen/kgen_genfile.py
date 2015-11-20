### kgen_genfile.py ###
# dupulicate parts before invoke callback function

import os 
import re
import sys
import inspect
import base_classes
import statements
import block_statements
import typedecl_statements
from kgen_utils import Config, KGGenType, ProgramException, traverse, match_namepath, pack_innamepath, pack_exnamepath
from kgen_state import State
from ordereddict import OrderedDict

########### Common ############

class GENERATION_STAGE(object):
    NODE_CREATED, BEGIN_PROCESS, FINISH_PROCESS, ALL_STAGES = range(4)

class FILE_TYPE(object):
    KERNEL, STATE, BOTH = ('K', 'S', 'B')

class KERNEL_SELECTION(object):
    ALL, FIRST, LAST = ('all', 'first', 'last')

event_register = OrderedDict()

TAB = ' '*4
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

def get_entity_name(entity):
    from Fortran2003 import Entity_Decl
 
    edecl = Entity_Decl(entity)
    return edecl.items[0].string

def _take_functions(cls, obj, end_obj):
    import types

    if obj:
        mro_classes = inspect.getmro(cls)
        for cls in mro_classes:
            if cls.__dict__.has_key('tokgen'):
                tokgen = cls.__dict__['tokgen']
                obj.tokgen = types.MethodType( tokgen, obj )
                break
        if not hasattr(obj, 'tokgen') or obj.tokgen is None:
            raise ProgramException('%s does not have tokgen function'%cls)

        for cls in mro_classes:
            if cls.__dict__.has_key('tostr'):
                tostr = cls.__dict__['tostr']
                obj.tostr = types.MethodType( tostr, obj )
                break

        if end_obj:
            mro_classes = inspect.getmro(base_classes.EndStatement)
            for cls in mro_classes:
                if cls.__dict__.has_key('tokgen'):
                    tokgen = cls.__dict__['tokgen']
                    end_obj.tokgen = types.MethodType( tokgen, end_obj )
                    break
            if not hasattr(end_obj, 'tokgen') or end_obj.tokgen is None:
                raise ProgramException('%s does not have tokgen function'%end_obj.__class__)

def _genobj_from_obj(file_type, parent, node, kernel_id, attrs=None):

    obj = None
    try:
        if isinstance(node, base_classes.BeginStatement):
            if file_type==FILE_TYPE.KERNEL:
                obj = GenK_BeginStatement(parent, node, node.__class__, kernel_id, attrs=attrs)
            elif file_type==FILE_TYPE.STATE:
                obj = GenS_BeginStatement(parent, node, node.__class__, kernel_id, attrs=attrs)
            else: raise ProgramException('Unknown file_type: %s'%file_type)

            obj.construct_name = node.construct_name
            obj.blocktype = node.__class__.__name__.lower()
        elif isinstance(node, base_classes.Statement):
            if file_type==FILE_TYPE.KERNEL:
                obj = GenK_Statement(parent, node, node.__class__, kernel_id, attrs=attrs)
            elif file_type==FILE_TYPE.STATE:
                obj = GenS_Statement(parent, node, node.__class__, kernel_id, attrs=attrs)
            else: raise ProgramException('Unknown file_type: %s'%file_type)
        else: raise ProgramException('Unknown class: %s'%node.__class__)
        if hasattr(node, 'name'): obj.name = node.name
    except:
        raise

    _take_functions(node.__class__, obj, None)

    return obj

def _genobj_from_cls(file_type, parent, node, kernel_id, attrs=None):

    obj = None
    end_obj = None
    try:
        if issubclass(node, base_classes.BeginStatement):
            if file_type==FILE_TYPE.KERNEL:
                obj = GenK_BeginStatement(parent, None, node, kernel_id, attrs=attrs)
                end_obj = GenK_Statement(parent, None, base_classes.EndStatement, kernel_id)
            elif file_type==FILE_TYPE.STATE:
                obj = GenS_BeginStatement(parent, None, node, kernel_id, attrs=attrs)
                end_obj = GenS_Statement(parent, None, base_classes.EndStatement, kernel_id)
            else: raise ProgramException('Unknown file_type: %s'%file_type)
            obj.construct_name = None
            obj.blocktype = node.__name__.lower()
            if hasattr(obj, 'name'): end_obj.name = obj.name
            else: end_obj.name = None
            if node!=block_statements.BeginSource:
                obj.kgen_end_obj = end_obj
                end_obj.kgen_parent = obj
                if node.__name__.lower()=='ifthen':
                    end_obj.blocktype = 'if'
                else:
                    end_obj.blocktype = node.__name__.lower()
        elif issubclass(node, base_classes.Statement):
            if file_type==FILE_TYPE.KERNEL:
                obj = GenK_Statement(parent, None, node, kernel_id, attrs=attrs)
            elif file_type==FILE_TYPE.STATE:
                obj = GenS_Statement(parent, None, node, kernel_id, attrs=attrs)
            else: raise ProgramException('Unknown file_type: %s'%file_type)
        else: raise ProgramException('Unknown class: %s'%node.__class__)
    except:
        raise

    _take_functions(node, obj, end_obj)

    return obj

def genkobj(parent, node, kernel_id, attrs=None):
    if node is None: return

    if inspect.isclass(node):
        return _genobj_from_cls(FILE_TYPE.KERNEL, parent, node, kernel_id, attrs=attrs)
    else:
        return _genobj_from_obj(FILE_TYPE.KERNEL, parent, node, kernel_id, attrs=attrs)

def gensobj(parent, node, kernel_id, attrs=None):
    if node is None: return

    if inspect.isclass(node):
        return _genobj_from_cls(FILE_TYPE.STATE, parent, node, kernel_id, attrs=attrs)
    else:
        return _genobj_from_obj(FILE_TYPE.STATE, parent, node, kernel_id, attrs=attrs)


########### Plugin ############

class PluginMsg(object):
    def __init__(self, event):
        self.event = event

    def add_event(self, kernel_id, file_type, gen_stage, target, matchfunc, callbackfunc):
        nextdict = self.event
        if not nextdict.has_key(kernel_id):
            nextdict[kernel_id] = OrderedDict()
            nextdict = nextdict[kernel_id]
        else: nextdict = nextdict[kernel_id]

        if not nextdict.has_key(file_type):
            nextdict[file_type] = OrderedDict()
            nextdict = nextdict[file_type]
        else: nextdict = nextdict[file_type]

        if not nextdict.has_key(gen_stage):
            nextdict[gen_stage] = OrderedDict()
            nextdict = nextdict[gen_stage]
        else: nextdict = nextdict[gen_stage]

        if not nextdict.has_key(target):
            nextdict[target] = []
            nextlist = nextdict[target]
        else: nextlist = nextdict[target]

        nextlist.append((matchfunc, callbackfunc))

def event_point(cur_kernel_id, cur_file_type, cur_gen_stage, node, plugins=None):
    for plugin_dir, plugin_modules in event_register.iteritems():
        if plugins and plugin_dir not in plugins: continue
        for plugin_name, plugin_objects in plugin_modules.iteritems():
            for plugin_class, kernel_ids in plugin_objects.iteritems():
                for kernel_id, file_types in kernel_ids.iteritems():
                    if  kernel_id==KERNEL_SELECTION.ALL: pass
                    else:
                        if kernel_id==KERNEL_SELECTION.FIRST and kernel_id!=0: continue
                        if kernel_id==KERNEL_SELECTION.LAST and kernel_id!=len(State.kernels)-1: continue
                        if isinstance(kernel_id, int) and kernel_id!=cur_kernel_id: continue
                
                    for file_type, gen_stages in file_types.iteritems():
                        if file_type!=cur_file_type: continue

                        for gen_stage, targets in gen_stages.iteritems():
                            if gen_stage==GENERATION_STAGE.ALL_STAGES: pass
                            elif gen_stage!=cur_gen_stage: continue

                            for target, funclist in targets.iteritems():
                                if inspect.isclass(target):
                                    if node.kgen_stmt:
                                        if isinstance(node.kgen_stmt, target):
                                            for matchfunc, cbfunc in funclist:
                                                if matchfunc is None or matchfunc(node): cbfunc(node)
                                    elif node.kgen_match_class:
                                        if node.kgen_match_class is target:
                                            for matchfunc, cbfunc in funclist:
                                                if matchfunc is None or matchfunc(node): cbfunc(node)
                                    else: raise ProgramException('Incorrect node type: %s'%node)
                                else:
                                    if node is target:
                                        for matchfunc, cbfunc in funclist:
                                            if matchfunc is None or matchfunc(node): cbfunc(node)

def getinfo(name):
    if name=='kernel_name': return Config.callsite['subpname'].firstpartname()
    elif name=='kernel_driver_name': return State.kernel_driver['name']
    elif name=='kernel_driver_args': return State.kernel_driver['args']
    elif name=='parentblock_subp_name': return State.parentblock['stmt'].name
    elif name=='is_mpi_app': return Config.mpi['enabled']
    elif name=='mpi_rank_size': return Config.mpi['size']
    elif name=='mpi_ranks': return Config.mpi['ranks']
    elif name=='invocation_size': return Config.invocation['size']
    elif name=='invocation_numbers': return Config.invocation['numbers']
    elif name=='print_var_names': return Config.debug['printvar']
    else: raise ProgramException('No information for %s'%name)

def set_plugin_env(mod):

    mod.genkobj = genkobj
    mod.gensobj = genkobj

    mod.getinfo =  getinfo
    mod.get_entity_name = get_entity_name
    mod.pack_exnamepath = pack_exnamepath
    mod.match_namepath = match_namepath

    mod.GENERATION_STAGE = GENERATION_STAGE
    mod.FILE_TYPE = FILE_TYPE
    mod.KERNEL_SELECTION = KERNEL_SELECTION

    mod.create_part = create_part
    mod.remove_part = remove_part
    mod.get_part = get_part
    mod.has_object_in_part = has_object_in_part
    mod.get_index_part_order = get_index_part_order
    mod.register_part_to_part_order = register_part_to_part_order
    mod.unregister_part_from_part_order = unregister_part_from_part_order
    mod.insert_item_in_part = insert_item_in_part
    mod.append_item_in_part = append_item_in_part
    mod.append_comment_in_part = append_comment_in_part
    mod.extend_part = extend_part
    mod.discard_items_in_part = discard_items_in_part

    mod.TAB = TAB
    mod.DUMMY_KERNEL_ID = DUMMY_KERNEL_ID

    mod.UNIT_PART = UNIT_PART
    mod.USE_PART = USE_PART
    mod.IMPORT_PART = IMPORT_PART
    mod.IMPLICIT_PART = IMPLICIT_PART
    mod.DECL_PART = DECL_PART
    mod.CONTAINS_PART = CONTAINS_PART
    mod.SUBP_PART = SUBP_PART
    mod.EXEC_PART = EXEC_PART
    mod.TYPE_PD_PART = TYPE_PD_PART
    mod.TYPE_POS_PART = TYPE_POS_PART
    mod.TYPE_COMP_PART = TYPE_COMP_PART
    mod.TYPE_BP_PART = TYPE_BP_PART
    mod.INTF_SPEC_PART = INTF_SPEC_PART

def init_plugins():
    global plugin_register
    global plugin_module_priority

    plugin_home = os.path.dirname(os.path.realpath(__file__))+'/plugins'
    sys.path.insert(0, plugin_home)
    kgen_plugin = __import__('kgen_plugin')
    plugin_dirs = next(os.walk(plugin_home))[1]
    sorted_plugin_dirs = []
    for plugin_dir in kgen_plugin.Kgen_Plugin.priority:
        if plugin_dir in plugin_dirs:
            sorted_plugin_dirs.append(plugin_dir)
    for plugin_dir in sorted_plugin_dirs:
        plugin_path = '%s/%s'%(plugin_home, plugin_dir)
        sys.path.insert(0, plugin_path)
        plugin_files = [x[:-3] for x in os.listdir(plugin_path) if x.endswith(".py")]
        for plugin in plugin_files:
            mod = __import__(plugin)
            set_plugin_env(mod)
            match = lambda x: inspect.isclass(x) and x is not kgen_plugin.Kgen_Plugin and issubclass(x, kgen_plugin.Kgen_Plugin)
            for name, cls in inspect.getmembers(mod, match): 
                obj = cls()
                if not event_register.has_key(plugin_dir):
                    event_register[plugin_dir] = OrderedDict() 
                if not event_register[plugin_dir].has_key(cls.__module__):
                    event_register[plugin_dir][cls.__module__] = OrderedDict() 
                if not event_register[plugin_dir][cls.__module__].has_key(obj):
                    event_register[plugin_dir][cls.__module__][obj] = OrderedDict() 
                obj.register(PluginMsg(event_register[plugin_dir][cls.__module__][obj]))

def create_part(node, part_name):
    part = 'part_%s'%part_name
    setattr(node, part, [])
    return getattr(node, part)

def remove_part(node, part_name):
    part = 'part_%s'%part_name
    delattr(node, part)

def get_index_part_order(node, part_name):
    return node.kgen_part_order.index(part_name)

def unregister_part_from_part_order(node, part_name):
    node.kgen_part_order.remove(part_name)

def register_part_to_part_order(node, index, part_name):
    node.kgen_part_order.insert(index, part_name)

def get_part(node, part_name):
    part = 'part_%s'%part_name
    return getattr(node, part)

def insert_item_in_part(node, part_name, index, item):
    part = get_part(node, part_name)
    part.insert(index, item)

def append_item_in_part(node, part_name, item):
    part = get_part(node, part_name)
    part.append(item)

def append_comment_in_part(node, part_name, comment_string):
    part = get_part(node, part_name)
    comment = gensobj(node, statements.Comment, DUMMY_KERNEL_ID, attrs={'comment': comment_string})
    part.append(comment)

def extend_part(node, part_name, items):
    part = get_part(node, part_name)
    part.extend(items)

def discard_items_in_part(node, part_name):
    part = get_part(node, part_name)
    part = []


def has_object_in_part(node, part_name, cls, attrs=None ):
    part = get_part(node, part_name)
    for item in part:
        if hasattr(item, 'kgen_stmt') and isinstance(item.kgen_stmt, cls):
            if attrs is None:
                return True
            elif all(hasattr(item, k)  and getattr(item, k)==v for k, v in attrs.items()):
                return True
    return False

########### Statement ############
class Gen_Statement(object):
    kgen_gen_attrs = {'indent': ''}

    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):
        self.kgen_isvalid = True
        self.kgen_parent = parent
        self.kgen_stmt = stmt
        self.kgen_match_class = match_class
        self.kgen_kernel_id = kernel_id
        self.kgen_stmt_tokgen = False

        if attrs:
            for key, value in attrs.iteritems():
                setattr(self, key, value)

    def statement_created(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.NODE_CREATED, self, plugins=plugins)

    def statement_process(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.BEGIN_PROCESS, self, plugins=plugins)

    def statement_finalize(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.FINISH_PROCESS, self, plugins=plugins)

    def tostring(self):

        if isinstance(self.kgen_stmt, statements.Comment):
            if self.kgen_stmt:
                if not self.kgen_stmt.item.comment.startswith('!KGEN#'):
                    start = self.kgen_stmt.item.span[0]-1
                    end = self.kgen_stmt.item.span[1]
                    lines = self.kgen_stmt.top.prep[start:end]
                    lines_str = '\n'.join(lines)
                    if lines_str.strip().startswith(self.kgen_stmt.item.comment.strip()):
                        return lines_str
            else:
                return self.comment
        elif self.kgen_isvalid:
            cur_indent = self.kgen_gen_attrs['indent']
            if self.kgen_stmt:
                lines_str = None
                if hasattr(self, 'kgen_forced_line') and self.kgen_forced_line:
                    lines_str = self.kgen_forced_line
                elif hasattr(self.kgen_stmt.item, 'span'):
                    start = self.kgen_stmt.item.span[0]-1
                    end = self.kgen_stmt.item.span[1]
                    lines = self.kgen_stmt.top.prep[start:end]
                    lines_str = '\n'.join(lines)

                if self.kgen_stmt_tokgen:
                    if isinstance(self.kgen_stmt, block_statements.BeginStatement):
                        self.kgen_indent = cur_indent
                        self.kgen_gen_attrs['indent'] = cur_indent + TAB
                    elif isinstance(self.kgen_stmt, base_classes.EndStatement):
                        self.kgen_gen_attrs['indent'] = self.kgen_parent.kgen_indent
                    return cur_indent + self.stmt.tokgen()
                else:
                    if lines_str:
                        cur_indent = get_indent(lines_str)
                        if isinstance(self.kgen_stmt, base_classes.BeginStatement):
                            self.kgen_indent = cur_indent
                            self.kgen_gen_attrs['indent'] = cur_indent + TAB
                        else:
                            self.kgen_gen_attrs['indent'] = cur_indent
                        return lines_str
                    elif isinstance(self.kgen_stmt, block_statements.BeginSource):
                        pass
                    else:
                        import pdb; pdb.set_trace()
                        raise ProgramException('Wrong path for tostring')
            elif self.kgen_match_class:
                if issubclass(self.kgen_match_class, base_classes.BeginStatement):
                    self.kgen_indent = cur_indent
                    self.kgen_gen_attrs['indent'] = cur_indent + TAB
                elif issubclass(self.kgen_match_class, base_classes.EndStatement):
                    self.kgen_gen_attrs['indent'] = self.kgen_parent.kgen_indent
                    cur_indent = self.kgen_parent.kgen_indent

                if self.kgen_match_class in [statements.ElseIf, statements.Else, statements.ElseWhere]:
                    kgenstr = self.tokgen()
                    if not kgenstr is None: return self.kgen_parent.kgen_indent + kgenstr
                else:
                    kgenstr = self.tokgen()
                    if not kgenstr is None: return cur_indent + kgenstr
            else: raise ProgramException('Class does not have stmt nor match_class')

    def tokgen(self):
        #import pdb; pdb.set_trace()
        raise ProgramException('Inherited class should implement tokgen().')

class GenK_Statement(Gen_Statement):
    kgen_file_type = FILE_TYPE.KERNEL
    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):

        def process_exclude(node, bag, depth):
            from Fortran2003 import Name
            if isinstance(node, Name):
                for namepath, actions in bag['excludes'].iteritems():
                    if match_namepath(namepath, pack_innamepath(bag['stmt'], node.string)):
                        bag['matched'] = True
                        return True

        super(GenK_Statement, self).__init__(parent, stmt, match_class, kernel_id, attrs=attrs)

        if stmt:
            stmt.genkpair = self

            if not hasattr(stmt, 'geninfo') and not hasattr(stmt, 'unknowns'):
                self.kgen_isvalid = False
            elif stmt and hasattr(stmt, 'f2003') and Config.exclude.has_key('namepath'):
                bag = {'excludes': Config.exclude['namepath'], 'matched': False, 'stmt':stmt}
                traverse(stmt.f2003, process_exclude, bag)
                if bag['matched']:
                    self.kgen_isvalid = False

    def created(self, plugins):
        self.statement_created(plugins)

    def process(self, plugins):
        self.statement_process(plugins)

    def finalize(self, plugins):
        self.statement_finalize(plugins)

class GenS_Statement(Gen_Statement):
    kgen_file_type = FILE_TYPE.STATE
    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):
        super(GenS_Statement, self).__init__(parent, stmt, match_class, kernel_id, attrs=attrs)

        if stmt: stmt.genspair = self

    def created(self, plugins):
        self.statement_created(plugins)

    def process(self, plugins):
        self.statement_process(plugins)

    def finalize(self, plugins):
        self.statement_finalize(plugins)

########### BeginStatement ############
class Gen_BeginStatement(object):
    def beginstatement_init(self, stmt, match_class):

        self.kgen_end_obj = None

        self.kgen_part_order = []
        if match_classes.has_key(match_class):
            # add partition
            for part_name in match_classes[match_class]:
                part = 'part_%s'%part_name
                self.kgen_part_order.append(part_name)
                setattr(self, part, [])
        else:
            # add default partition
            for part_name in default_part_names:
                part = 'part_%s'%part_name
                self.kgen_part_order.append(part_name)
                setattr(self, part, [])

        if self.kgen_file_type==FILE_TYPE.KERNEL:
            if not self.kgen_isvalid: return
            genobj = genkobj
        elif self.kgen_file_type==FILE_TYPE.STATE:
            genobj = gensobj

        if stmt:
            insert_order = self.kgen_part_order[:]
            for node in stmt.content[:-1]:
                item = genobj(self, node, self.kgen_kernel_id)

                if node.__class__ is typedecl_statements.Integer and insert_order[0]==TYPE_PD_PART:
                    if any(attr in ['kind', 'len'] for attr in node.attrspec) and all(decl.find('=')>0 for decl in node.entity_decls):
                        insert_order = self.insert_in_order(item, insert_order)
                    else:
                        insert_order = self.insert_in_order(item, insert_order[1:])
                else:
                    insert_order = self.insert_in_order(item, insert_order)

            if isinstance(stmt.content[-1], base_classes.EndStatement):
                self.kgen_end_obj = genobj(self, stmt.content[-1], self.kgen_kernel_id)
            else:
                item = genobj(self, stmt.content[-1], self.kgen_kernel_id)
                self.insert_in_order(item, insert_order)

    def beginstatement_tostring(self):
        lines = []

        # tostring head
        if self.kgen_file_type==FILE_TYPE.KERNEL:
            l = super(GenK_BeginStatement, self).tostring()
        else:
            l = super(GenS_BeginStatement, self).tostring()
        if l is not None: lines.append(l)

        lines.extend(self.tostring_parts())

        if self.kgen_end_obj:
            l = self.kgen_end_obj.tostring()
            if l is not None: lines.append(l)

        return '\n'.join(lines)

    def tostring_parts(self):
        lines = []
        for part_name in self.kgen_part_order:
            part = getattr(self, 'part_%s'%part_name)
            for item in part:
                l = item.tostring()
                if l is not None: lines.append(l)
        return lines

    def beginstatement_created(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.NODE_CREATED, self, plugins=plugins)

        temp_parts = {}
        for part_name in self.kgen_part_order:
            part = getattr(self, 'part_%s'%part_name)
            temp_parts[part_name] = part[:]

        for part_name in self.kgen_part_order:
            for item in temp_parts[part_name]:
                item.created(plugins)

    def beginstatement_process(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.BEGIN_PROCESS, self, plugins=plugins)

        temp_parts = {}
        for part_name in self.kgen_part_order:
            part = getattr(self, 'part_%s'%part_name)
            temp_parts[part_name] = part[:]

        for part_name in self.kgen_part_order:
            for item in temp_parts[part_name]:
                item.process(plugins)

    def beginstatement_finalize(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.FINISH_PROCESS, self, plugins=plugins)

        temp_parts = {}
        for part_name in self.kgen_part_order:
            part = getattr(self, 'part_%s'%part_name)
            temp_parts[part_name] = part[:]

        for part_name in self.kgen_part_order:
            for item in temp_parts[part_name]:
                item.finalize(plugins)

    def insert_in_order(self, item, insert_order):

        if item.kgen_stmt.__class__ is statements.Comment:
            append_item_in_part(self, insert_order[0], item)
            return insert_order

        new_order = []
        matched = False
        for part_name in insert_order:
            if not matched and item.kgen_stmt.__class__ in part_classes[part_name]:
                matched = True
                append_item_in_part(self, part_name, item)
            if matched:
                new_order.append(part_name)

        if not matched:
            import pdb; pdb.set_trace()
            raise ProgramException('Wrong sequence of stmt type: %s'%item.kgen_stmt.__class__)

        return new_order

class GenK_BeginStatement(GenK_Statement, Gen_BeginStatement):
    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):

        super(GenK_BeginStatement, self).__init__(parent, stmt, match_class, kernel_id, attrs=attrs)

        self.kgen_end_obj = None

        self.beginstatement_init(stmt, match_class)

    def created(self, plugins):
        self.beginstatement_created(plugins)

    def process(self, plugins):
        self.beginstatement_process(plugins)

    def finalize(self, plugins):
        self.beginstatement_finalize(plugins)

    def tostring(self):
        return self.beginstatement_tostring()

class GenS_BeginStatement(GenS_Statement, Gen_BeginStatement):
    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):

        super(GenS_BeginStatement, self).__init__(parent, stmt, match_class, kernel_id, attrs=attrs)

        self.beginstatement_init(stmt, match_class)

    def created(self, plugins):
        self.beginstatement_created(plugins)

    def process(self, plugins):
        self.beginstatement_process(plugins)

    def finalize(self, plugins):
        self.beginstatement_finalize(plugins)

    def tostring(self):
        return self.beginstatement_tostring()

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
    program = genkobj(driver, block_statements.Program, DUMMY_KERNEL_ID)
    program.name = getinfo('kernel_driver_name')
    append_item_in_part(driver, UNIT_PART, program)

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
    for plugin_dir in event_register.keys():
        for kfile, sfile, filepath in genfiles:
            kfile.created([plugin_dir])
            sfile.created([plugin_dir])
        driver.created([plugin_dir])

        for kfile, sfile, filepath in genfiles:
            kfile.process([plugin_dir])
            sfile.process([plugin_dir])
        driver.process([plugin_dir])

        for kfile, sfile, filepath in genfiles:
            kfile.finalize([plugin_dir])
            sfile.finalize([plugin_dir])
        driver.finalize([plugin_dir])

    # generate source files from each node of the tree
    for kfile, sfile, filepath in genfiles:
        filename = os.path.basename(filepath)
        klines = kfile.tostring()
        if klines is not None:
            with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                fd.write(klines)

        slines = sfile.tostring()
        if slines is not None:
            with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                fd.write(slines)

    with open('%s/%s.f90'%(Config.path['kernel'], getinfo('kernel_driver_name')), 'wb') as fd:
        lines = driver.tostring()
        if lines is not None: fd.write(lines)

    # generate kgen_utils.f90 in kernel directory
    generate_kgen_utils(DUMMY_KERNEL_ID)

    State.state = State.STATE_GENERATED

