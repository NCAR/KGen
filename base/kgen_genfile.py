### kgen_genfile.py ###
# dupulicate parts before invoke callback function

import os 
import re
import sys
import inspect
import api
import base_classes
import statements
import block_statements
import typedecl_statements
from kgen_utils import Config, KGGenType, ProgramException, traverse, match_namepath, pack_innamepath, pack_exnamepath, get_exclude_actions, Logger
from kgen_state import State
from kgen_plugin import Kgen_Plugin
from ordereddict import OrderedDict

########### Common ############

TAB = ' '*4

class GENERATION_STAGE(object):
    NODE_CREATED, BEGIN_PROCESS, FINISH_PROCESS, ALL_STAGES = range(4)

class FILE_TYPE(object):
    KERNEL, STATE, BOTH = ('K', 'S', 'B')

class KERNEL_SELECTION(object):
    ALL, FIRST, LAST = ('all', 'first', 'last')

KERNEL_ID_0 = 0

event_register = OrderedDict()
named_parts = OrderedDict()
plugin_common = OrderedDict()


PART_PREFIX = '_kgen_part_'

UNIT_PART = 'unit'
USE_PART = 'use'
IMPORT_PART = 'import'
IMPLICIT_PART = 'implicit'
DECL_PART = 'decl'
CONTAINS_PART = 'contains'
SUBP_PART = 'subp'
EXEC_PART = 'exec'
TYPE_PART = 'type'
#TYPE_PD_PART = 'type_pd'
#TYPE_POS_PART = 'type_pos'
#TYPE_COMP_PART = 'type_comp'
#TYPE_BP_PART_CONTAINS = 'type_bp_contains'
#TYPE_BP_PART = 'type_bp'
INTF_SPEC_PART = 'intf_spec'

match_classes = {
    block_statements.BeginSource: [ UNIT_PART ],
    block_statements.Program: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, EXEC_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Module: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Subroutine: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, EXEC_PART, CONTAINS_PART, SUBP_PART ],
    block_statements.Function: [ USE_PART, IMPORT_PART, IMPLICIT_PART, DECL_PART, EXEC_PART, CONTAINS_PART, SUBP_PART ],
    #block_statements.Type: [ TYPE_PD_PART, TYPE_POS_PART, TYPE_COMP_PART, TYPE_BP_PART_CONTAINS, TYPE_BP_PART ],
    block_statements.Type: [ TYPE_PART ],
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
    TYPE_PART: block_statements.private_or_sequence + block_statements.component_part + block_statements.type_bound_procedure_part,
#    TYPE_PD_PART: [ typedecl_statements.Integer ],
#    TYPE_POS_PART: block_statements.private_or_sequence,
#    TYPE_COMP_PART: block_statements.component_part,
#    TYPE_BP_PART_CONTAINS: [ statements.Contains ],
#    TYPE_BP_PART: block_statements.type_bound_procedure_part,
    INTF_SPEC_PART: block_statements.internal_subprogram + [ statements.ModuleProcedure ]
}

def get_indent(line):
    return re.match(r"[^\S\n]*", line).group()

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
                elif node.__name__.lower()=='blockdata':
                    end_obj.blocktype = 'block data'
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

def is_plugin_common_block(kernel_id, node, plugins):
    if not isinstance(plugins, list) or node is None: return False

    for pkid, pattrs in plugin_common.iteritems():
        if pkid!=kernel_id: continue
        for pname, attrs in pattrs.iteritems():
            for bname, bid in attrs['blocks'].iteritems():
                pnode, partname, part = named_parts[kernel_id][bid]
                if part==node:
                    return True
    return False

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

        if (matchfunc, callbackfunc) not in nextlist:
            nextlist.append((matchfunc, callbackfunc))

def event_point(cur_kernel_id, cur_file_type, cur_gen_stage, node, plugins=None):
    # event debugging
    #if cur_file_type=='K' and cur_gen_stage==0 and isinstance(node.kgen_stmt, statements.Call):
    #    import pdb; pdb.set_trace() 
    if not node.kgen_isvalid: return

    for plugin_name, plugin_modules in event_register.iteritems():
        if plugins and plugin_name not in plugins: continue
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
                                                if matchfunc is None or matchfunc(node):
                                                    cbfunc(node)
                                    elif node.kgen_match_class:
                                        if node.kgen_match_class is target:
                                            for matchfunc, cbfunc in funclist:
                                                if matchfunc is None or matchfunc(node):
                                                    cbfunc(node)
                                    else: raise ProgramException('Incorrect node type: %s'%node)
                                else:
                                    if isinstance(target, Gen_Statement) and node is target:
                                        for matchfunc, cbfunc in funclist:
                                            if matchfunc is None or matchfunc(node):
                                                cbfunc(node)
                                    elif isinstance(target, base_classes.Statement) and node.kgen_stmt is target:
                                        for matchfunc, cbfunc in funclist:
                                            if matchfunc is None or matchfunc(node):
                                                cbfunc(node)


plugin_default_infolist = [ 'kernel_name', 'kgen_version', 'kernel_path', 'kernel_driver_name', 'kernel_driver_callsite_args', \
    'is_openmp_app', 'is_openmp_critical', 'is_mpi_app', 'mpi_comm', 'mpi_logical', 'mpi_status_size', 'mpi_use', 'invocations', 'print_var_names', \
    'callsite_file_path', 'callsite_stmts', 'parentblock_stmt', 'topblock_stmt', 'verbose_level', 'repeat_count', 'dummy_stmt', \
    'add_mpi_frame', 'mpi_frame_np', 'verify_tol', 'walk_stmts' ]

def getinfo(name):
    if name in plugin_default_infolist: 
        if name=='kernel_name': return State.kernel['name']
        elif name=='kgen_version': return '%d.%d.%s'%tuple(Config.kgen['version'])
        elif name=='kernel_path': return os.path.abspath('%s/%s'%(Config.path['outdir'], Config.path['kernel']))
        elif name=='kernel_driver_name': return State.kernel_driver['name']
        elif name=='kernel_driver_callsite_args': return State.kernel_driver['callsite_args']
        elif name=='is_openmp_app': return Config.openmp['enabled']
        elif name=='is_openmp_critical': return Config.openmp['critical']
        elif name=='is_mpi_app': return Config.mpi['enabled']
        elif name=='mpi_comm': return Config.mpi['comm']
        elif name=='mpi_logical': return Config.mpi['logical']
        elif name=='mpi_status_size': return Config.mpi['status_size']
        elif name=='mpi_use': return Config.mpi['use_stmts']
        elif name=='invocations': return Config.invocation['triples']
        elif name=='print_var_names': return Config.debug['printvar']
        elif name=='callsite_file_path': return Config.callsite['filepath']
        elif name=='callsite_stmts': return State.callsite['stmts']
        elif name=='parentblock_stmt': return State.parentblock['stmt']
        elif name=='topblock_stmt': return State.topblock['stmt']
        elif name=='verbose_level': return Config.verify['verboselevel']
        elif name=='repeat_count': return Config.timing['repeat']
        elif name=='dummy_stmt': return statements.DummyStatement()
        elif name=='add_mpi_frame': return Config.add_mpi_frame['enabled']
        elif name=='mpi_frame_np': return Config.add_mpi_frame['np']
        elif name=='mpi_frame_mpiexec': return Config.add_mpi_frame['mpiexec']
        elif name=='verify_tol': return Config.verify['tolerance']
        elif name=='walk_stmts': return api.walk
    elif State.plugindb.has_key(name):
        return State.plugindb[name]
    else:
        raise ProgramException('No information for %s'%name)

def setinfo(name, info):
    if name in plugin_default_infolist:
        raise ProgramException('Given name is already defined as a KGen internal information name: %s'%name)
    else:
        State.plugindb[name] = info

def set_plugin_env(mod):

    mod.genkobj = genkobj
    mod.gensobj = genkobj
    mod.KGGenType = KGGenType

    mod.getinfo =  getinfo
    mod.setinfo =  setinfo

    mod.get_entity_name = get_entity_name
    mod.pack_exnamepath = pack_exnamepath
    mod.match_namepath = match_namepath

    mod.GENERATION_STAGE = GENERATION_STAGE
    mod.FILE_TYPE = FILE_TYPE
    mod.KERNEL_SELECTION = KERNEL_SELECTION

    mod.get_partname = get_partname
    mod.get_rawname = get_rawname
    mod.check_node = check_node
    mod.get_exclude_actions = get_exclude_actions

    mod.get_namedpart = get_namedpart
    mod.namedpart_gennode = namedpart_gennode
    mod.namedpart_create_subpart = namedpart_create_subpart
    mod.namedpart_link_part = namedpart_link_part
    mod.namedpart_remove_subpart = namedpart_remove_subpart
    mod.namedpart_has_node = namedpart_has_node
    mod.namedpart_get_node = namedpart_get_node
    mod.namedpart_append_node = namedpart_append_node
    mod.namedpart_append_genknode = namedpart_append_genknode
    mod.namedpart_append_gensnode = namedpart_append_gensnode
    mod.namedpart_append_comment = namedpart_append_comment
    mod.namedpart_insert_comment = namedpart_insert_comment
    mod.namedpart_insert_node = namedpart_insert_node
    mod.namedpart_insert_genknode = namedpart_insert_genknode
    mod.namedpart_insert_gensnode = namedpart_insert_gensnode
    mod.namedpart_remove_node = namedpart_remove_node

    mod.get_part = get_part
    mod.get_part_index = get_part_index
    mod.part_has_node = part_has_node
    mod.part_get_node = part_get_node
    mod.part_append_node = part_append_node
    mod.part_append_genknode = part_append_genknode
    mod.part_append_gensnode = part_append_gensnode
    mod.part_append_comment = part_append_comment
    mod.part_insert_comment = part_insert_comment
    mod.part_insert_node = part_insert_node
    mod.part_insert_genknode = part_insert_genknode
    mod.part_insert_gensnode = part_insert_gensnode
    mod.part_remove_node = part_remove_node

    mod.TAB = TAB

    mod.UNIT_PART = UNIT_PART
    mod.USE_PART = USE_PART
    mod.IMPORT_PART = IMPORT_PART
    mod.IMPLICIT_PART = IMPLICIT_PART
    mod.DECL_PART = DECL_PART
    mod.CONTAINS_PART = CONTAINS_PART
    mod.SUBP_PART = SUBP_PART
    mod.EXEC_PART = EXEC_PART
    mod.TYPE_PART = TYPE_PART
#    mod.TYPE_PD_PART = TYPE_PD_PART
#    mod.TYPE_POS_PART = TYPE_POS_PART
#    mod.TYPE_COMP_PART = TYPE_COMP_PART
#    mod.TYPE_BP_PART = TYPE_BP_PART
    mod.INTF_SPEC_PART = INTF_SPEC_PART

def init_plugins(kernel_ids):
    global plugin_common

    plugin_common = Kgen_Plugin.plugin_common

    for kernel_id in kernel_ids:
        Kgen_Plugin.plugin_common[kernel_id] = OrderedDict()
        for plugin_name, plugin_path in Config.plugin['priority'].iteritems():
            plugin_common[kernel_id][plugin_name] = OrderedDict()
            plugin_common[kernel_id][plugin_name]['blocks'] =  OrderedDict()
            sys.path.insert(0, plugin_path)
            plugin_files = [x[:-3] for x in os.listdir(plugin_path) if x.endswith(".py")]
            for plugin in plugin_files:
                try:
                    mod = __import__(plugin)
                    set_plugin_env(mod)
                    match = lambda x: inspect.isclass(x) and x is not Kgen_Plugin and issubclass(x, Kgen_Plugin)
                    for name, cls in inspect.getmembers(mod, match): 
                        obj = cls()
                        if not event_register.has_key(plugin_name):
                            event_register[plugin_name] = OrderedDict() 
                        if not event_register[plugin_name].has_key(cls.__module__):
                            event_register[plugin_name][cls.__module__] = OrderedDict() 
                        if not event_register[plugin_name][cls.__module__].has_key(obj):
                            event_register[plugin_name][cls.__module__][obj] = OrderedDict() 
                        obj.register(PluginMsg(event_register[plugin_name][cls.__module__][obj]))
                except ValueError as e:
                    pass
                except: raise 


def get_namedpart(kernel_id, name):
    assert named_parts.has_key(kernel_id), 'missing kernel id %s'%str(kernel_id)
    assert named_parts[kernel_id].has_key(name), 'No part is found by the name of %s'%name

    return named_parts[kernel_id][name]

def namedpart_gennode(genfunc, clsobj, kernel_id, name, attrs=None):
    assert clsobj

    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    return  genfunc(pnode, clsobj, kernel_id, attrs=attrs)

def get_partname(name, is_partname):
    if is_partname:return name
    else: return PART_PREFIX + name

def get_rawname(name, is_partname):
    if is_partname:return name[len(PART_PREFIX):]
    else: return name

def get_part(node, name, is_partname=False):
    partname = get_partname(name, is_partname)
    return getattr(node, partname, [])

def get_part_index(node):
    pnode = node.kgen_parent
    for name in pnode.kgen_part_order:
        part = get_part(pnode, name)
        for i, partnode in enumerate(part):
            if partnode==node:
                return i, name, part
            elif isinstance(partnode, list):
                for j, subpartnode in enumerate(partnode):
                    if subpartnode==node:
                        return i, name, part
    return -1, None, None

def check_node(node, checkfunc):
    if not isinstance(node, Gen_Statement):
        import pdb; pdb.set_trace()
        raise ProgramException('Not Gen_Statement type')

    return callable(checkfunc) and checkfunc(node)

def namedpart_create_subpart(pnode, name, rawname, index=None):
    assert pnode

    kernel_id = pnode.kgen_kernel_id
    if not named_parts.has_key(kernel_id): named_parts[kernel_id] = OrderedDict()
    
    named_part = []
    named_parts[kernel_id][name] = (pnode, rawname, named_part)
    part_insert_node(pnode, rawname, index, named_part)
    return named_part

def namedpart_link_part(pnode, name, rawname):
    assert pnode

    kernel_id = pnode.kgen_kernel_id
    if not named_parts.has_key(kernel_id): named_parts[kernel_id] = OrderedDict()
   
    part = get_part(pnode, rawname) 
    named_parts[kernel_id][name] = (pnode, rawname, part)
    return part

def namedpart_remove_subpart(kernel_id, name):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    part_remove_node(pnode, rawname, named_part)
    return named_part

def namedpart_has_node(kernel_id, name, checks, is_partname=False):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    for node in named_part:
        if check_node(node, checks): return True
    return False

def namedpart_get_node(kernel_id, name, checks, is_partname=False):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    for node in named_part:
        if check_node(node, checks): return node

def namedpart_append_node(kernel_id, name, node):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    named_part.append(node)
    return node

def namedpart_append_gennode(genfunc, kernel_id, name, clsobj, attrs=None):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    node = namedpart_gennode(genfunc, clsobj, kernel_id, name, attrs=attrs)
    named_part.append(node)
    return node

def namedpart_append_genknode(kernel_id, name, clsobj, attrs=None):
    return namedpart_append_gennode(genkobj, kernel_id, name, clsobj, attrs=attrs)

def namedpart_append_gensnode(kernel_id, name, clsobj, attrs=None):
    return namedpart_append_gennode(gensobj, kernel_id, name, clsobj, attrs=attrs)

def namedpart_append_comment(kernel_id, name, comment_string, style=None):
    return namedpart_append_gensnode(kernel_id, name, statements.Comment, attrs={'comment': comment_string, 'style':style})

def namedpart_insert_comment(kernel_id, name, index, comment_string, style=None):
    return namedpart_insert_gensnode(kernel_id, name, statements.Comment, index, attrs={'comment': comment_string, 'style':style})

def namedpart_insert_node(kernel_id, name, index, node):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    named_part.insert(index, node)
    return node

def namedpart_insert_gennode(genfunc, kernel_id, name, clsobj, index, attrs=None):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    node = namedpart_gennode(genfunc, clsobj, kernel_id, name, attrs=attrs)
    named_part.insert(index, node)
    return node

def namedpart_insert_genknode(kernel_id, name, clsobj, index, attrs=None):
    return namedpart_insert_gennode(genkobj, kernel_id, name, clsobj, index, attrs=attrs)

def namedpart_insert_gensnode(kernel_id, name, clsobj, index, attrs=None):
    return namedpart_insert_gennode(gensobj, kernel_id, name, clsobj, index, attrs=attrs)

def namedpart_remove_node(kernel_id, name, node):
    pnode, rawname, named_part = get_namedpart(kernel_id, name)
    named_part.remove(node)
    return node

def part_has_node(pnode, name, checks, is_partname=False):
    part = get_part(pnode, name, is_partname=is_partname)
    for node in part:
        if isinstance(node, list):
            for subnode in node:
                if check_node(subnode, checks): return True
        elif check_node(node, checks): return True
    return False

def part_get_node(pnode, name, checks, is_partname=False):
    part = get_part(pnode, name, is_partname=is_partname)
    for node in part:
        if isinstance(node, list):
            for subnode in node:
                if check_node(subnode, checks): return subnode
        elif check_node(node, checks): return node

def part_append_node(pnode, name, node, is_partname=None):
    part = get_part(pnode, name, is_partname=is_partname)
    part.append(node)
    return node

def part_append_gennode(genfunc, pnode, name, clsobj, attrs=None, is_partname=None):
    part = get_part(pnode, name, is_partname=is_partname)
    node = genfunc(pnode, clsobj, pnode.kgen_kernel_id, attrs=attrs)
    part.append(node)
    return node

def part_append_genknode(pnode, name, clsobj, attrs=None, is_partname=None):
    return part_append_gennode(genkobj, pnode, name, clsobj, attrs=attrs, is_partname=is_partname)

def part_append_gensnode(pnode, name, clsobj, attrs=None, is_partname=None):
    return part_append_gennode(gensobj, pnode, name, clsobj, attrs=attrs, is_partname=is_partname)

#def part_append_comment(pnode, name, comment_string, is_partname=None, style=None):
#    part = get_part(pnode, name, is_partname=is_partname)
#    comment = gensobj(pnode, statements.Comment, pnode.kgen_kernel_id, attrs={'comment': comment_string, 'style':style})
#    part.append(comment)
#    return comment

def part_append_comment(pnode, name, comment_string, is_partname=None, style=None):
    comment = gensobj(pnode, statements.Comment, pnode.kgen_kernel_id, attrs={'comment': comment_string, 'style':style})
    return part_append_node(pnode, name, comment, is_partname=is_partname)

def part_insert_comment(pnode, name, index, comment_string, is_partname=None, style=None):
    comment = gensobj(pnode, statements.Comment, pnode.kgen_kernel_id, attrs={'comment': comment_string, 'style':style})
    return part_insert_node(pnode, name, index, comment, is_partname=is_partname)

def part_insert_node(pnode, name, index, node, is_partname=None):
    part = get_part(pnode, name, is_partname=is_partname)
    if index is None: index = len(part)
    part.insert(index, node)
    return node

def part_insert_gennode(genfunc, pnode, name, clsobj, index, attrs=None, is_partname=None):
    part = get_part(pnode, name, is_partname=is_partname)
    node = genfunc(pnode, clsobj, pnode.kgen_kernel_id, attrs=attrs)
    part.insert(index, node)
    return node

def part_insert_genknode(pnode, name, clsobj, index, attrs=None, is_partname=None):
    return part_insert_gennode(genkobj, pnode, name, clsobj, index, attrs=attrs, is_partname=is_partname)

def part_insert_gensnode(pnode, name, clsobj, index, attrs=None, is_partname=None):
    return part_insert_gennode(gensobj, pnode, name, clsobj, index, attrs=attrs, is_partname=is_partname)

def part_remove_node(pnode, name, node, is_partname=None):
    part = get_part(pnode, name, is_partname=is_partname)
    part.remove(node)
    return node

#######################################3

def create_part(node, name, is_partname=False):
    partname = get_partname(name, is_partname)
    setattr(node, partname, [])
    return getattr(node, partname)

def remove_part(node, name, is_partname=False):
    partname = get_partname(name, is_partname)
    delattr(node, partname)

def get_index_part_order(node, name, is_partname=False):
    rawname = get_rawname(name, is_partname)
    return node.kgen_part_order.index(rawname)

def unregister_part_from_part_order(node, name, is_partname=False):
    rawname = get_rawname(name, is_partname)
    node.kgen_part_order.remove(rawname)

def register_part_to_part_order(node, index, name, is_partname=False):
    rawname = get_rawname(name, is_partname)
    node.kgen_part_order.insert(index, rawname)

def insert_item_in_part(node, name, index, item, is_partname=False):
    part = get_part(node, name, is_partname=is_partname)
    part.insert(index, item)

def insert_itemlist_in_part(node, name, index, itemlist, is_partname=False):
    part = get_part(node, name, is_partname=is_partname)
    part[index:index] = itemlist

def append_item_in_part(node, name, item, is_partname=False):
    part = get_part(node, name, is_partname=is_partname)
    part.append(item)

def extend_part(node, name, items, is_partname=False):
    part = get_part(node, name, is_partname=is_partname)
    part.extend(items)

def replace_part(node, old_part, new_part):
    for partname in [p for p in dir(node) if p.startswith(PART_PREFIX)]:
        part = get_part(node, partname, is_partname=True)
        if part==old_part:
            setattr(node, partname, new_part)

def get_mypart(node, only_in_part_order=True):
    if only_in_part_order:
        upper_node = node.kgen_parent
        for name in upper_node.kgen_part_order:
            part = get_part(upper_node, name)
            if node in part: return part
    else:
        pass

def discard_items_in_part(node, name, is_partname=False):
    part = get_part(node, name, is_partname=is_partname)
    part = []



###################################


########### Statement ############
class Gen_Statement(object):
    kgen_gen_attrs = {'indent': '', 'span': None}

    def __init__(self, parent, stmt, match_class, kernel_id, attrs=None):
        self.kgen_isvalid = True
        self.kgen_parent = parent
        self.kgen_stmt = stmt
        self.kgen_match_class = match_class
        self.kgen_kernel_id = kernel_id
        self.kgen_use_tokgen = False

        if attrs:
            for key, value in attrs.iteritems():
                setattr(self, key, value)

    def statement_created(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.NODE_CREATED, self, plugins=plugins)

    def statement_process(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.BEGIN_PROCESS, self, plugins=plugins)

    def statement_finalize(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.FINISH_PROCESS, self, plugins=plugins)

    def statement_flatten(self, kernel_id, plugins):
        pass

    def str_unresolved(self, stmt):
        from kgen_state import ResState

        if not hasattr(stmt, 'unknowns'): return ''

        l = []
        for uname, req in stmt.unknowns.items():
            if req.state != ResState.RESOLVED:
                l.append(uname.firstpartname())
        if l:
            output = ' !!! UNRESOLVED !!! %s'%', '.join(l)
            Logger.warn(output, stmt=stmt)
            return output
        else: return ''

    def pack_fortran_line(self, indent, line, comment):

        lines = []

        maxline = Config.fort['maxlinelen'] - 2
        splitline = line.split(' ')
        stripline = line.strip()
        ompline = stripline.upper().replace(' ', '')
        isomp = False
        if len(ompline)>4 and stripline[:5]=='!$OMP':
            isomp = True
        elif len(stripline)>0 and stripline[0]=='!':
            iscomment = True
        else:
            iscomment = False

        tmpline = indent[:]
        for l in splitline:
            if not l: l = ' '
            l += ' '
            if len(tmpline) + len(l) > maxline:
                lines.append(tmpline+'&')
                if isomp:
                    tmpline = indent[:] + '!$OMP ' + l
                elif iscomment:
                    tmpline = indent[:] + '!' + l
                else:
                    tmpline = indent[:] + '&' + l

                while len(tmpline) > maxline:
                    lines.append(tmpline[:maxline]+'&')
                    p = tmpline[maxline:]
                    if isomp:
                        tmpline = indent[:] + '!$OMP ' + p
                    elif iscomment:
                        tmpline = indent[:] + '!' + p
                    else:
                        tmpline = indent[:] + '&' + p
            else:
                tmpline += l

        if len(tmpline)>1:
            if tmpline[-1]=='&':
                lines.append(tmpline[:-1])
            else:
                lines.append(tmpline)
        if comment:
            lines.append(comment)

        return '\n'.join(lines)

    def tostring(self):

        if isinstance(self.kgen_stmt, statements.Comment):
            if self.kgen_stmt:
                if hasattr(self, 'kgen_forced_line'):
                    if isinstance(self.kgen_forced_line, str):
                        return self.kgen_forced_line
                    elif isinstance(self.kgen_forced_line, bool) and not self.kgen_forced_line:
                        return
                    else:
                        raise ProgramException('Wrong use of kgen_forced_line')

                elif not self.kgen_stmt.item.comment.startswith('!KGEN#'):
                    start = self.kgen_stmt.item.span[0]-1
                    end = self.kgen_stmt.item.span[1]
                    lines = self.kgen_stmt.top.prep[start:end]
                    lines_str = '\n'.join(lines)
                    if lines_str.strip().startswith(self.kgen_stmt.item.comment.strip()):
                        if hasattr(self, 'kgen_parent') and self.kgen_parent.kgen_isvalid:
                            return lines_str
            else:
                return self.comment
        elif self.kgen_isvalid:
            cur_indent = self.kgen_gen_attrs['indent']
            if self.kgen_stmt:
                lines_str = None
                unres_str = self.str_unresolved(self.kgen_stmt)

                if hasattr(self, 'kgen_forced_line'):
                    lines_str = self.kgen_forced_line
                elif hasattr(self.kgen_stmt, 'item') and hasattr(self.kgen_stmt.item, 'span'):
                    if not self.kgen_stmt.item.span is self.kgen_gen_attrs['span']:
                        start = self.kgen_stmt.item.span[0]-1
                        end = self.kgen_stmt.item.span[1]
                        lines = self.kgen_stmt.top.prep[start:end]
                        lines_str = '\n'.join(lines)
                        self.kgen_gen_attrs['span'] = self.kgen_stmt.item.span
                    else:
                        return

                #if isinstance(self, GenK_Statement) and lines_str and lines_str.find('MPI_finalize')>0:
                #    import pdb; pdb.set_trace()

                if self.kgen_use_tokgen:
                    if isinstance(self.kgen_stmt, block_statements.BeginStatement) and \
                        not self.kgen_stmt.__class__ in [ block_statements.If ]:
                        self.kgen_indent = cur_indent
                        self.kgen_gen_attrs['indent'] = cur_indent + TAB
                    elif isinstance(self.kgen_stmt, base_classes.EndStatement):
                        self.kgen_gen_attrs['indent'] = self.kgen_parent.kgen_indent
                        cur_indent = self.kgen_parent.kgen_indent

                    return self.pack_fortran_line(cur_indent, self.tokgen(), unres_str)
                else:
                    if isinstance(lines_str, str):
                        cur_indent = get_indent(lines_str)
                        if isinstance(self.kgen_stmt, base_classes.BeginStatement) and \
                             not self.kgen_stmt.__class__ in [ block_statements.If ]:
                            self.kgen_indent = cur_indent
                            self.kgen_gen_attrs['indent'] = cur_indent + TAB
                        else:
                            self.kgen_gen_attrs['indent'] = cur_indent
                        return lines_str + unres_str
                    elif isinstance(lines_str, bool) and not lines_str:
                        return
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
                    if not kgenstr is None:
                        #return self.kgen_parent.kgen_indent + kgenstr
                        return self.pack_fortran_line(self.kgen_parent.kgen_indent, kgenstr, None)
                else:
                    kgenstr = self.tokgen()
                    if not kgenstr is None:
                        #return cur_indent + kgenstr
                        return self.pack_fortran_line(cur_indent, kgenstr, None)
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

    def flatten(self, kernel_id, plugins):
        return self.statement_flatten(kernel_id, plugins)

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

    def flatten(self, kernel_id, plugins):
        return self.statement_flatten(kernel_id, plugins)

########### BeginStatement ############
class Gen_BeginStatement(object):
    def beginstatement_init(self, stmt, match_class):

        self.kgen_end_obj = None

        self.kgen_part_order = []
        if match_classes.has_key(match_class):
            # add partition
            for name in match_classes[match_class]:
                partname = get_partname(name, False)
                self.kgen_part_order.append(name)
                setattr(self, partname, [])
        else:
            # add default partition
            for name in default_part_names:
                partname = get_partname(name, False)
                self.kgen_part_order.append(name)
                setattr(self, partname, [])

        if self.kgen_file_type==FILE_TYPE.KERNEL:
            #if not self.kgen_isvalid: return
            genobj = genkobj
        elif self.kgen_file_type==FILE_TYPE.STATE:
            genobj = gensobj

        if stmt:
            insert_order = self.kgen_part_order[:]
            #if isinstance(stmt, block_statements.Interface) and stmt.content[0].name=='mpas_decomp_function':
            #    import pdb; pdb.set_trace()
            for node in stmt.content[:-1]:
                item = genobj(self, node, self.kgen_kernel_id)
                insert_order = self.insert_in_order(item, insert_order)

            if isinstance(stmt.content[-1], base_classes.EndStatement):
                self.kgen_end_obj = genobj(self, stmt.content[-1], self.kgen_kernel_id)
                self.kgen_end_obj.kgen_isvalid = self.kgen_isvalid
                if hasattr(self.kgen_end_obj.kgen_stmt, 'blocktype'):
                    self.kgen_end_obj.blocktype = self.kgen_end_obj.kgen_stmt.blocktype
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

        if not self.kgen_stmt.__class__ in [ block_statements.If ]:
            lines.extend(self.tostring_parts())

        if self.kgen_end_obj:
            if self.kgen_end_obj.kgen_file_type==FILE_TYPE.KERNEL:
                l = super(GenK_Statement, self.kgen_end_obj).tostring()
            else:
                l = super(GenS_Statement, self.kgen_end_obj).tostring()
            #l = self.kgen_end_obj.tostring()
            if l is not None: lines.append(l)

        return '\n'.join(lines)

    def tostring_parts(self):
        lines = []
        for name in self.kgen_part_order:
            for node in get_part(self, name):
                if isinstance(node, list):
                    for sub_node in node:
                        l = sub_node.tostring()
                        if l is not None: lines.append(l)
                else:
                    l = node.tostring()
                    if l is not None: lines.append(l)
        return lines

    def beginstatement_created(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.NODE_CREATED, self, plugins=plugins)

        temp_parts = OrderedDict()
        for name in self.kgen_part_order:
            part = getattr(self, get_partname(name, False))
            temp_parts[name] = part[:]

        for name in self.kgen_part_order:
            for node in temp_parts[name]:
                if isinstance(node, list):
                    for sub_node in node:
                        sub_node.created(plugins)
                else:
                    node.created(plugins)

    def beginstatement_process(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.BEGIN_PROCESS, self, plugins=plugins)

        temp_parts = OrderedDict()
        for name in self.kgen_part_order:
            part = getattr(self, get_partname(name, False))
            temp_parts[name] = part[:]

        for name in self.kgen_part_order:
            for node in temp_parts[name]:
                if isinstance(node, list):
                    for sub_node in node:
                        sub_node.process(plugins)
                else:
                    node.process(plugins)

    def beginstatement_finalize(self, plugins):
        event_point(self.kgen_kernel_id, self.kgen_file_type, GENERATION_STAGE.FINISH_PROCESS, self, plugins=plugins)

        temp_parts = OrderedDict()
        for name in self.kgen_part_order:
            part = getattr(self, get_partname(name, False))
            temp_parts[name] = part[:]

        for name in self.kgen_part_order:
            for node in temp_parts[name]:
                if isinstance(node, list):
                    for sub_node in node:
                        sub_node.finalize(plugins)
                else:
                    node.finalize(plugins)

    def beginstatement_flatten(self, kernel_id, plugins):
        #import pdb; pdb.set_trace()
        for name in self.kgen_part_order:
            part = getattr(self, get_partname(name, False))
            flatten_part = []
            for node in part:
                if isinstance(node, list):
                    for sub_node in node:
                        sub_node.flatten(kernel_id, plugins)
                    if is_plugin_common_block(kernel_id, node, plugins):
                        flatten_part.append(node)
                    else:
                        flatten_part.extend(node) 
                else:
                    node.flatten(kernel_id, plugins)
                    flatten_part.append(node)
            setattr(self, get_partname(name, False), flatten_part)

    def insert_in_order(self, item, insert_order):

        if item.kgen_stmt.__class__ is statements.Comment:
            append_item_in_part(self, insert_order[0], item)
            return insert_order

        new_order = []
        matched = False
        for name in insert_order:
            if not matched:
                if isinstance(item.kgen_stmt, statements.StmtFuncStatement):
                    if statements.Assignment in part_classes[name]:
                        matched = True
                        append_item_in_part(self, name, item)
                    elif statements.StmtFuncStatement in part_classes[name]:
                        if item.kgen_stmt.issfs:
                            matched = True
                            append_item_in_part(self, name, item)
                    else:
                        #import pdb; pdb.set_trace()
                        raise ProgramException('Wrong sequence for StmtFuncStatement: %s'%item.kgen_stmt.__class__)
                elif item.kgen_stmt.__class__ in part_classes[name]:
                    matched = True
                    append_item_in_part(self, name, item)
            if matched:
                new_order.append(name)

        if not matched:
            #import pdb; pdb.set_trace()
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

    def flatten(self, kernel_id, plugins):
        self.beginstatement_flatten(kernel_id, plugins)

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

    def flatten(self, kernel_id, plugins):
        self.beginstatement_flatten(kernel_id, plugins)

    def tostring(self):
        return self.beginstatement_tostring()

########### functions ############

def generate_kgen_utils(k_id):
    from kgen_extra import kgen_utils_file_head, kgen_utils_file_checksubr, \
        kgen_get_newunit, kgen_error_stop, kgen_utils_file_tostr, kgen_utils_array_sumcheck

    with open('%s/kgen_utils.f90'%Config.path['kernel'], 'wb') as f:
        f.write('MODULE kgen_utils_mod')
        f.write(kgen_utils_file_head)
        f.write('\n')
        f.write('CONTAINS')
        f.write('\n')
        f.write(kgen_utils_array_sumcheck)
        f.write(kgen_utils_file_tostr)
        f.write(kgen_utils_file_checksubr)
        f.write(kgen_get_newunit)
        f.write(kgen_error_stop)
        f.write('END MODULE kgen_utils_mod\n')

#def generate_srcfiles():
#    """Generate files."""
#
#    # setup plugin framework
#    init_plugins([KERNEL_ID_0])
#
#    # generate kgen_driver.f90 in kernel directory
#    driver = genkobj(None, block_statements.BeginSource, KERNEL_ID_0)
#    program = genkobj(driver, block_statements.Program, KERNEL_ID_0)
#    program.name = getinfo('kernel_driver_name')
#    append_item_in_part(driver, UNIT_PART, program)
#
#    # construct a generation tree
#    genfiles = []
#    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
#        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
#
#            kfile = genkobj(None, srcobj.tree, KERNEL_ID_0)
#            sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
#            if kfile is None or sfile is None:
#                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
#            genfiles.append((kfile, sfile, filepath))
#            State.used_srcfiles[filepath] = (srcobj, mods_used, units_used)
#
#    # process each nodes in the tree
#    for plugin_name in event_register.keys():
#        for kfile, sfile, filepath in genfiles:
#            kfile.created([plugin_name])
#            sfile.created([plugin_name])
#        driver.created([plugin_name])
#
#        for kfile, sfile, filepath in genfiles:
#            kfile.process([plugin_name])
#            sfile.process([plugin_name])
#        driver.process([plugin_name])
#
#        for kfile, sfile, filepath in genfiles:
#            kfile.finalize([plugin_name])
#            sfile.finalize([plugin_name])
#        driver.finalize([plugin_name])
#
#        for kfile, sfile, filepath in genfiles:
#            kfile.flatten(KERNEL_ID_0, [plugin_name])
#            sfile.flatten(KERNEL_ID_0, [plugin_name])
#        driver.flatten(KERNEL_ID_0, [plugin_name])
#        #named_part = OrderedDict()
#
#    # generate source files from each node of the tree
#    for kfile, sfile, filepath in genfiles:
#        filename = os.path.basename(filepath)
#        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
#        klines = kfile.tostring()
#        if klines is not None:
#            with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
#                fd.write(klines)
#
#        if sfile.kgen_stmt.used4genstate:
#            Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
#            slines = sfile.tostring()
#            if slines is not None:
#                with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
#                    fd.write(slines)
#
#    with open('%s/%s.f90'%(Config.path['kernel'], getinfo('kernel_driver_name')), 'wb') as fd:
#        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
#        lines = driver.tostring()
#        if lines is not None: fd.write(lines)
#
#    # generate kgen_utils.f90 in kernel directory
#    generate_kgen_utils(KERNEL_ID_0)
#
#    State.state = State.STATE_GENERATED
#
