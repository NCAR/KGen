# kgen_genstate.py

import os
from api import walk
from statements import Contains, Use, Import
from block_statements import Program, Module, EndModule, Function, implicit_part, declaration_construct, execution_part
from kgen_utils import Config
from kgen_state import State
from kgen_genutils import TAB, SrcStage, write, write_stmt, write_state_usepart_callsite, write_state_interface_write_var_mod, \
    write_state_subroutines_write_var, write_subroutines_kgen, write_state_specpart_kgen, write_state_call_module_externs, \
    write_state_write_inputs, write_state_write_outputs, write_state_write_driver_inputs, write_state_print_counter, \
    write_state_write_fileopen, write_state_fileopen_tail, write_state_fileclose_head, write_state_write_fileclose, \
    write_state_subroutine_module_externs, write_file_header, write_state_usepart_module, write_state_subroutines_type_write_var

def generate_state():
    """ Generate state files """

    # create state and kernel directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    if isinstance(State.topblock['stmt'], Program):
        generate_callsite_program()
    elif isinstance(State.topblock['stmt'], Module):
        generate_callsite_module()
    else:
        raise ProgramException('Unknown parent type: %s' % State.topblock['stmt'].__class__)

    # generate instrumented files except callsite file
    generate_state_files()

    State.state = State.STATE_GENERATED

def generate_state_files():
    """ Generate instrumented files except callsite file """

    for filepath, (srcfile, mods_used) in State.modfiles.iteritems():
        if not srcfile.used4genstate: continue
        filename = os.path.basename(srcfile.abspath)
        with open('%s/%s'%(Config.path['state'], filename), 'wb') as f:

            write_file_header(f, filename)

            target_stmt = None
            mod_name = ''
            for stmt, depth in walk(srcfile.tree, -1):

                if stmt in srcfile.tree.a.module.values():
                    mod_name = stmt.name

                    if hasattr(stmt, 'spec_stmts'):
                        contains_stmt = [ s for s in stmt.spec_stmts if isinstance(s, Contains) ]
                        if len(contains_stmt)>1: raise ProgramException('More than on Contains stmts: %s'%contains_stmt)
                        elif len(contains_stmt)==1:
                            target_stmt = contains_stmt[0]
                        else:
                            target_stmt = stmt.content[-1]
                    else:
                        target_stmt = stmt.content[-1]

                    write_stmt(f, stmt, depth)
                    write_state_usepart_module(f, depth, State.modules[mod_name])

                elif target_stmt and stmt is target_stmt:
                    mod_num = State.modules[mod_name]['num']

                    write_state_interface_write_var_mod(f, depth, State.modules[mod_name]['extern']['tkdpat'], mod_num, \
                        dtypelist=State.modules[mod_name]['dtype'])

                    if len(State.modules[mod_name]['extern']['names'])>0:
                        write(f, 'PUBLIC %s'%('kgen_write_externs_%s'%mod_name), depth)

#                    if len(State.modules[mod_name]['extern']['names'])>0 or State.modules[mod_name]['extern']['tkdpat']>0 or \
#                        isinstance(target_stmt, Contains):
#                        #write(f, 'CONTAINS', d=depth)
#                        write_state_subroutines_write_var(f, depth, State.modules[mod_name]['extern']['tkdpat'], contains=True)
#                    else:
#                        write_state_subroutines_write_var(f, depth, State.modules[mod_name]['extern']['tkdpat'])

                    if isinstance(target_stmt, Contains) or len(State.modules[mod_name]['extern']['names'])>0 or len(State.modules[mod_name]['dtype'])>0:
                        write(f, 'CONTAINS', d=depth)
                        write_state_subroutines_write_var(f, depth, State.modules[mod_name]['extern']['tkdpat'], contains=False)
                    else:
                        write_state_subroutines_write_var(f, depth, State.modules[mod_name]['extern']['tkdpat'], contains=True)

                    write_state_subroutines_type_write_var(f, depth, State.modules[mod_name]['dtype'])
                    write_state_subroutine_module_externs(f, depth, State.modules[mod_name]['extern'], mod_name)

                    if isinstance(stmt, EndModule):
                        write_stmt(f, stmt, depth)
                    target_stmt = None
                    mod_name = ''
                else: 
                    write_stmt(f, stmt, depth)

def generate_callsite_program():
    """ Generate kernel and state files with program type parent"""

    # generate call site files for state and kernel
    cs_tree = State.callsite['file'].tree
    cs_filename = os.path.basename(cs_tree.reader.id)

    # generate callsite file for state
    #prog_stage = BEFORE_PROGRAM
    prog_stmt = State.callsite['parent']
    endprog_stmt = State.callsite['parent'].content[-1]
    with open('%s/%s'%(Config.path['state'], cs_filename), 'wb') as f:
        pass

    # generate the other files
    pass

def process_call_line(f, depth, stmt):
    from Fortran2003 import Name, Call_Stmt, Part_Ref, Assignment_Stmt

    if isinstance(State.callsite['expr'], Call_Stmt):
        write_stmt(f, stmt, depth)
    elif isinstance(State.callsite['expr'], Part_Ref):
        lhs = State.callsite['stmt'].f2003.items[0]
        expr = State.callsite['expr']
        write(f, '%s = %s'%(lhs.tofortran().strip(), expr.tofortran().strip()), d=depth)
    else:
        raise ProgramException('Unknown expr type is found: %s' % State.callsite['expr'].__class__)

def process_callsite_stmt(f, depth, stmt):

    write_state_write_fileopen(f, depth)

    if Config.mpi['enabled']: depth += 2
    else: depth += 1

    write_state_call_module_externs(f, depth)

    write_state_write_driver_inputs(f, depth)

    write_state_write_inputs(f, depth)

    write_state_fileopen_tail(f, depth)

    if Config.mpi['enabled']: depth -= 2
    else: depth -= 1

    write(f, '! call to kernel', d=depth)
    process_call_line(f, depth, stmt)

    write_state_fileclose_head(f, depth)

    if Config.mpi['enabled']: depth += 2
    else: depth += 1

    write_state_write_outputs(f, depth)

    write_state_write_fileclose(f, depth)

def generate_callsite_module():
    """ Generate state files with module type parent"""

    # create state directories
    statepath = Config.path['state']
    if not os.path.exists(statepath):
        os.makedirs(statepath)

    # generate call site files for state and kernel
    cs_tree = State.topblock['file'].tree
    cs_filename = os.path.basename(cs_tree.reader.id)

    # generate callsite file for state
    mod_stage = SrcStage.BEFORE_MODULE_STMT
    mod_stmt = State.topblock['stmt']
    endmod_stmt = mod_stmt.content[-1]
    subp_stmt = State.parentblock['stmt']
    endsubp_stmt = subp_stmt.content[-1]
    contains_stmt_added = False
    parentblock_subroutine_written = False 
    with open('%s/%s'%(statepath, cs_filename), 'wb') as f:
        write_file_header(f, cs_filename)
        for stmt, depth in walk(cs_tree, -1):
            if mod_stage==SrcStage.BEFORE_MODULE_STMT:
                if stmt is mod_stmt:
                    mod_stage = SrcStage.AFTER_MODULE_STMT
                    write_stmt(f, stmt, depth)
                    write_state_usepart_callsite(f, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_STMT:
                if stmt.__class__ in  [ Use, Import ] + implicit_part:
                    # have this type of stmts wrtten before declarations
                    pass

                elif stmt.__class__ in  declaration_construct:
                    mod_stage = SrcStage.AFTER_MODULE_USE_PART
                    #if stmt.__class__ in implicit_part:
                    #    write_stmt(f, stmt, depth)
                    if len(State.topblock['extern']['names'])>0:
                        write(f, 'PUBLIC %s'%('kgen_write_externs_%s'%mod_stmt.name), depth)
                    #if not stmt.__class__ in implicit_part:
                    #    write_stmt(f, stmt, depth)
                    write_stmt(f, stmt, depth)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_MODULE_CONTAINS_STMT
                    if len(State.topblock['extern']['names'])>0:
                        write(f, 'PUBLIC %s'%('kgen_write_externs_%s'%mod_stmt.name), depth)
                    write_state_interface_write_var_mod(f, depth, State.topblock['extern']['tkdpat'], 0, State.topblock['dtype'])
                    write_stmt(f, stmt, depth)
                    write_state_subroutines_write_var(f, depth, State.topblock['extern']['tkdpat'])
                    write_state_subroutines_type_write_var(f, depth, State.topblock['dtype'])
                    write_state_subroutine_module_externs(f, depth, State.topblock['extern'], State.topblock['stmt'].name)
                    write_subroutines_kgen(f, depth)
                    continue
                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_USE_PART:
                if isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_MODULE_CONTAINS_STMT
                    # write interface for actual args and externs and kernel externs
                    write_state_interface_write_var_mod(f, depth, State.topblock['extern']['tkdpat'], 0, State.topblock['dtype'])
                    write_stmt(f, stmt, depth)
                    write_state_subroutines_write_var(f, depth, State.topblock['extern']['tkdpat'])
                    write_state_subroutines_type_write_var(f, depth, State.topblock['dtype'])
                    write_state_subroutine_module_externs(f, depth, State.topblock['extern'], State.topblock['stmt'].name)
                    write_subroutines_kgen(f, depth)
                    continue
                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_CONTAINS_STMT:
                if stmt is subp_stmt:
                    mod_stage = SrcStage.AFTER_SUBP_STMT
                    write_stmt(f, stmt, depth)
                    write_state_usepart_module(f, depth, State.parentblock)
                    continue
                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_STMT:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT
                    write_state_specpart_kgen(f, depth)
                    #write_state_interface_write_var_subp(f, depth, State.parentblock['inout_driver']['tkdpat'])
                    process_callsite_stmt(f, depth, stmt)
                    continue
                elif stmt.__class__ in  [ Use, Import ] + implicit_part:
                    # have this type of stmts wrtten before declarations
                    pass
                elif stmt.__class__ in  declaration_construct:
                    mod_stage = SrcStage.AFTER_SUBP_USE_PART
                    #write_state_specpart_kgen(f, depth)
                    #write_state_interface_write_var_subp(f, depth, State.parentblock['inout_driver']['tkdpat'])
                    #write_stmt(f, stmt, depth)
                    #continue
                elif stmt.__class__ in execution_part:
                    mod_stage = SrcStage.AFTER_SUBP_SPEC_PART
                    write_state_specpart_kgen(f, depth)
                    #write_state_interface_write_var_subp(f, depth, State.parentblock['inout_driver']['tkdpat'])
                    #write_stmt(f, stmt, depth)
                    #continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    #write_stmt(f, stmt, depth)
                    #continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT
                    #write_stmt(f, stmt, depth)
                    #continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_USE_PART:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT
                    write_state_specpart_kgen(f, depth)
                    process_callsite_stmt(f, depth, stmt)
                    continue
                elif stmt.__class__ in execution_part:
                    mod_stage = SrcStage.AFTER_SUBP_SPEC_PART
                    write_state_specpart_kgen(f, depth)
                    #process_callsite_stmt(f, depth, stmt)
                    #write_state_interface_write_var_subp(f, depth, State.parentblock['inout_driver']['tkdpat'])
                    write_stmt(f, stmt, depth)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_stmt(f, stmt, depth)
                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and not contains_stmt_added:
                        write(f, 'CONTAINS', d=depth)
                        contains_stmt_added = True

                    write_state_print_counter(f, depth)

                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_SPEC_PART:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT
                    process_callsite_stmt(f, depth, stmt)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_stmt(f, stmt, depth)
                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and not contains_stmt_added:
                        write(f, 'CONTAINS', d=depth)
                        contains_stmt_added = True

                    write_state_print_counter(f, depth)

                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_CALLSITE_STMT:
                if isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_stmt(f, stmt, depth)
                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )

                    if not_have_contains and not contains_stmt_added:
                        write(f, 'CONTAINS', d=depth)
                        contains_stmt_added = True

                    write_state_print_counter(f, depth)

                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_CONTAINS_STMT:
                if stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and not contains_stmt_added:
                        write(f, 'CONTAINS', d=depth)
                        contains_stmt_added = True

                    write_state_print_counter(f, depth)

                    if not parentblock_subroutine_written:
                        write_state_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                        write_state_subroutines_write_var(f, depth, State.parentblock['writesubr']['tkdpat'])
                        parentblock_subroutine_written = True
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_ENDSUBP_STMT:
                if stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_stmt(f, stmt, depth)
                    continue

                # place action
                write_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_ENDMODULE_STMT:

                # place action
                write_stmt(f, stmt, depth)

            else: raise ProgramException('Unknown module stage: %d'%mod_stage)

