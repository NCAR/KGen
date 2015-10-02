# kgen_genkernel.py
#
import os
from api import walk
from block_statements import Program, Module, EndModule, Function, EndFunction, implicit_part, declaration_construct, execution_part, Type
from statements import Contains, Use, Public, Access, Import, Parameter, Comment
from typedecl_statements import TypeDeclarationStatement
from kgen_utils import Config, ProgramException, KGName
from kgen_state import State
from kgen_genutils import SrcStage, write, write_kernel_stmt, write_kernel_read_inputs, \
    write_kernel_read_outputs, write_kernel_verify_outputs, write_driver_usepart_kgen, write_driver_specpart, \
    write_kernel_interface_read_var_mod, write_driver_read_fileopen, write_driver_call_module_externs, write_driver_read_fileclose, \
    write_kernel_subroutine_module_externs, write_kernel_subroutines_read_var, write_kernel_subroutines_type_verify_var, \
    write_kernel_subroutines_verify_var, write_subroutines_kgen, write_kernel_timing, write_kernel_pertcalls, \
    write_file_header, write_kernel_subroutines_type_write_var, write_kgen_utils_file, write_kernel_subroutines_perturb_var, \
    write_kernel_usepart_module, write_kernel_usepart_callsite, write_kernel_driver_inputs

class EntityDecl(object):
    def __init__(self, decl):
        from Fortran2003 import Entity_Decl
        self.decl_stmt = Entity_Decl(decl)

    def get_name(self):
        from Fortran2003 import Name
        if hasattr(self.decl_stmt, 'items') and isinstance(self.decl_stmt.items[0], Name):
            return self.decl_stmt.items[0].string.lower()
        else: raise ProgramException('Decl name is not Name type: %s'%self.decl_stmt.items[0].__class__)

    def __eq__(self, other):
        if isinstance(other, str):
            return other==self.get_name()
        elif isinstance(other, EntityDecl):
            return other.get_name()==self.get_name()
        else: return False
 
def generate_kernel():
    """ Generate kernel files """

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

    if isinstance(State.topblock['stmt'], Program):
        generate_kernel_program()
    elif isinstance(State.topblock['stmt'], Module):
        generate_kernel_module()
    else:
        raise ProgramException('Unknown top module type: %s' % State.topblock['stmt'].__class__)

    generate_kernel_files()

    State.state = State.KERNEL_GENERATED

def generate_kernel_files():

    remove_modfiles = []

    for filepath, (srcfile, mods_used) in State.modfiles.iteritems():
        filename = os.path.basename(srcfile.abspath)
        is_blank = True
        kernel_file = '%s/%s'%(Config.path['kernel'], filename)
        with open(kernel_file, 'wb') as f:
            write_file_header(f, filename)

            target_stmt = None
            mod_stmt = None 
            for stmt, depth in walk(srcfile.tree, -1):
                # if stmt is module

                if stmt in srcfile.tree.a.module.values():
                    mod_stmt = stmt
                    if hasattr(stmt, 'spec_stmts'):
                        contains_stmt = [ s for s in stmt.spec_stmts if isinstance(s, Contains) ]
                        if len(contains_stmt)>1:
                            raise ProgramException('More than on Contains stmts: %s'%contains_stmt)
                        elif len(contains_stmt)==1:
                            target_stmt = contains_stmt[0]
                        else:
                            target_stmt = stmt.content[-1]
                    else:
                        target_stmt = stmt.content[-1]

                    write_kernel_stmt(f, stmt, depth)

                    if hasattr(stmt, 'unknowns') or hasattr(stmt, 'geninfo'):
                        is_blank = False
                        write_kernel_usepart_module(f, depth, State.modules[mod_stmt.name])

                elif target_stmt and stmt is target_stmt:
                    if mod_stmt:
                        if len(State.modules[mod_stmt.name]['extern']['names'])>0:
                            write(f, 'PUBLIC %s'%('kgen_read_externs_%s'%mod_stmt.name), depth+1)

                        mod_num = State.modules[mod_stmt.name]['num']
                        write_kernel_interface_read_var_mod(f, depth, State.modules[mod_stmt.name]['extern']['tkdpat'], \
                            mod_num, State.modules[mod_stmt.name]['dtype'])

                        if isinstance(stmt, Contains):
                            write_kernel_stmt(f, stmt, depth)
                            write_kernel_subroutines_read_var(f, depth, State.modules[mod_stmt.name]['extern']['tkdpat'])
                        elif len(State.modules[mod_stmt.name]['extern']['names'])>0 or len(State.modules[mod_stmt.name]['dtype'])>0:
                            write(f, 'CONTAINS', d=depth)
                            write_kernel_subroutines_read_var(f, depth, State.modules[mod_stmt.name]['extern']['tkdpat'])
                        else:
                            write_kernel_subroutines_read_var(f, depth, State.modules[mod_stmt.name]['extern']['tkdpat'], contains=True)

                        write_kernel_subroutine_module_externs(f, depth, State.modules[mod_stmt.name]['extern'], mod_stmt.name)
                        write_kernel_subroutines_type_write_var(f, depth, State.modules[mod_stmt.name]['dtype'])
                        write_kernel_subroutines_type_verify_var(f, depth, State.modules[mod_stmt.name]['dtype'])

                        if isinstance(stmt, EndModule):
                            write_kernel_stmt(f, stmt, depth)
                        target_stmt = None
                        mod_stmt = None
                    else:
                        raise PrpgramException('None target statement')
                else: 
                    if isinstance(stmt, Use):
                        process_kernel_module_use_stmt(f, stmt, depth)
                    elif isinstance(stmt, TypeDeclarationStatement):
                        process_kernel_module_typedecl_stmt(f, stmt, depth)
                    elif isinstance(stmt, Access) or isinstance(stmt, Parameter):
                        process_kernel_module_spec_stmt(f, stmt, depth)
                    else:
                        write_kernel_stmt(f, stmt, depth)

            if is_blank:
                os.remove(kernel_file)
                remove_modfiles.append(filepath)

    for remove_modfile in remove_modfiles:
        #import pdb; pdb.set_trace()
        del State.modfiles[remove_modfile]

def generate_kernel_program():
    """ Generate kernel and state files with program type parent"""

    # generate call site files for state and kernel
    cs_tree = State.topblock['file'].tree
    cs_filename = os.path.basename(cs_tree.reader.id)

    # generate callsite file for state
    #prog_stage = BEFORE_PROGRAM
    prog_stmt = State.parentblock['stmt']
    endprog_stmt = State.parentblock['stmt'].content[-1]
    with open('%s/%s'%(Config.path['kernel'], cs_filename), 'wb') as f:
        pass

def generate_kernel_module():
    """ Generate kernel files with module type parent"""

    generate_kernel_module_callsite()
    generate_kernel_module_driver()
    generate_kernel_utils()

def process_kernel_callsite_stmt(f, stmt, depth):
    #from statements import Assignment
    from Fortran2003 import Name, Call_Stmt, Part_Ref, Assignment_Stmt

    write(f, 'tolerance = %s'%Config.verify['tolerance'], d=depth)
    write(f, 'CALL kgen_init_check(check_status, tolerance)', d=depth)

    write_kernel_read_inputs(f, depth)
    write_kernel_read_outputs(f, depth)
    write_kernel_pertcalls(f, depth)
    write(f, '! call to kernel', d=depth)
    if isinstance(State.callsite['expr'], Call_Stmt):
        write_kernel_stmt(f, stmt, depth)
        #write(f, str(State.callsite['expr']), d=depth)
    elif isinstance(State.callsite['expr'], Part_Ref):
    #if isinstance(stmt, Assignment):
        lhs = State.callsite['stmt'].f2003.items[0]
        expr = State.callsite['expr']
        write(f, '%s = %s'%(lhs.tofortran().strip(), expr.tofortran().strip()), d=depth)
    else:
        raise ProgramException('Unknown expr type is found: %s' % State.callsite['expr'].__class__)

    write_kernel_verify_outputs(f, depth)
    write(f, 'CALL kgen_print_check("%s", check_status)'%State.kernel['stmt'].name, d=depth)
    write_kernel_timing(f, depth)

def process_kernel_module_spec_stmt(f, stmt, depth):
    # if stmt is a res
    if hasattr(stmt, 'geninfo'):
        entities = []
        if stmt.geninfo.has_key(KGName):
            for kgname in stmt.geninfo[KGName]:
                if isinstance(stmt, Parameter):
                    if kgname.firstpartname() not in [ entity.split('=')[0].strip() for entity in entities ]:
                        if hasattr(stmt, 'leftnames') and kgname.firstpartname() in stmt.leftnames:
                            entities.append(stmt.items[stmt.leftnames.index(kgname.firstpartname())])
                        else:
                            raise ProgramException('%s is not found in %s' % ( kgname.firstpartname(), str(stmt)))
                else:
                    if kgname.firstpartname() not in entities:
                        entities.append(kgname.firstpartname())
        write_kernel_stmt(f, stmt, depth, items=entities)

def process_kernel_module_use_stmt(f, stmt, depth):
    # if stmt is a res
    if hasattr(stmt, 'geninfo'):
        entities = []
        if stmt.geninfo.has_key(KGName):
            for kgname in stmt.geninfo[KGName]:
                if kgname.firstpartname() not in entities:
                    entities.append(kgname.firstpartname())
        # for each entity
        for entity in entities:
            if entity in stmt.norenames:
                write_kernel_stmt(f, stmt, depth, items=[entity])
            else:
                in_renames = False
                for (n,o) in stmt.renames:
                    if entity==n:
                        write_kernel_stmt(f, stmt, depth, items=['%s => %s'%(n,o)])
                        in_renames = True
                        break
                if not in_renames:
                    # TODO for rayl issue
                    #import pdb ;pdb.set_trace()
                    write(f, 'USE %s, only : %s'%(stmt.name, entity), d=depth)

def add_public_stmt(f, depth):

    #if len(State.topblock['dtype'])>0:
    #    write(f, 'PUBLIC kgen_read_var', depth)

    if not hasattr(State.topblock['stmt'], 'spec_stmts'):
        write(f, 'PUBLIC %s'%State.parentblock['stmt'].name, depth)
    else:
        public_stmts = [ stmt for stmt in State.topblock['stmt'].spec_stmts if isinstance(stmt, Public) ]
        if len(public_stmts)==0:
            write(f, 'PUBLIC %s'%State.parentblock['stmt'].name, depth)
        else:
            for public_stmt in public_stmts:
                if public_stmt.items and State.parentblock['stmt'].name in public_stmt.items:
                    if not hasattr(public_stmt, 'geninfo'):
                        write(f, 'PUBLIC %s'%State.parentblock['stmt'].name, depth)
                    else:
                        items = [ n.firstpartname() for n in public_stmt.geninfo[KGName] ]
                        if not State.parentblock['stmt'].name in items:
                            write(f, 'PUBLIC %s'%State.parentblock['stmt'].name, depth)
                    return
            write(f, 'PUBLIC %s'%State.parentblock['stmt'].name, depth)

def process_kernel_module_typedecl_stmt(f, stmt, depth):

    # if stmt is a component of a derived type 
    if isinstance(stmt.parent, Type):
        write_kernel_stmt(f, stmt, depth)
    # if stmt is a res
    elif hasattr(stmt, 'geninfo'):
        entities = []
        if stmt.geninfo.has_key(KGName):
            for kgname in stmt.geninfo[KGName]:
                if kgname.firstpartname() not in entities:
                    entities.append(kgname.firstpartname())
        # for each entity
        for entity in entities:
            #decl = [e for e in stmt.entity_decls if e.split('(')[0].strip()==entity.lower()]
            org_decl = [e for e in stmt.entity_decls if EntityDecl(e)==entity.lower()]

            # checking
            decl = [EntityDecl(e).get_name() for e in stmt.entity_decls if EntityDecl(e)==entity.lower()]
            if len(decl)>1: raise ProgramException('More than one entity is found: %s'%str(decl))

            if stmt.parent is State.topblock['stmt'] and \
                stmt in State.parentblock['output']['res_stmt'].values():
                ref_decl = [ 'ref_'+org_decl[0] ]
                write_kernel_stmt(f, stmt, depth, items=ref_decl)
            
            write_kernel_stmt(f, stmt, depth, items=org_decl)



def process_kernel_callsite_typedecl_stmt(f, stmt, depth):
    # if stmt is a res
    if hasattr(stmt, 'geninfo'):
        entities = []
        if stmt.geninfo.has_key(KGName):
            for kgname in stmt.geninfo[KGName]:
                if kgname.firstpartname() not in entities:
                    entities.append(kgname.firstpartname())

        # output names
        outnames = [ name.firstpartname() for name in State.parentblock['output']['names']]

        # for each entity
        for entity in entities:
            org_decl = [e for e in stmt.entity_decls if EntityDecl(e)==entity.lower()]
            decl = [EntityDecl(e).get_name() for e in stmt.entity_decls if EntityDecl(e)==entity.lower()]
            if len(decl)>1: raise ProgramException('More than one entity is found: %s'%str(decl))
            var = stmt.parent.a.variables[decl[0]]
            if not decl[0] in outnames:
                write_kernel_stmt(f, stmt, depth, items=org_decl)

            for outname in outnames:
                if outname==decl[0]:
                    res_stmt = State.parentblock['output']['res_stmt'][outname]
                    var = res_stmt.parent.a.variables[decl[0]]
                    dimattr = []
                    for attr in var.parent.attrspec:
                        if attr.startswith('dimension'):
                            dimattr.append(attr)
                    delattrs = ['intent(in)', 'intent(out)', 'intent(inout)'] + dimattr
                    if var.is_array():
                        addattrs = []
                        if var.is_explicit_shape_array():
                            arrspec = var.get_array_spec()
                            dim = '(%s)'%','.join([':'.join(bnd) for bnd in arrspec])
                        else:
                            dim = '(%s)'%','.join([':']*var.rank)
                            if 'allocatable' not in var.parent.attrspec:
                                addattrs = ['allocatable']

                        if var.is_pointer():
                            write_kernel_stmt(f, stmt, depth, items=[ '%s%s'%(decl[0],dim)+' => NULL()' ], delattr=delattrs)
                            write_kernel_stmt(f, stmt, depth, items=[ 'ref_%s%s'%(decl[0],dim)+' => NULL()' ], delattr=delattrs)
                        else:
                            write_kernel_stmt(f, stmt, depth, items=[ '%s%s'%(decl[0],dim) ], delattr=delattrs, addattr=addattrs)
                            write_kernel_stmt(f, stmt, depth, items=[ 'ref_%s%s'%(decl[0],dim) ], delattr=delattrs, addattr=addattrs)
                    else:
                        if var.is_pointer():
                            write_kernel_stmt(f, stmt, depth, items=[ decl[0]+' => NULL()' ], delattr=delattrs)
                            write_kernel_stmt(f, stmt, depth, items=[ 'ref_'+decl[0]+' => NULL()' ], delattr=delattrs)
                        else:
                            write_kernel_stmt(f, stmt, depth, items=[ decl[0] ], delattr=delattrs)
                            write_kernel_stmt(f, stmt, depth, items=[ 'ref_'+decl[0] ], delattr=delattrs)
                    break

def generate_kernel_module_callsite():
    """ Generate kernel callsite file with module type parent"""
    
    # generate call site files for state and kernel
    cs_tree = State.topblock['file'].tree
    cs_filename = os.path.basename(cs_tree.reader.id)

    # generate callsite file for state
    mod_stage = SrcStage.BEFORE_MODULE_STMT
    mod_stmt = State.topblock['stmt']
    endmod_stmt = mod_stmt.content[-1]
    subp_stmt = State.parentblock['stmt']
    endsubp_stmt = subp_stmt.content[-1]
    with open('%s/%s'%(Config.path['kernel'], cs_filename), 'wb') as f:
        write_file_header(f, cs_filename)
        for stmt, depth in walk(cs_tree, -1):
            if mod_stage==SrcStage.BEFORE_MODULE_STMT:
                if stmt is mod_stmt:
                    mod_stage = SrcStage.AFTER_MODULE_STMT
                    write_kernel_stmt(f, stmt, depth)
                    write_kernel_usepart_callsite(f, depth)
                    continue

                # place action
                write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_STMT:
                if stmt.__class__ in  [ Use, Import ] + implicit_part:
                    # have this type of stmts wrtten before declarations
                    pass

                elif stmt.__class__ in  declaration_construct:
                    mod_stage = SrcStage.AFTER_MODULE_USE_PART

                    if isinstance(stmt, TypeDeclarationStatement):
                        process_kernel_module_typedecl_stmt(f, stmt, depth)
                    elif isinstance(stmt, Access) or isinstance(stmt, Parameter):
                        process_kernel_module_spec_stmt(f, stmt, depth)
                    else:
                        write_kernel_stmt(f, stmt, depth)

                    if len(State.topblock['extern']['names'])>0:
                        write(f, 'PUBLIC %s'%('kgen_read_externs_%s'%mod_stmt.name), depth)

                    add_public_stmt(f, depth)


                    write_kernel_interface_read_var_mod(f, depth, [], 0, \
                        dtypelist=State.topblock['dtype'])

                    continue

                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_MODULE_CONTAINS_STMT
                    if len(State.topblock['extern']['names'])>0:
                        write(f, 'PUBLIC %s'%('kgen_read_externs_%s'%mod_stmt.name), depth)

                    add_public_stmt(f, depth)

                    write_kernel_interface_read_var_mod(f, depth, [], 0, \
                        dtypelist=State.topblock['dtype'])

                    write_kernel_stmt(f, stmt, depth)

                    write_kernel_subroutines_read_var(f, depth, State.topblock['extern']['tkdpat'])
                    write_kernel_subroutine_module_externs(f, depth, State.topblock['extern'], State.topblock['stmt'].name)
                    write_kernel_subroutines_type_write_var(f, depth, State.topblock['dtype'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.topblock['dtype'])

                    continue

                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                if isinstance(stmt, Use):
                    process_kernel_module_use_stmt(f, stmt, depth)
                    #write_kernel_stmt(f, stmt, depth)
                elif isinstance(stmt, Access) or isinstance(stmt, Parameter):
                    process_kernel_module_spec_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)

                #write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_USE_PART:
                if isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_MODULE_CONTAINS_STMT

                    write_kernel_stmt(f, stmt, depth)

                    write_kernel_subroutines_read_var(f, depth, State.topblock['extern']['tkdpat'])
                    write_kernel_subroutine_module_externs(f, depth, State.topblock['extern'], State.topblock['stmt'].name)
                    write_kernel_subroutines_type_write_var(f, depth, State.topblock['dtype'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.topblock['dtype'])
                    continue
                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue

                # place action

                if isinstance(stmt, Use):
                    process_kernel_module_use_stmt(f, stmt, depth)
                elif isinstance(stmt, TypeDeclarationStatement):
                    process_kernel_module_typedecl_stmt(f, stmt, depth)
                elif isinstance(stmt, Access) or isinstance(stmt, Parameter):
                    process_kernel_module_spec_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)
                #write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_MODULE_CONTAINS_STMT:
                if stmt is subp_stmt:
                    mod_stage = SrcStage.AFTER_SUBP_STMT

                    items = [ n.firstpartname() for n in State.kernel_driver['input']['names']]+['kgen_unit']
                    if isinstance(stmt, Function):
                        write_kernel_stmt(f, stmt, depth, items=items, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth, items=items)

                    write_kernel_usepart_module(f, depth+1, State.parentblock)
                    continue

                elif stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                if isinstance(stmt, Use):
                    process_kernel_module_use_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)


            elif mod_stage==SrcStage.AFTER_SUBP_STMT:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT

                    #write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rdtsc', depth)
                    write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock', depth)
                    write(f, 'INTEGER, PARAMETER :: maxiter=%d'%Config.timing['repeat'], depth)
                    write(f, 'TYPE(check_t):: check_status', d=depth)
                    write(f, 'REAL(KIND=kgen_dp) :: tolerance', d=depth)

                    process_kernel_callsite_stmt(f, stmt, depth)
                    continue
                elif stmt.__class__ in  [ Use, Import ] + implicit_part:
                    # have this type of stmts wrtten before declarations
                    pass

                elif stmt.__class__ in  declaration_construct:
                    mod_stage = SrcStage.AFTER_SUBP_USE_PART
                    write(f, 'integer, intent(in) :: kgen_unit', d=depth)

                    #write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rdtsc', depth)
                    write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock', depth)
                    write(f, 'INTEGER, PARAMETER :: maxiter=%d'%Config.timing['repeat'], depth)
                    write(f, 'TYPE(check_t):: check_status', d=depth)
                    write(f, 'REAL(KIND=kgen_dp) :: tolerance', d=depth)

                    #if isinstance(stmt, TypeDeclarationStatement):
                    #    process_kernel_callsite_typedecl_stmt(f, stmt, depth)
                    #elif isinstance(stmt, Use):
                    #    process_kernel_module_use_stmt(f, stmt, depth)
                    #else:
                    #    write_kernel_stmt(f, stmt, depth)
                    #continue
                elif stmt.__class__ in execution_part:
                    mod_stage = SrcStage.AFTER_SUBP_SPEC_PART
                    write(f, 'integer, intent(in) :: kgen_unit', d=depth)

                    #write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rdtsc', depth)
                    write(f, 'INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock', depth)
                    write(f, 'INTEGER, PARAMETER :: maxiter=%d'%Config.timing['repeat'], depth)
                    write(f, 'TYPE(check_t):: check_status', d=depth)
                    write(f, 'REAL(KIND=kgen_dp) :: tolerance', d=depth)

                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    if isinstance(stmt, EndFunction):
                        write_kernel_stmt(f, stmt, depth, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                if isinstance(stmt, TypeDeclarationStatement):
                    process_kernel_callsite_typedecl_stmt(f, stmt, depth)
                elif isinstance(stmt, Use):
                    process_kernel_module_use_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_USE_PART:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT
                    process_kernel_callsite_stmt(f, stmt, depth)
                    continue
                elif stmt.__class__ in execution_part:
                    mod_stage = SrcStage.AFTER_SUBP_SPEC_PART
                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and len(State.parentblock['inout']['tkdpat'])>0:
                        write(f, 'CONTAINS', d=depth)
                    write_kernel_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_read_var(f, depth, State.parentblock['inout']['tkdpat'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_verify_var(f, depth, State.parentblock['output']['tkdpat'])
                    write_kernel_subroutines_perturb_var(f, depth)

                    if isinstance(stmt, EndFunction):
                        write_kernel_stmt(f, stmt, depth, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                if isinstance(stmt, TypeDeclarationStatement) and not isinstance(stmt.parent, Type):
                    process_kernel_callsite_typedecl_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_SPEC_PART:
                if stmt is State.callsite['stmt']:
                    mod_stage = SrcStage.AFTER_CALLSITE_STMT
                    process_kernel_callsite_stmt(f, stmt, depth)
                    continue
                elif isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and len(State.parentblock['inout']['tkdpat'])>0:
                        write(f, 'CONTAINS', d=depth)
                    write_kernel_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_read_var(f, depth, State.parentblock['inout']['tkdpat'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_verify_var(f, depth, State.parentblock['output']['tkdpat'])
                    write_kernel_subroutines_perturb_var(f, depth)

                    if isinstance(stmt, EndFunction):
                        write_kernel_stmt(f, stmt, depth, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_CALLSITE_STMT:
                if isinstance(stmt, Contains):
                    mod_stage = SrcStage.AFTER_SUBP_CONTAINS_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue
                elif stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and len(State.parentblock['inout']['tkdpat'])>0:
                        write(f, 'CONTAINS', d=depth)
                    write_kernel_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_read_var(f, depth, State.parentblock['inout']['tkdpat'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_verify_var(f, depth, State.parentblock['output']['tkdpat'])
                    write_kernel_subroutines_perturb_var(f, depth)
                    if isinstance(stmt, EndFunction):
                        write_kernel_stmt(f, stmt, depth, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_SUBP_CONTAINS_STMT:
                if stmt is endsubp_stmt:
                    mod_stage = SrcStage.AFTER_ENDSUBP_STMT

                    not_have_contains = not hasattr(subp_stmt, 'spec_stmts') or \
                        all( not isinstance(spec_stmt, Contains) for spec_stmt in subp_stmt.spec_stmts )
                    if not_have_contains and len(State.parentblock['inout']['tkdpat'])>0:
                        write(f, 'CONTAINS', d=depth)
                    write_kernel_subroutines_type_write_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_read_var(f, depth, State.parentblock['inout']['tkdpat'])
                    write_kernel_subroutines_type_verify_var(f, depth, State.parentblock['dtype'])
                    write_kernel_subroutines_verify_var(f, depth, State.parentblock['output']['tkdpat'])
                    write_kernel_subroutines_perturb_var(f, depth)

                    if isinstance(stmt, EndFunction):
                        write_kernel_stmt(f, stmt, depth, to_subr=True)
                    else:
                        write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_ENDSUBP_STMT:
                if stmt is endmod_stmt:
                    mod_stage = SrcStage.AFTER_ENDMODULE_STMT
                    write_kernel_stmt(f, stmt, depth)
                    continue

                # place action
                if isinstance(stmt, Use):
                    process_kernel_module_use_stmt(f, stmt, depth)
                else:
                    write_kernel_stmt(f, stmt, depth)

            elif mod_stage==SrcStage.AFTER_ENDMODULE_STMT:
                write_kernel_stmt(f, stmt, depth)

            else: raise ProgramException('Unknown module stage: %d'%mod_stage)

def process_driver_call_callsite_stmt(f, stmt, depth):
    args = ', '.join([ n.firstpartname() for n in State.kernel_driver['input']['names']])
    if len(args)>0:
        write(f, 'call %s(%s, kgen_unit)'%(State.parentblock['stmt'].name, args), d=depth)
    else:
        write(f, 'call %s(kgen_unit)'%State.parentblock['stmt'].name, d=depth)

def generate_kernel_module_driver():
    """ Generate kernel driver file with module type parent"""

    depth = 0
    filename = 'kernel_driver.f90'
    with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as f:
        write_file_header(f, filename)

        write(f, 'PROGRAM kernel_driver', d=depth)
        depth += 1
        write_driver_usepart_kgen(f, depth)
        write(f, 'IMPLICIT NONE', d=depth, pren=True)

        #non_dtype = []
        #for tkdpat in State.kernel_driver['input']['tkdpat']:
        #    if tkdpat[4] is None: non_dtype.append(tkdpat)
        #write_kernel_interface_read_var_mod(f, depth, non_dtype, -1)

        write_driver_specpart(f, depth)
        write_driver_read_fileopen(f, depth)

        if Config.mpi['enabled']: depth += 2
        else: depth += 1

        # read externs in modules
        write_driver_call_module_externs(f, depth)

        # read driver inputs
        write_kernel_driver_inputs(f, depth)

        # call callsite subp
        process_driver_call_callsite_stmt(f, State.parentblock['stmt'], depth)

        if Config.mpi['enabled']: depth -= 1

        write_driver_read_fileclose(f, depth)
        depth -= 1

        write(f, 'CONTAINS', d=depth)
        depth += 1

        write_kernel_subroutines_read_var(f, depth, State.kernel_driver['input']['tkdpat'])
        write_subroutines_kgen(f, depth)

        depth -= 1
        write(f, 'END PROGRAM kernel_driver', d=depth, pren=True)

def generate_kernel_utils():

    filename = 'kgen_utils.f90'
    with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as f:
        write_kgen_utils_file(f)
