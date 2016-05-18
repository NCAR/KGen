# kgen_genmake.py
import os
from kgen_utils import Config
from kgen_state import State
from collections import OrderedDict

def write(f, line, n=True, t=False):
    nl = ''
    tab = ''
    if n: nl = '\n'
    if t: tab = '\t'
    f.write(tab + line + nl)

def obj(file):
    l = file.split('.')
    if len(l)>1:
        l[-1] = 'o'
    return '.'.join(l)

def generate_makefiles():
    # Makefile for kernel
    generate_kernel_makefile()

    # Makefile for state
    generate_state_makefile()

    State.state = State.MAKEFILES_GENERATED

def generate_kernel_makefile():
    #NOTE: for gfortran, use -ffixed-line-length-none and -ffree-line-length-none

    # source files
    kgen_utils_file = 'kgen_utils.f90'
    kernel_driver_file = 'kernel_driver.f90'
    callsite_file = State.topblock['stmt'].reader.id

    #basenames
    callsite_base = os.path.basename(callsite_file)
    dep_base_srcfiles = [ os.path.basename(filepath) for filepath, srclist in State.used_srcfiles.iteritems() ]
    dep_bases = dep_base_srcfiles + [ kernel_driver_file ]

    # all object files
    all_objs_srcfiles = [ obj(dep_base_srcfile) for dep_base_srcfile in dep_base_srcfiles ]
    all_objs = all_objs_srcfiles + [ obj(kernel_driver_file), obj(kgen_utils_file) ]

    # dependency
    depends = {}

    # dependency for kernel_driver.f90
    depends[kernel_driver_file] = ' '.join(all_objs_srcfiles + [obj(kgen_utils_file) ])

    # dependency for other files
    for abspath, (srcfile, mods_used, units_used) in State.used_srcfiles.iteritems():
        dep = [ obj(kgen_utils_file) ] 
        for mod in mods_used:
            if mod.reader.id!=abspath and \
                not obj(os.path.basename(mod.reader.id)) in dep:
                dep.append(obj(os.path.basename(mod.reader.id)))
        for unit in units_used:
            if unit.item.reader.id!=abspath and \
                not obj(os.path.basename(unit.item.reader.id)) in dep:
                dep.append(obj(os.path.basename(unit.item.reader.id)))

        basename = os.path.basename(abspath)
        if basename==callsite_base:
            dobjs = all_objs[:]
            dobjs.remove(obj(callsite_base))
            dobjs.remove(obj(kernel_driver_file))
            depends[basename] = ' '.join(dobjs)
        else:
            depends[basename] = ' '.join(dep)

    # prerun commands
    #pre_cmds = ''
    #if Config.kernel_link['pre_cmds']:
    #    pre_cmds = ';'.join(Config.kernel_link['pre_cmds'])

    # link flags and objects
    link_flags = ''
    objects = ''
    if Config.include.has_key('import'):
        for path, import_type in Config.include['import'].iteritems():
            if import_type.startswith('library'):
                inc = '-L'+path
                pos1 = import_type.find('(')
                pos2 = import_type.find(')')
                lib = '-l'+import_type[(pos1+1):pos2].strip()
                link_flags += ' %s %s'%(inc, lib)
            elif import_type=='object':
                objects += ' %s'%os.path.basename(path)

    # find fc flag sets
    compilers = OrderedDict()
    compiler_options = OrderedDict()
    for path, kfile in Config.include['file'].items():

        base = os.path.basename(path)
        if base not in dep_bases: continue

        if kfile.has_key('compiler'):
            if kfile['compiler'] in compilers:
                if base not in compilers[kfile['compiler']]:
                    compilers[kfile['compiler']].append(base)
            else:
                compilers[kfile['compiler']] = [ base, kernel_driver_file ]
        if kfile.has_key('compiler_options'):
            if kfile['compiler_options'] in compiler_options:
                if base not in compiler_options[kfile['compiler_options']]:
                    compiler_options[kfile['compiler_options']].append(base)
            else:
                compiler_options[kfile['compiler_options']] = [ base, kernel_driver_file ]

    with open('%s/Makefile'%(Config.path['kernel']), 'wb') as f:
        write(f, '# Makefile for KGEN-generated kernel')
        write(f, '')

        write(f, 'FC := %s'%Config.kernel_compile['FC'])
        for i, compiler in enumerate(compilers):
            write(f, 'FC_SET_%d := %s'%(i, compiler))

        write(f, 'FC_FLAGS := %s'%Config.kernel_compile['FC_FLAGS'])
        for i, options in enumerate(compiler_options):
            write(f, 'FC_FLAGS_SET_%d := %s'%(i, options))

        write(f, 'PRERUN := %s'%Config.kernel_compile['PRERUN'])

        write(f, '')
        write(f, 'ALL_OBJS := %s'%' '.join(all_objs))
        write(f, '')

        write(f, 'run: build')
        if Config.add_mpi_frame['enabled']:
            write(f, '${PRERUN}; %s -np %s ./kernel.exe'%(Config.add_mpi_frame['mpiexec'], Config.add_mpi_frame['np']), t=True)
        else:
            write(f, '${PRERUN}; ./kernel.exe', t=True)
        write(f, '')

        write(f, 'build: ${ALL_OBJS}')

        fc_str = 'FC'
        fc_flags_str = 'FC_FLAGS'
        if len(compilers)>0: fc_str += '_SET_0'
        if len(compiler_options)>0: fc_flags_str += '_SET_0'

        write(f, '${PRERUN}; ${%s} ${%s} %s %s -o kernel.exe $^'%(fc_str, fc_flags_str, link_flags, objects), t=True)
        write(f, '')

        for dep_base in dep_bases:
            write(f, '%s: %s %s' % (obj(dep_base), dep_base, depends[dep_base]))

            dfc_str = 'FC'
            dfc_flags_str = 'FC_FLAGS'
            for i, (compiler, files) in enumerate(compilers.items()):
                if dep_base in files:
                    dfc_str += '_SET_%d'%i
            for i, (compiler_option, files) in enumerate(compiler_options.items()):
                if dep_base in files:
                    dfc_flags_str += '_SET_%d'%i

            write(f, '${PRERUN}; ${%s} ${%s} -c -o $@ $<'%(dfc_str, dfc_flags_str), t=True)
            write(f, '')

        write(f, '%s: %s' % (obj(kgen_utils_file), kgen_utils_file))
        write(f, '${PRERUN}; ${%s} ${%s} -c -o $@ $<'%(fc_str, fc_flags_str), t=True)
        write(f, '')
           
        write(f, 'clean:')
        write(f, 'rm -f kernel.exe *.mod ${ALL_OBJS}', t=True)
    pass

def generate_state_makefile():

    org_files = [ filepath for filepath, (srcfile, mods_used, units_used) in State.used_srcfiles.iteritems() if srcfile.tree.used4genstate ] 
    if not State.topblock['stmt'].reader.id in org_files:
        org_files.append(State.topblock['path'])

    with open('%s/Makefile'%(Config.path['state']), 'wb') as f:
        if Config.state_run['cmds']>0:
            write(f, 'run: build')
            write(f, Config.state_run['cmds'], t=True)
        else:
            write(f, 'echo "No information is provided to run. Please specify run commands using \'state-run\' command line option"; exit -1', t=True)
        write(f, '')

        if Config.state_build['cmds']>0:
            write(f, 'build: %s'%Config.state_switch['type'])
            write(f, Config.state_build['cmds'], t=True)
            for org_file in org_files:
                write(f, 'mv -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
        else:
            write(f, 'echo "No information is provided to build. Please specify build commands using \'state-build\' command line option"; exit -1', t=True)
        write(f, '')

        write(f, '%s: save'%Config.state_switch['type'])
        if Config.state_switch['type']=='replace':
            for org_file in org_files:
                basename = os.path.basename(org_file)
                write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
        elif Config.state_switch['type']=='copy':
            if Config.state_switch['cmds']>0:
                write(f, Config.state_switch['cmds'], t=True)
            else:
                write(f, 'echo "No information is provided to copy files. Please specify switch commands using \'state-switch\' command line option"; exit -1', t=True)
        write(f, '')

        write(f, 'recover:')
        for org_file in org_files:
            write(f, 'cp -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
        write(f, '')

        write(f, 'recover_from_locals:')
        for org_file in org_files:
            write(f, 'cp -f %s.kgen_org %s'%(os.path.basename(org_file), org_file), t=True)
        write(f, '')


        write(f, 'save:')
        for org_file in org_files:
            write(f, 'if [ ! -f %(f)s.kgen_org ]; then cp -f %(f)s %(f)s.kgen_org; fi'%{'f':org_file}, t=True)
            write(f, 'if [ ! -f %(g)s.kgen_org ]; then cp -f %(f)s %(g)s.kgen_org; fi'%{'f':org_file, 'g':os.path.basename(org_file)}, t=True)
        write(f, '')

        write(f, '#clean:')
        write(f, '#rm -f kernel.exe *.mod *.o', t=True)
