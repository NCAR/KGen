# kgen_genmake.py
import os
from kgen_utils import Config
from kgen_state import State

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
    kernel_driver_file = 'kernel_driver.f90'
    kgen_utils_file = 'kgen_utils.f90'
    callsite_file = State.topblock['path']

    #basenames
    callsite_base = os.path.basename(State.topblock['path'])
    dep_bases = [ os.path.basename(path) for path in State.srcfiles.keys() ]

    # all object files
    all_objs = [ obj(kernel_driver_file), obj(callsite_base), obj(kgen_utils_file) ] + \
        [ obj(dep_base) for dep_base in dep_bases ]

    # dependency
    kernel_driver_depends = [ obj(callsite_base), obj(kgen_utils_file) ] + [ obj(dep_base) for dep_base in dep_bases ]

    depends = {}
    for abspath, (srcfile, mods_used, units_used) in State.srcfiles.iteritems():
        dep = [ obj(kgen_utils_file) ] 
        for mod in mods_used:
            if mod.reader.id!=abspath and \
                not obj(os.path.basename(mod.reader.id)) in dep:
                dep.append(obj(os.path.basename(mod.reader.id)))
        for unit in units_used:
            if unit.item.reader.id!=abspath and \
                not obj(os.path.basename(unit.item.reader.id)) in dep:
                dep.append(obj(os.path.basename(unit.item.reader.id)))

        depends[os.path.basename(abspath)] = ' '.join(dep)

    # prerun commands
    pre_cmds = ''
    if Config.kernel_link['pre_cmds']:
        pre_cmds = ';'.join(Config.kernel_link['pre_cmds'])

    # link flags
    inc = [ '-L'+p for p in Config.kernel_link['include'] ]
    lib = [ '-l'+l for l in Config.kernel_link['lib'] ]
    link_flags = '%s %s'%(' '.join(inc), ' '.join(lib))

    with open('%s/Makefile'%(Config.path['kernel']), 'wb') as f:
        write(f, '# Makefile for KGEN-generated kernel')
        write(f, '')

        write(f, 'FC := %s'%Config.kernel_compile['FC'])
        write(f, 'FC_FLAGS := %s'%Config.kernel_compile['FC_FLAGS'])
        write(f, '')
        write(f, 'ALL_OBJS := %s' % ' '.join(all_objs))
        write(f, '')

        write(f, 'run: build')
        write(f, './kernel.exe', t=True)
        write(f, '')

        write(f, 'build: ${ALL_OBJS}')
        if pre_cmds:
            write(f, 'bash -i -c "%s; ${FC} ${FC_FLAGS} %s -o kernel.exe $^"'%(pre_cmds, link_flags), t=True)
        else:
            write(f, '${FC} ${FC_FLAGS} %s -o kernel.exe $^'%link_flags, t=True)
        write(f, '')

        write(f, '%s: kernel_driver.f90 %s' % (obj(kernel_driver_file), ' '.join(kernel_driver_depends)))
        write(f, '${FC} ${FC_FLAGS} -c -o $@ $<', t=True)
        write(f, '')

        for dep_base in dep_bases:
            write(f, '%s: %s %s' % (obj(dep_base), dep_base, depends[dep_base]))
            write(f, '${FC} ${FC_FLAGS} -c -o $@ $<', t=True)
            write(f, '')

        write(f, '%s: %s' % (obj(kgen_utils_file), kgen_utils_file))
        write(f, '${FC} ${FC_FLAGS} -c -o $@ $<', t=True)
        write(f, '')
           
        write(f, 'clean:')
        write(f, 'rm -f kernel.exe *.mod ${ALL_OBJS}', t=True)
    pass

def generate_state_makefile():

    org_files = [ filepath for filepath, (srcfile, mods_used, units_used) in State.srcfiles.iteritems() if srcfile.used4genstate ] + [ State.topblock['path'] ]
    with open('%s/Makefile'%(Config.path['state']), 'wb') as f:
        write(f, 'run: build')
        if Config.state_run['cmds']>0:
            write(f, Config.state_run['cmds'], t=True)
        else:
            write(f, 'echo "No information is provided to run. Please specify run commands using \'state-run\' command line option"; exit -1', t=True)
        write(f, '')

        write(f, 'build: %s'%Config.state_switch['type'])
        if Config.state_build['cmds']>0:
            write(f, Config.state_build['cmds'], t=True)
        else:
            write(f, 'echo "No information is provided to build. Please specify build commands using \'state-build\' command line option"; exit -1', t=True)
        for org_file in org_files:
            write(f, 'mv -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
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
