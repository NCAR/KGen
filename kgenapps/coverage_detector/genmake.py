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

def generate_makefile():

    org_files = [ filepath for filepath, (srcfile, mods_used, units_used) in State.used_srcfiles.iteritems() ] 
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
