
import sys
import os
import glob
import shutil

CURDIR = os.path.dirname(os.path.realpath(__file__))
ROOTDIR = '%s/../../../'%CURDIR
SRCDIR = '%s/tests/resource/Fortran_program/calc'%ROOTDIR
CALLSITE = 'update_mod.F90:update_mod:update:calc'

KGEN_SRC = '%s/kgen'%ROOTDIR
KGEN_APP = '%s/bin/kgen'%ROOTDIR
sys.path.insert(0, KGEN_SRC)

from kgutils import run_shcmd
from kgconfig import Config

def test_strace():

    inc = Config.find_machine()

    relpath = os.path.relpath(CURDIR, start=ROOTDIR)
    outdir = '%s/%s'%(os.path.expandvars(inc.get('variable', 'work_directory')), \
        '%s_%s'%(relpath.replace('/', '_'), os.path.basename(__file__)[:-3]))
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    os.makedirs(outdir)
    for filename in glob.glob(os.path.join(SRCDIR, '*')):
        shutil.copy(filename, outdir)

    # create a kgen command
    cmds = []
    cmds.append(KGEN_APP)
    if inc.has_section('compiler') and inc.has_option('compiler', 'intel') and inc.get('compiler', 'intel'):
        cmds.extend(['--prerun', 'build="%(cmd)s",run="%(cmd)s",kernel_build="%(cmd)s",kernel_run="%(cmd)s"'%\
            {'cmd': inc.get('compiler', 'intel')}])
    cmds.append('--cmd-clean "cd %s; make -f Makefile.mpirun clean"'%outdir)
    cmds.append('--cmd-build "cd %s; make -f Makefile.mpirun build"'%outdir)
    cmds.append('--cmd-run "cd %s; make -f Makefile.mpirun run"'%outdir)
    cmds.append('--kernel-option FC=ifort')
    cmds.append('--outdir %s'%outdir)
    cmds.append('--mpi enable')
    cmds.append('--openmp enable')
    cmds.append('-I %s'%outdir)
    cmds.append('%s/%s'%(outdir, CALLSITE))
    
    # run kgen
    shcmds = ' '.join(cmds)
    print 'SHCMD: %s'%shcmds
    out, err, retcode = run_shcmd(' '.join(cmds))

    print '\n******* STDOUT KGEN **********\n'
    print out
    print '\n******* STDERR KGEN **********\n'
    print err

    assert retcode == 0

    out, err, retcode = run_shcmd('cd %s/kernel; make'%outdir)

    print '\n******* STDOUT KERNEL **********\n'
    print out
    print '\n******* STDERR KERNEL **********\n'
    print err

    # check output
    if retcode == 0:
        outlines = out.split('\n')
        if any( line.find('Verification FAILED') >= 0 for line in outlines ):
            assert False
        if not any( line.find('Verification PASSED') >= 0 for line in outlines ):
            assert False

        assert True
    else:
        assert False
