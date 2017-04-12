'''Basic KGen Extractor Unit test cases
'''
import sys
import os
import shutil
import glob
import pytest

CURDIR = os.path.dirname(os.path.realpath(__file__))
ROOTDIR = '%s/../../../'%CURDIR
SRCDIR = '%s/tests/resource/Fortran_program/modules'%ROOTDIR
CALLSITE = 'calling_module.F90:calling_module:calling_subroutine:add'

KGEN_APPLICATION = '%s/kgen'%ROOTDIR
sys.path.insert(0, KGEN_APPLICATION)

from kgutils import run_shcmd
from kgconfig import Config
from kggenfile import init_plugins, KERNEL_ID_0
from parser.main import Parser
from extractor.main import Extractor

@pytest.yield_fixture(scope="module")
def extractor():

    inc = Config.find_machine()

    relpath = os.path.relpath(CURDIR, start=ROOTDIR) 
    outdir = '%s/%s'%(os.path.expandvars(inc.get('variable', 'work_directory')), \
        '%s_%s'%(relpath.replace('/', '_'), os.path.basename(__file__)[:-3]))
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    for filename in glob.glob(os.path.join(SRCDIR, '*')):
        shutil.copy(filename, outdir)

    args = []
    if inc.has_section('compiler') and inc.has_option('compiler', 'gnu') and inc.get('compiler', 'gnu'):
        args.extend(['--prerun', 'build="%(cmd)s",run="%(cmd)s",kernel_build="%(cmd)s",kernel_run="%(cmd)s"'%\
            {'cmd': inc.get('compiler', 'gnu')}])
    args.extend(['--cmd-clean', 'cd %s; make clean'%outdir])
    args.extend(['--cmd-build', 'cd %s; make build'%outdir])
    args.extend(['--cmd-run', 'cd %s; make run'%outdir])
    args.extend(['--invocation', '0:0:0'])
    args.extend(['--kernel-option', 'FC=gfortran'])
    args.extend(['--outdir', outdir])
    args.extend(['-I', outdir])
    args.extend(['%s/%s'%(outdir, CALLSITE)])
    Config.parse(args)

    Config.process_include_option()
    Config.collect_mpi_params()


    parser = Parser()
    parser.run()

    init_plugins([KERNEL_ID_0])

    ext = Extractor()

    yield ext

    #shutil.rmtree(outdir) 

def test_run(extractor):
    extractor.run()    

    # run kernel
    out, err, retcode = run_shcmd('make', cwd='%s/%s'%(Config.path['outdir'], Config.path['kernel']) )

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

del sys.path[0]
