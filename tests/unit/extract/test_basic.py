'''Basic KGen Extractor Unit test cases
'''
import sys
import os
import shutil
import glob
import tempfile
import pytest

CURDIR = os.path.dirname(os.path.realpath(__file__))
SRCDIR = '%s/../../resource/simple_program'%CURDIR
CALLSITE = 'calling_module.F90:calling_module:calling_subroutine:add'

KGEN_APPLICATION = '%s/../../../kgen'%CURDIR
sys.path.insert(0, KGEN_APPLICATION)

from kgconfig import Config
from kggenfile import init_plugins, KERNEL_ID_0
from parser.main import Parser
from extractor.main import Extractor

@pytest.yield_fixture(scope="module")
def extractor():
    outdir = tempfile.mkdtemp()
    for filename in glob.glob(os.path.join(SRCDIR, '*.*')):
        shutil.copy(filename, outdir)
    args = []
    args.extend(['--cmd-clean', '"cd %s; make clean"'%outdir])
    args.extend(['--cmd-build', '"cd %s; make build"'%outdir])
    args.extend(['--cmd-run', '"cd %s; make run"'%outdir])
    args.extend(['--kernel-option', 'FC=gfortran,FC_FLAGS=-O2'])
    args.extend(['--invocation', '0:0:0'])
    args.extend(['--outdir', outdir])
    args.extend(['-I', outdir])
    args.extend(['%s/%s'%(outdir, CALLSITE)])
    Config.parse(args)
    Config.kernel['name'] = 'unittest'
    Config.process_include_option()
    Config.collect_mpi_params()

    parser = Parser()
    parser.run()

    init_plugins([KERNEL_ID_0])

    ext = Extractor()

    yield ext

    shutil.rmtree(outdir) 

def test_run(extractor):
    extractor.run()    
    import pdb; pdb.set_trace()

del sys.path[0]
