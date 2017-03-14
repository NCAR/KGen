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
from extractor.main import Extractor


@pytest.yield_fixture(scope="module")
def extractor():
    ext = Extractor()
    outdir = tempfile.mkdtemp()
    for filename in glob.glob(os.path.join(SRCDIR, '*.*')):
        shutil.copy(filename, outdir)
    args = []
    args.extend(['--cmd-clean', '"cd %s; make clean"'%outdir])
    args.extend(['--cmd-build', '"cd %s; make build"'%outdir])
    args.extend(['--cmd-run', '"cd %s; make run"'%outdir])
    args.extend(['--invocation', '0:0:0'])
    args.extend(['--outdir', outdir])
    args.extend(['%s/%s'%(outdir, CALLSITE)])
    Config.parse(args)
    Config.kernel['name'] = 'unittest'
    yield ext
    shutil.rmtree(outdir) 

def test_run(extractor):
    print 'AAA', Config.path['outdir']
    extractor.run()    

del sys.path[0]
