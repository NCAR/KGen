import os
import shutil
from cover_func_test import CoverFuncTest

class CoverFuncYSTest(CoverFuncTest):
    def mkworkdir(self, myname, result):
        workdir = '%s/tmp'%self.TEST_DIR
        if os.path.exists(workdir): shutil.rmtree(workdir)
        os.mkdir(workdir)

        tmpsrc = '%s/src'%workdir
        os.mkdir(tmpsrc)

        result[myname]['workdir'] = workdir
        result[myname]['tmpsrc'] = tmpsrc

        result[myname]['status'] = self.PASSED
        return result

    def download(self, myname, result):
        testfiles = [ os.path.join(self.TEST_DIR,f) for f in os.listdir(self.TEST_DIR) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(self.TEST_DIR, f))]

        tmpsrc = result['mkdir_task']['tmpsrc']

        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, tmpsrc)
            dest_files.append(os.path.join(tmpsrc, os.path.basename(testfile)))

        result[myname]['srcfiles'] = dest_files

        result[myname]['status'] = self.PASSED
        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']
        mpiinc = '/ncar/opt/intel/12.1.0.233/impi/4.0.3.008/intel64/include'

        passed, out, err = self.run_coverage(os.path.join(tmpsrc, 'update_mod.F90'), \
            'update_mod:update:calc', _D='ROW=4,COLUMN=4', _I='%s:%s'%(tmpsrc, mpiinc), \
            __invocation='1:3', \
            __mpi='ranks=0:1', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(self.COMPILER, self.COMPILER_FLAGS), \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['coverfile'] = 'calc.cover'
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['coverfile'] = None
            self.set_status(result, myname, self.FAILED, out)
        return result
