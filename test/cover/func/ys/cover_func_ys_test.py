import os
import getpass
import shutil
from cover_func_test import CoverFuncTest

class CoverFuncYSTest(CoverFuncTest):
    def mkworkdir(self, myname, result):
        if self.WORK_DIR is None:
            self.WORK_DIR = '/glade/scratch/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

        if os.path.exists('%s/state'%workdir):
            shutil.rmtree('%s/state'%workdir)

        tmpsrc = '%s/src'%workdir
        if os.path.exists(tmpsrc):
            shutil.rmtree(tmpsrc)
        os.mkdir(tmpsrc)

        result[myname]['sysdir'] = systestdir
        result[myname]['workdir'] = workdir
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result


#        workdir = '%s/tmp'%self.TEST_DIR
#        if os.path.exists(workdir): shutil.rmtree(workdir)
#        os.mkdir(workdir)
#
#        tmpsrc = '%s/src'%workdir
#        os.mkdir(tmpsrc)
#
#        result[myname]['workdir'] = workdir
#        result[myname]['tmpsrc'] = tmpsrc
#
#        result[myname]['status'] = self.PASSED
#        return result

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
            __mpi='ranks=all', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['coverfiles'] = [ 'invocations.0', 'invocations.1', 'invocations.2', 'invocations.3' ]
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['coverfiles'] = None
            self.set_status(result, myname, self.FAILED, out)
        return result

    def genstate(self, myname, result):

        tmpsrc = result['mkdir_task']['tmpsrc']
        workdir = result['mkdir_task']['workdir']
        statefiles = result['generate_task']['coverfiles']

        out, err, retcode = self.run_shcmd('make', cwd='%s/state'%workdir)
        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if err:
            self.set_status(result, myname, self.FAILED, err)
            return result

        outfiles = []
        for statefile in statefiles:
            outfile = os.path.join(tmpsrc, statefile)
            if os.path.exists(outfile):
                outfiles.append(outfile)

        result[myname]['outfiles'] = outfiles

        if len(outfiles)==len(statefiles):
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, errmsg=str(outfiles))
        return result

    def verify(self, myname, result):
        tmpsrc = result['mkdir_task']['tmpsrc']
        coverfiles = result['generate_task']['coverfiles']

        if any(not os.path.exists(os.path.join(tmpsrc, cfile)) for cfile in coverfiles):
            self.set_status(result, myname, self.FAILED, errmsg='Coverfiles could not be found.')
            return result


        self.set_status(result, myname, self.PASSED)
        return result

    def rmdir(self, myname, result):

        workdir = result['mkdir_task']['workdir']

        if os.path.exists(os.path.join(self.TEST_DIR, 'kgen.log')):
            os.remove(os.path.join(self.TEST_DIR, 'kgen.log'))

        if not self.LEAVE_TEMP:
            if os.path.exists(workdir):
                shutil.rmtree(workdir)

        self.set_status(result, myname, self.PASSED)

        return result
