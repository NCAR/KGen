# kgentest.py
from __future__ import print_function

import os
import shutil
import glob
import getpass
from kext_func_test import KExtFuncTest
from kgutils import run_shcmd

class KExtFuncCHTest(KExtFuncTest):

    def mkworkdir(self, myname, result):
        if self.WORK_DIR is None:
            self.WORK_DIR = '/glade/scratch/%s'%getpass.getuser()

        systestdir = '%s/kgensystest'%self.WORK_DIR
        if not os.path.exists(systestdir):
            os.mkdir(systestdir)

        workdir = '%s/%s'%(systestdir, self.TEST_ID.replace('/', '_'))
        if not os.path.exists(workdir):
            os.mkdir(workdir)

        if os.path.exists('%s/kernel'%workdir):
            shutil.rmtree('%s/kernel'%workdir)

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

    def download(self, myname, result):
        srcdir = '%s/..'%self.TEST_DIR

        testfiles = [ os.path.join(srcdir,f) for f in os.listdir(srcdir) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(srcdir, f))]

        tmpsrc = result['mkdir_task']['tmpsrc']

        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, tmpsrc)
            dest_files.append(os.path.join(tmpsrc, os.path.basename(testfile)))

        result[myname]['srcfiles'] = dest_files

        self.set_status(result, myname, self.PASSED)
        return result

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['mkdir_task']['tmpsrc']
        FC = result['config_task']['FC']
        FC_FLAGS = result['config_task']['FC_FLAGS']
        PRERUN = result['config_task']['PRERUN']

        passed, out, err = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __cmd_clean='"cd %s; make -f Makefile clean"'%tmpsrc, \
            __cmd_build='"cd %s; make -f Makefile build"'%tmpsrc, \
            __cmd_run='"cd %s; make -f Makefile run"'%tmpsrc, \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(FC, FC_FLAGS), \
            __prerun='build="%s",run="%s"'%(PRERUN, PRERUN), \
            __outdir=workdir)

            #__invocation='0:0:0', \
            #__state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            #__state_run='cmds="cd %s; make run"'%tmpsrc, \

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob(workdir+"/kernel/add.*.*.*")
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, err)
        return result

#    def genstate(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        statefiles = result['generate_task']['statefiles']
#        FC = result['config_task']['FC']
#        FC_FLAGS = result['config_task']['FC_FLAGS']
#        PRERUN = result['config_task']['PRERUN']
#
#        out, err, retcode = run_shcmd('make FC="%s" FC_FLAGS="%s"'%(FC, FC_FLAGS), cwd='%s/state'%workdir)
#        result[myname]['stdout'] = out 
#        result[myname]['stderr'] = err
#
#        if retcode != 0:
#            print (out, err, retcode)
#            self.set_status(result, myname, self.FAILED, err)
#            return result
#
#        outfiles = []
#        for statefile in statefiles:
#            outfile = os.path.join('%s/kernel'%workdir, statefile)
#            if os.path.exists(outfile):
#                outfiles.append(outfile)
#
#        result[myname]['outfiles'] = outfiles
#
#        if len(outfiles)==len(statefiles):
#            self.set_status(result, myname, self.PASSED)
#        else:
#            self.set_status(result, myname, self.FAILED, errmsg=str(outfiles))
#        return result

    def rmdir(self, myname, result):
        workdir = result['mkdir_task']['workdir']

        if not self.LEAVE_TEMP:

            if os.path.exists(workdir):
                shutil.rmtree(workdir)

            if os.path.exists(os.path.join(self.TEST_DIR, 'kgen_cmds.sh')):
                os.remove(os.path.join(self.TEST_DIR, 'kgen_cmds.sh'))

            if os.path.exists(os.path.join(self.TEST_DIR, 'kgen.log')):
                os.remove(os.path.join(self.TEST_DIR, 'kgen.log'))

        self.set_status(result, myname, self.PASSED)

        return result
