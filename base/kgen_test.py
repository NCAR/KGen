# kgen_test.py

import os
import shutil
import subprocess

class KGenTest(object):
    taskid = 0

    def _gentask(self):
        return self.task

    def configure_test(self):
        pass

    def runcmd(self, cmd, input=None, **kwargs):
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, \
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, **kwargs)
        return proc.communicate(input=input)

    def add_test(self, testfunc):
        self.task['actions'] = [ ( testfunc, None, None ) ]

class KExtTest(KGenTest):
    def get_testfiles(self):
        testfiles = [ os.path.join(self.TEST_DIR,f) for f in os.listdir(self.TEST_DIR) if \
            f!=self.TEST_SCRIPT and f!='%sc'%self.TEST_SCRIPT and \
            not f.startswith('.') and os.path.isfile(os.path.join(self.TEST_DIR, f))] 

        return testfiles

    def copy_testfiles(self, testfiles, dest):
        dest_files = []
        for testfile in testfiles:
            shutil.copy2(testfile, dest)
            dest_files.append(os.path.join(dest, os.path.basename(testfile)))
        return dest_files

    def extract_kernel(self, target, namepath, *args, **kwargs):

        cmds = [ self.KGEN ]
        for kw, kwarg in kwargs.iteritems():
            flag = kw.replace('_', '-').replace('UNDERSCORE', '_')
            cmds.append('%s %s'%(flag, kwarg))
        cmds.append('%s:%s'%(target, namepath))

        out, err = self.runcmd(' '.join(cmds), cwd=self.TEST_DIR)
        if not out or out.find('ERROR')>0 or err:
            self.test['detail'] = err
            return None
        return out

    def generate_state(self, workdir, statefiles):
        out, err = self.runcmd('make', cwd='%s/state'%workdir)
        if err:
            self.test['detail'] = err
            return None

        outfiles = []
        for statefile in statefiles:
            outfile = os.path.join('%s/kernel'%workdir, statefile)
            if not os.path.exists(outfile):
                self.test['detail'] = '%s does not exist'%outfile
                return None
            outfiles.append(outfile)

        return outfiles

    def run_kernel(self, workdir):
        out, err = self.runcmd('make', cwd='%s/kernel'%workdir)
        if err:
            self.test['detail'] = err
            return None
        return out

    def verify_result(self, result):
        if not result or result.find('FAILED')>0 or result.find('PASSED')<0:
            self.test['detail'] = str(result)
            return False
        self.test['result'] = True

        return True

    def predefined_namepath1(self):
        if not os.path.exists(self.TEST_DIR): return

        tmpdir = '%s/tmp'%self.TEST_DIR
        if os.path.exists(tmpdir): shutil.rmtree(tmpdir)
        os.mkdir(tmpdir)

        tmpsrc = '%s/src'%tmpdir
        os.mkdir(tmpsrc)

        testfiles = self.get_testfiles()

        self.copy_testfiles(testfiles, tmpsrc)

        result = self.extract_kernel(os.path.join(tmpsrc, 'calling_module.F90'), \
            'calling_module:calling_subroutine:add', _D='ROW=4,COLUMN=4', _I=tmpsrc, \
            __invocation='1', \
            __state_build='cmds="cd %s; make clean; make build"'%tmpsrc, \
            __state_run='cmds="cd %s; make run"'%tmpsrc, \
            __kernel_compile='FC="%s",FC_FLAGS="%s"'%(self.COMPILER, self.COMPILER_FLAGS), \
            __outdir=tmpdir)
        if not result:
            return 

        statefiles = self.generate_state(tmpdir, [ 'add.1' ])
        if not statefiles:
            return

        result = self.run_kernel(tmpdir)
        if not result:
            return

        result = self.verify_result(result)
        if result:
            print '        ==> PASSED'
            if not self.LEAVE_TEMP and os.path.exists(tmpdir):
                shutil.rmtree(tmpdir)
        else:
            print '        ==> FAILED'
        return result

    def configure_predefined_test(self, testname):
        if testname=='namepath1':
            self.add_test(self.predefined_namepath1)
        else:
            print('ERROR: %s is not predefined testname.'%testname)

class KCoverTest(KGenTest):
    pass

