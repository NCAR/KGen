# kgentest.py

import os
import shutil
import re
import time
from kext_sys_ys_test import KExtSysYSTest

class KExtSysYSHommeTest(KExtSysYSTest):
    def get_prerun_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module try-load ncarenv/1.0')
        prerun_cmds.append('module try-load ncarbinlibs/1.1')
        prerun_cmds.append('module try-load ncarcompilers/1.0')
        prerun_cmds.append('module try-load intel/16.0.1')
        prerun_cmds.append('module try-load impi/5.0.1.035')
        prerun_cmds.append('module try-load netcdf/4.3.0')
        prerun_cmds.append('module try-load pnetcdf/1.4.1')
        prerun_cmds.append('module try-load cmake/2.8.10.2')

        return prerun_cmds

    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/homme_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if homme exists in appsrc dir
        out, err, retcode = self.run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            out, err, retcode = self.run_shcmd('svn checkout https://svn-homme-model.cgd.ucar.edu/trunk_tags/homme1_3_9/ .', cwd=appsrc)

        # copy homme src into test specific src dir
        tmpsrc = '%s/homme_work'%systestdir
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']

        blddir = '%s/bld'%workdir
        if not os.path.exists(blddir):
            os.mkdir(blddir)

        result[myname]['blddir'] = blddir

        datadir = '%s/data'%workdir

        if self.REBUILD or not os.path.exists(datadir) or len([name for name in os.listdir(datadir) if os.path.isfile(os.path.join(datadir, name))])==0:

            # prepare prerun command
            prerun_cmds = self.get_prerun_cmds()
            prerun_cmds.append('rm -rf CMakeFiles CMakeCache.txt')

            # prepare cmake command
            cmake_cmd = ['cmake']
            #cmake_cmd.append('-DHOMME_PROJID="STDD0002"')
            cmake_cmd.append('-DENABLE_PERFTEST=TRUE')
            cmake_cmd.append('-DENABLE_OPENMP=TRUE')
            cmake_cmd.append('--DUSE_MPIEXEC="mpirun"')
            cmake_cmd.append('-DCMAKE_C_COMPILER="mpiicc"')
            cmake_cmd.append('-DCMAKE_CXX_COMPILER="mpiicc"')
            cmake_cmd.append('-DCMAKE_Fortran_COMPILER="mpiifort"')
            cmake_cmd.append('-DNETCDF_DIR:PATH=$NETCDF')
            cmake_cmd.append('-DPNETCDF_DIR:PATH=$PNETCDF')
            cmake_cmd.append('-DHDF5_DIR:PATH=/glade/apps/opt/hdf5/1.8.12/intel/12.1.5')
            cmake_cmd.append(tmpsrc)

            out, err, retcode = self.run_shcmd('%s; %s'%('; '.join(prerun_cmds), ' '.join(cmake_cmd)), cwd=blddir)

            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg=err)
                return result

        # include.ini was created manually

        self.set_status(result, myname, self.PASSED)

        return result

    def build(self, myname, result):

        statefiles = result['generate_task']['statefiles']
        workdir = result['mkdir_task']['workdir']

        datadir = '%s/data'%workdir
        result[myname]['datadir'] = datadir

        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
            # prepare prerun command
            prerun_cmds = self.get_prerun_cmds()

            # build
            out, err, retcode = self.run_shcmd('%s; make clean; make -j 8 perfTest &> build.log'%'; '.join(prerun_cmds), cwd=blddir)
            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg='Homme build is failed.')
            else:
                self.set_status(result, myname, self.PASSED)
        else:
            # copy files from data to kernel directory
            for statefile in statefiles:
                shutil.copyfile(os.path.join(datadir, statefile), '%s/kernel/%s'%(workdir, statefile))

            result['goto'] = 'runkernel_task'
            self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        casedir = result['config_task']['casedir']
        casename = result['config_task']['casename']
        workdir = result['mkdir_task']['workdir']

        # may need to add -P BSUB directive in .run and .st_archive scripts

        # run homme 
        out, err, retcode = self.run_shcmd('./%s.submit'%casename, cwd=casedir)

        if retcode != 0 or not out:
            self.set_status(result, myname, self.FAILED, errmsg='Job submission is failed.')
            return result

        # find jobid
        jobid = None
        for iter in range(120):
            time.sleep(5)
            out, err, retcode = self.run_shcmd('bjobs')
            for line in out.split('\n'):
                items = line.split()
                if len(items)>6 and items[6].endswith('KGENCESM'):
                    jobid = items[0]
                    break
            if jobid: break

        if jobid is None:
            self.set_status(result, myname, self.FAILED, errmsg='Job id is not found.')
            return result

        status = ''
        maxiter = 3600
        iter = 0
        while status not in [ 'DONE', 'PSUSP', 'USUSP', 'SSUSP', 'EXIT', 'UNKWN', 'ZOMBI', 'FINISHED' ]:
            time.sleep(1)
            out, err, retcode = self.run_shcmd('bjobs %s'%jobid)
            if retcode==0:
                for line in out.split('\n'):
                    items = line.split()
                    if len(items)>3 and items[0]==jobid:
                        status = items[2]
                    elif len(items)>0 and items[-1]=='found':
                        status = 'FINISHED'
            else:
                print('DEBUG: ', out, err, retcode)

            iter += 1
            if iter>=maxiter:
                break

        if status=='DONE' or 'FINISHED':
            self.set_status(result, myname, self.PASSED)
        else:
            self.set_status(result, myname, self.FAILED, errmsg='Job completion status is not expected.')

        return result
