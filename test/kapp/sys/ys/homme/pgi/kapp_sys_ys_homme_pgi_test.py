# kgentest.py

import os
import shutil
import re
import time
from kapp_sys_ys_homme_test import KAppSysYSHommeTest
from kgen_utils import run_shcmd

job_script = \
"""#!/bin/bash

#BSUB -a poe
#BSUB -P STDD0002
#BSUB -q premium
#BSUB -W 0:20
#BSUB -x
#BSUB -J KHOMME
#BSUB -e homme.%%J.err
#BSUB -o homme.%%J.out
#BSUB -n %s
#BSUB -R "span[ptile=%s]" 

%s

# Pure MPI test 1
%s %s < %s
"""

class KAppSysYSHommePgiTest(KAppSysYSHommeTest):

    def get_prerun_kernel_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module try-load ncarenv/1.0')
        prerun_cmds.append('module try-load ncarbinlibs/1.1')
        prerun_cmds.append('module try-load ncarcompilers/1.0')
        prerun_cmds.append('module try-load pgi/15.10')

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = self.get_prerun_kernel_cmds()
        #prerun_cmds.append('module try-load impi/5.0.1.035')
        prerun_cmds.append('module try-load netcdf/4.3.0')
        prerun_cmds.append('module try-load pnetcdf/1.4.1')
        prerun_cmds.append('module try-load cmake/3.0.2')

        return prerun_cmds

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']

        blddir = '%s/bld'%workdir
        if not os.path.exists(blddir):
            os.mkdir(blddir)
        rundir = '%s/run'%workdir
        if not os.path.exists(rundir):
            os.mkdir(rundir)

        result[myname]['blddir'] = blddir
        result[myname]['rundir'] = rundir

        # setup
        if not os.path.exists('%s/movies'%rundir):
            os.mkdir('%s/movies'%rundir)
        if os.path.exists('%s/vcoord'%rundir):
            os.system('rm -f %s/vcoord'%rundir)
        #os.system('ln -s %s/test/vcoord %s/vcoord'%(tmpsrc, rundir))
        os.system('ln -s %s/../../config/perfTestWACCM_64_1_4/vcoord %s/vcoord'%(self.TEST_DIR, rundir))

        # create namelist
        if os.path.exists('%s/perfTestWACCM.nl'%rundir):
            os.remove('%s/perfTestWACCM.nl'%rundir)
        #shutil.copy('%s/test/reg_test/namelists/perfTestWACCM.nl'%tmpsrc, rundir)
        shutil.copy('%s/../../config/perfTestWACCM_64_1_4/perfTestWACCM-ne8.nl'%self.TEST_DIR, rundir)
        #if os.path.exists('%s/camBench.nl'%rundir):
        #    os.remove('%s/camBench.nl'%rundir)
        #shutil.copy('%s/test/perftest/camBench.nl'%tmpsrc, rundir)

        # copy exclude.ini
        if os.path.exists('%s/exclude.ini'%workdir):
            os.remove('%s/exclude.ini'%workdir)
        shutil.copy('%s/exclude.ini'%self.TEST_DIR, workdir)


        result[myname]['prerun_config'] = self.get_prerun_cmds() + ['rm -rf CMakeFiles CMakeCache.txt']
        result[myname]['prerun_build'] = self.get_prerun_cmds()
        result[myname]['prerun_run'] = self.get_prerun_cmds() + ['export OMP_NUM_THREADS=2', \
            'ulimit -s unlimited', 'export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH}' ]
        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()
        result[myname]['mpirun'] = 'mpirun.lsf'
            #'export LD_LIBRARY_PATH=$NETCDF/lib:/glade/apps/opt/hdf5/1.8.12/intel/12.1.5/lib:$LD_LIBRARY_PATH', 

        # create job submit script
        with open('%s/homme.submit'%rundir, 'w') as fd:
            #fd.write(job_script%('16', '16', '\n'.join(result[myname]['prerun_run']), result[myname]['mpirun'], '%s/test_execs/perfTest/perfTest'%blddir, '%s/camBench.nl'%rundir))
            fd.write(job_script%('16', '16', '\n'.join(result[myname]['prerun_run']), result[myname]['mpirun'], '%s/test_execs/perfTestWACCM/perfTestWACCM'%blddir, '%s/perfTestWACCM-ne8.nl'%rundir))

        if self.REBUILD or not os.path.exists(blddir) or len([name for name in os.listdir(blddir) if os.path.isfile(os.path.join(blddir, name))])==0:

            # prepare prerun command
            prerun_cmds = result[myname]['prerun_config']

            # prepare cmake command
            cmake_cmd = ['cmake']
            cmake_cmd.append('-DHOMME_PROJID="STDD0002"')
            cmake_cmd.append('-DENABLE_PERFTEST=TRUE')
            cmake_cmd.append('-DENABLE_OPENMP=TRUE')
            cmake_cmd.append('-DUSE_MPIEXEC="mpirun.lsf"')
            cmake_cmd.append('-DCMAKE_C_COMPILER="/glade/apps/opt/modulefiles/ys/cmpwrappers/mpipcc"')
            cmake_cmd.append('-DCMAKE_Fortran_COMPILER="/glade/apps/opt/modulefiles/ys/cmpwrappers/mpipf90"')
            cmake_cmd.append('-DNETCDF_DIR:PATH=$NETCDF')
            cmake_cmd.append('-DPNETCDF_DIR:PATH=$PNETCDF')
            cmake_cmd.append('-DHDF5_DIR:PATH=/glade/apps/opt/hdf5/1.8.12/pgi/13.3')
            cmake_cmd.append('-DSZIP_DIR:PATH=/glade/apps/opt/szip/2.1/intel/12.1.5')
            cmake_cmd.append(tmpsrc)

            if self.LEAVE_TEMP:
                with open('%s/config_cmds.sh'%blddir, 'w') as f:
                    f.write('#!/bin/bash\n')
                    f.write('\n')
                    for cmd in prerun_cmds:
                        f.write('    %s\n'%cmd)
                    for cmd in cmake_cmd[:-1]:
                        f.write('    %s \\\n'%cmd)
                    f.write('    %s &> config.log'%cmake_cmd[-1])
                os.chmod('%s/config_cmds.sh'%blddir, 0755)

            out, err, retcode = run_shcmd('%s; %s'%('; '.join(prerun_cmds), ' '.join(cmake_cmd)), cwd=blddir)

            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg=err)
                return result

        # include.ini was created manually

        self.set_status(result, myname, self.PASSED)

        return result

