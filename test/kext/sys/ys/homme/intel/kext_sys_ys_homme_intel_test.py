# kgentest.py

import os
import shutil
import re
import time
from kext_sys_ys_homme_test import KExtSysYSHommeTest

class KExtSysYSHommeIntelTest(KExtSysYSHommeTest):

    def get_prerun_kernel_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module try-load ncarenv/1.0')
        prerun_cmds.append('module try-load ncarbinlibs/1.1')
        prerun_cmds.append('module try-load ncarcompilers/1.0')
        prerun_cmds.append('module try-load intel/16.0.1')

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = self.get_prerun_kernel_cmds()
        prerun_cmds.append('module try-load impi/5.0.1.035')
        prerun_cmds.append('module try-load netcdf/4.3.0')
        prerun_cmds.append('module try-load pnetcdf/1.4.1')
        prerun_cmds.append('module try-load cmake/2.8.10.2')

        return prerun_cmds

    def config(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        systestdir = result['mkdir_task']['sysdir']
        tmpsrc = result['download_task']['tmpsrc']

        blddir = '%s/bld'%workdir
        if not os.path.exists(blddir):
            os.mkdir(blddir)

        result[myname]['blddir'] = blddir
        result[myname]['prerun_config'] = self.get_prerun_cmds() + ['rm -rf CMakeFiles CMakeCache.txt']
        result[myname]['prerun_build'] = self.get_prerun_cmds()
        result[myname]['prerun_run'] = self.get_prerun_cmds() + ['export OMP_NUM_THREADS=2', \
            'export LD_LIBRARY_PATH=$NETCDF/lib:/glade/apps/opt/hdf5/1.8.12/intel/12.1.5/lib:$LD_LIBRARY_PATH', \
            'ulimit -s unlimited' ]
        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()
        result[myname]['mpirun'] = 'mpirun'

        if self.REBUILD or not os.path.exists(blddir) or len([name for name in os.listdir(blddir) if os.path.isfile(os.path.join(blddir, name))])==0:

            # prepare prerun command
            prerun_cmds = result[myname]['prerun_config']

            # prepare cmake command
            cmake_cmd = ['cmake']
            cmake_cmd.append('-DHOMME_PROJID="NTDD0004"')
            cmake_cmd.append('-DENABLE_PERFTEST=TRUE')
            cmake_cmd.append('-DENABLE_OPENMP=TRUE')
            cmake_cmd.append('-DUSE_MPIEXEC="mpirun"')
            cmake_cmd.append('-DCMAKE_C_COMPILER="mpiicc"')
            cmake_cmd.append('-DCMAKE_CXX_COMPILER="mpiicc"')
            cmake_cmd.append('-DCMAKE_Fortran_COMPILER="mpiifort"')
            cmake_cmd.append('-DNETCDF_DIR:PATH=$NETCDF')
            cmake_cmd.append('-DPNETCDF_DIR:PATH=$PNETCDF')
            cmake_cmd.append('-DHDF5_DIR:PATH=/glade/apps/opt/hdf5/1.8.12/intel/12.1.5')
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

            out, err, retcode = self.run_shcmd('%s; %s'%('; '.join(prerun_cmds), ' '.join(cmake_cmd)), cwd=blddir)

            if retcode != 0:
                self.set_status(result, myname, self.FAILED, errmsg=err)
                return result

        # include.ini was created manually

        self.set_status(result, myname, self.PASSED)

        return result

