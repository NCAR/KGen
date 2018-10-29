# kgentest.py

import os
import shutil
import re
import time
from kgutils import run_shcmd
from kext_sys_ch_homme_test import KExtSysCHHommeTest

job_script = \
"""#!/bin/bash

#PBS  -A NTDD0004 
#PBS  -N KHOMME 
#PBS  -r n 
#PBS  -j oe 
#PBS  -q premium 
#PBS  -V 
#PBS  -W block=true
#PBS  -S /bin/bash  
#PBS  -l walltime=0:20:00
#PBS  -l select=1:ncpus=36:mpiprocs=36

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

%s

# Pure MPI test 1
%s %s < %s
"""

namelist = \
"""&ctl_nl
horz_num_threads             = %(nth)s
partmethod                   = 4
topology                     = "cube"
test_case                    = "asp_baroclinic"
rotate_grid                  = 0
ne                           = %(nelem)s
qsize                        = 25
tstep_type                   = 5
ndays                        = 1
statefreq                    = 45
restartfreq                  = 43200
restartfile                  = "./R0001"
runtype                      = 0
tstep                        = %(tstep)s
rsplit                       = 3
qsplit                       = 1
psurf_vis                    = 0
integration                  = "explicit"
smooth                       = 0
nu                           = 5e16
nu_s                         = -1  ! use same value as nu
nu_q                         = 5e16
nu_p                         = 5e16
nu_div                       = -1
npdg=0
limiter_option               = 8
energy_fixer                 = -1
hypervis_order               = 2
hypervis_subcycle            = 4
u_perturb                    = 1
vert_remap_q_alg = 1
tracer_advection_formulation = 1
disable_diagnostics          = .true.
moisture = 'notdry'
/

&solver_nl
precon_method = "identity"
maxits        = 500
tol           = 1.e-9
/

&filter_nl
filter_type   = "taylor"
transfer_type = "bv"
filter_freq   = 0
filter_mu     = 0.04D0
p_bv          = 12.0D0
s_bv          = .666666666666666666D0
wght_fm       = 0.10D0
kcut_fm       = 2
/

&vert_nl
vform         = "ccm"
vfile_mid     = "vcoord/camm-26.fbin.littleendian"
vfile_int     = "vcoord/cami-26.fbin.littleendian"
/

&prof_inparm
profile_outpe_num   = 100
profile_single_file = .true.
/

&analysis_nl
output_prefix     = "perfTest-"
interp_gridtype   = 2
output_timeunits  = 1,1
output_frequency  = -1,-1
output_start_time = 0,0
output_end_time   = 30,30
output_varnames1  = 'zeta', 'u', 'v', 'ps', 'dp3d'
output_varnames2  = 'Q', 'Q2', 'Q3', 'Q4','phys_lat','phys_lon'
io_stride         = 8
output_type       = 'netcdf'
/
"""

class KExtSysCHHommeIntelTest(KExtSysCHHommeTest):

    def get_prerun_kernel_cmds(self):
        prerun_cmds = []
        prerun_cmds.append('module purge')
        prerun_cmds.append('module try-load ncarenv/1.2')
        prerun_cmds.append('module try-load ncarcompilers/0.4.1')
        prerun_cmds.append('module try-load intel/17.0.1')

        return prerun_cmds

    def get_prerun_cmds(self):
        prerun_cmds = self.get_prerun_kernel_cmds()
        prerun_cmds.append('module try-load impi/2017.1.132')
        prerun_cmds.append('module try-load netcdf/4.4.1.1')
        prerun_cmds.append('module try-load pnetcdf/1.10.0')
        prerun_cmds.append('module try-load cmake/3.9.1')

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
            'export LD_LIBRARY_PATH=$NETCDF/lib:/glade/u/apps/contrib/hdf5-mpi/1.8.12/intel/12.1.5/lib:$LD_LIBRARY_PATH', \
            'ulimit -s unlimited' ]
        result[myname]['prerun_kernel'] = self.get_prerun_kernel_cmds()
        result[myname]['mpirun'] = 'mpirun'
        result[myname]['job_script'] = job_script
        result[myname]['namelist'] = namelist

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
            cmake_cmd.append('-DHDF5_DIR:PATH=/glade/u/apps/contrib/hdf5-mpi/1.8.12/intel/12.1.5')
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

