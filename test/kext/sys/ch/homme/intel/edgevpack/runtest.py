# runtest.py
# 
import os
import sys
import glob
import shutil
from kext_sys_ch_homme_intel_test import KExtSysCHHommeIntelTest

here = os.path.dirname(__file__)

#job_script = \
#"""#!/bin/bash
#
##PBS  -A NTDD0004 
##PBS  -N KHOMME 
##PBS  -r n 
##PBS  -j oe 
##PBS  -q regular 
##PBS  -V 
##PBS  -W block=true
##PBS  -S /bin/bash  
##PBS  -l walltime=0:20:00
##PBS  -l select=1:ncpus=36:mpiprocs=36
#
#export TMPDIR=/glade/scratch/$USER/temp
#mkdir -p $TMPDIR
#
#%s
#
## Pure MPI test 1
#%s %s < %s
#"""
#
#namelist = \
#"""&ctl_nl
#horz_num_threads             = %(nth)s
#partmethod                   = 4
#topology                     = "cube"
#test_case                    = "asp_baroclinic"
#rotate_grid                  = 0
#ne                           = %(nelem)s
#qsize                        = 25
#tstep_type                   = 5
#ndays                        = 1
#statefreq                    = 45
#restartfreq                  = 43200
#restartfile                  = "./R0001"
#runtype                      = 0
#tstep                        = %(tstep)s
#rsplit                       = 3
#qsplit                       = 1
#psurf_vis                    = 0
#integration                  = "explicit"
#smooth                       = 0
#nu                           = 5e16
#nu_s                         = -1  ! use same value as nu
#nu_q                         = 5e16
#nu_p                         = 5e16
#nu_div                       = -1
#npdg=0
#limiter_option               = 8
#energy_fixer                 = -1
#hypervis_order               = 2
#hypervis_subcycle            = 4
#u_perturb                    = 1
#vert_remap_q_alg = 1
#tracer_advection_formulation = 1
#disable_diagnostics          = .true.
#moisture = 'notdry'
#/
#
#&solver_nl
#precon_method = "identity"
#maxits        = 500
#tol           = 1.e-9
#/
#
#&filter_nl
#filter_type   = "taylor"
#transfer_type = "bv"
#filter_freq   = 0
#filter_mu     = 0.04D0
#p_bv          = 12.0D0
#s_bv          = .666666666666666666D0
#wght_fm       = 0.10D0
#kcut_fm       = 2
#/
#
#&vert_nl
#vform         = "ccm"
#vfile_mid     = "vcoord/camm-26.fbin.littleendian"
#vfile_int     = "vcoord/cami-26.fbin.littleendian"
#/
#
#&prof_inparm
#profile_outpe_num   = 100
#profile_single_file = .true.
#/
#
#&analysis_nl
#output_prefix     = "perfTest-"
#interp_gridtype   = 2
#output_timeunits  = 1,1
#output_frequency  = -1,-1
#output_start_time = 0,0
#output_end_time   = 30,30
#output_varnames1  = 'zeta', 'u', 'v', 'ps', 'dp3d'
#output_varnames2  = 'Q', 'Q2', 'Q3', 'Q4','phys_lat','phys_lon'
#io_stride         = 8
#output_type       = 'netcdf'
#/
#"""

class Test(KExtSysCHHommeIntelTest):

    def generate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        blddir = result['config_task']['blddir']
        prerun_cmds = result['config_task']['prerun_run'] 
        mpirun = result['config_task']['mpirun'] 
        job_script = result['config_task']['job_script']
        namelist = result['config_task']['namelist']

        result[myname]['datadir'] = '%s/data'%workdir

        rundir = '%s/run'%workdir
        if os.path.exists(rundir):
            shutil.rmtree(rundir)
        os.mkdir(rundir)
        os.mkdir('%s/movies'%rundir)
        result[myname]['rundir'] = rundir
        
        # prepare namelist
        params = {'nelem': '6', 'nth': '2', 'nath': '2', 'tstep': '360'}
        if os.path.exists('%s/homme.nl'%rundir): os.system('rm -f %s/homme.nl'%rundir)
        with open('%s/homme.nl'%rundir, 'w') as fd:
            fd.write(namelist%params)

        # create symbolic linke to input data
        if os.path.exists('%s/vcoord'%rundir): os.system('unlink %s/vcoord'%rundir)
        os.system('ln -s %s/test/vcoord %s/vcoord'%(tmpsrc, rundir))

        os.system('rm -f %s/homme.*.err'%rundir)
        os.system('rm -f %s/homme.*.out'%rundir)

        # create job submit script
        with open('%s/homme.submit'%rundir, 'w') as fd:
            fd.write(job_script%('\n'.join(prerun_cmds), mpirun, '%s/test_execs/perfTest/perfTest'%blddir, '%s/homme.nl'%rundir))


        srcfile = '%s/src/share/prim_advection_mod.F90'%tmpsrc
        namepath = 'prim_advection_mod:euler_step:edgevpack'
        fc = 'ifort'
        #fc_flags = '-assume byterecl -fp-model precise -ftz -O3 -g -openmp'
        fc_flags = '-assume byterecl -fp-model precise -ftz -O3 -g'
        prerun_cmds = ';'.join(result['config_task']['prerun_kernel'])
        passed, out, err = self.extract_kernel(srcfile, namepath, workdir, \
            _i='%s/include.dat'%here, \
            _e='%s/exclude.dat'%here, \
            _I='%s/src:%s/src/share:%s/test_execs/perfTest'%(tmpsrc, tmpsrc, blddir), \
            _D='HAVE_CONFIG_H', \
            __cmd_clean='"cd %s; make clean"'%blddir, \
            __cmd_build='"cd %s; make -j 4 perfTest"'%blddir, \
            __cmd_run='"cd %s; qsub homme.submit"'%rundir, \
            __mpi='enable', \
            __openmp='enable', \
            __kernel_option='FC="%s",FC_FLAGS="%s"'%(fc, fc_flags), \
            __prerun='build="%s",run="%s"'%(prerun_cmds, prerun_cmds), \
            __outdir=workdir)
#
#            __kernel_option='FC="ifort",FC_FLAGS="%s"'%fc_flags, \
#            __prerun='build="module load mkl",run="module load mkl"', \
#            __check='tolerance="1.0D-10"', \
#            __outdir=workdir)

        result[myname]['stdout'] = out
        result[myname]['stderr'] = err

        if passed:
            result[myname]['statefiles'] = glob.glob('edgevpack.*.*.*')
            self.set_status(result, myname, self.PASSED)
        else:
            result[myname]['statefiles'] = []
            self.set_status(result, myname, self.FAILED, 'STDOUT: %s\nSTDERR: %s'%(out, err))

        return result

#    def replace(self, myname, result):
#
#        workdir = result['mkdir_task']['workdir']
#        tmpsrc = result['download_task']['tmpsrc']
#
#        for instrumented in glob.glob('%s/state/*.F90'%workdir):
#            fname = os.path.basename(instrumented)
#            if not os.path.exists('%s/src/share/%s.kgen'%(tmpsrc, fname)): 
#                shutil.copy2('%s/src/share/%s'%(tmpsrc, fname), '%s/src/share/%s.kgen'%(tmpsrc, fname))
#            os.remove('%s/src/share/%s'%(tmpsrc, fname))
#            shutil.copy2(instrumented, '%s/src/share'%tmpsrc)
# 
#        self.set_status(result, myname, self.PASSED)
#
#        return result
#
if __name__ == "__main__":
    # we may allow to run this test individually
    print('Please do not run this script from command line. Instead, run this script through KGen Test Suite .')
    print('Usage: cd ${KGEN_HOME}/test; ./kgentest.py')
    sys.exit(-1)
