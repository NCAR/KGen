# kgentest.py

import os
import shutil
import re
import time
from kext_sys_ys_test import KExtSysYSTest

job_script = \
"""#!/bin/bash

#BSUB -a poe
#BSUB -P NTDD0004 
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

namelist = \
"""&ctl_nl
NThreads                     = %(nth)s
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

class KExtSysYSHommeTest(KExtSysYSTest):

    def download(self, myname, result):

        systestdir = result['mkdir_task']['sysdir']
        workdir = result['mkdir_task']['workdir']

        appsrc = '%s/homme_ref'%systestdir
        if not os.path.exists(appsrc):
            os.mkdir(appsrc)

        # check if homme exists in appsrc dir
        out, err, retcode = self.run_shcmd('svn info | grep URL', cwd=appsrc)
        if retcode != 0 or not out or len(out)<3 or not out.startswith('URL'):
            out, err, retcode = self.run_shcmd('svn checkout -r 4971 https://svn-homme-model.cgd.ucar.edu/trunk/ .', cwd=appsrc)

        # copy homme src into test specific src dir
        tmpsrc = '%s/homme_work'%systestdir
#        if os.path.exists(tmpsrc):
#            shutil.rmtree(tmpsrc)
#        shutil.copytree(appsrc, tmpsrc)
        if not os.path.exists(tmpsrc):
            shutil.copytree(appsrc, tmpsrc)
        else:
            for fname in os.listdir('%s/src'%tmpsrc):
                if fname.endswith('.kgen'):
                    shutil.copyfile(os.path.join('%s/src'%tmpsrc, fname), os.path.join('%s/src'%tmpsrc, fname[:-5]))
            for fname in os.listdir('%s/src/share'%tmpsrc):
                if fname.endswith('.kgen'):
                    shutil.copyfile(os.path.join('%s/src/share'%tmpsrc, fname), os.path.join('%s/src/share'%tmpsrc, fname[:-5]))

        result[myname]['appsrc'] = appsrc
        result[myname]['tmpsrc'] = tmpsrc

        self.set_status(result, myname, self.PASSED)

        return result

    def build(self, myname, result):

        statefiles = result['generate_task']['statefiles']
        workdir = result['mkdir_task']['workdir']
        blddir = result['config_task']['blddir'] 
        prerun_cmds = result['config_task']['prerun_build'] 

        datadir = '%s/data'%workdir
        result[myname]['datadir'] = datadir

        if self.REBUILD or not os.path.exists(datadir) or any(not os.path.exists('%s/%s'%(datadir, sf)) for sf in statefiles):
            if self.LEAVE_TEMP:
                with open('%s/build_cmds.sh'%blddir, 'w') as f:
                    f.write('#!/bin/bash\n')
                    f.write('\n')
                    for cmd in prerun_cmds:
                        f.write('    %s\n'%cmd)
                    f.write('    make clean\n')
                    f.write('    make -j 4 perfTest &> build.log')
                os.chmod('%s/build_cmds.sh'%blddir, 0755)

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

            if os.path.exists(os.path.join(datadir, 'kgen_statefile.lst')):
                shutil.copyfile(os.path.join(datadir, 'kgen_statefile.lst'), '%s/kernel/kgen_statefile.lst'%workdir)

            result['goto'] = 'runkernel_task'
            self.set_status(result, myname, self.PASSED)

        return result

    def genstate(self, myname, result):

        workdir = result['mkdir_task']['workdir']
        tmpsrc = result['download_task']['tmpsrc']
        blddir = result['config_task']['blddir']
        prerun_cmds = result['config_task']['prerun_run'] 
        mpirun = result['config_task']['mpirun'] 

        rundir = '%s/run'%workdir
        if os.path.exists(rundir):
            shutil.rmtree(rundir)
        os.mkdir(rundir)
        os.mkdir('%s/movies'%rundir)
        result[myname]['rundir'] = rundir
        
        # may need to add -P BSUB directive in .run and .st_archive scripts

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
            fd.write(job_script%('16', '16', '\n'.join(prerun_cmds), mpirun, '%s/test_execs/perfTest/perfTest'%blddir, '%s/homme.nl'%rundir))


        # submit and wait to finish
        out, err, retcode = self.run_shcmd('bsub < homme.submit', cwd=rundir)

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
                if len(items)>6 and items[6].endswith('KHOMME'):
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
