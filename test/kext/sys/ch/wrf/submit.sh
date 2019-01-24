#PBS  -N KINTWRF
#PBS  -r n 
#PBS  -j oe 
#PBS  -V 
#PBS  -W block=true
#PBS  -S /bin/bash  
#PBS  -l select=1:ncpus=36:mpiprocs=36
#PBS  -l walltime=00:30:00
#PBS  -A NTDD0004
#PBS  -q premium

cd WORKDIR

mpiexec_mpt dplace -s 1 ./wrf.exe
