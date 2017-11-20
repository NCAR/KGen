#!/bin/bash
#BSUB -n 64
#BSUB -R "span[ptile=16]"
#BSUB -P NTDD0004
#BSUB -J KGENMPAS
#BSUB -o coreJob.out
#BSUB -e coreJob.err
#BSUB -W 01:00
#BSUB -q premium

module unload intel
module load pgi/16.5
module unload netcdf
module load netcdf-mpi/4.3.3.1
module load pnetcdf/1.6.1

export PIO=/glade/u/home/youngsun/trepo/temp/kgensystest/mpas_ref/pio_pgi

cd ./run

mpirun.lsf ../core_atmosphere/atmosphere_model
