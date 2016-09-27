#!/bin/bash

module unload intel
module load pgi/16.5
module unload netcdf
module load netcdf-mpi/4.3.3.1
module load pnetcdf/1.6.1

setenv CC mpicc
setenv FC mpif90
setenv F90 mpif90
setenv F77 mpif90
setenv CXX mpic++
setenv MPIF90 mpif90
setenv MPICC mpicc
setenv MPIF77 mpif77
setenv MPIFC mpif90
setenv MPICXX mpicxx

export PIO=/glade/u/home/youngsun/trepo/temp/kgensystest/mpas_ref/pio_pgi

cd ./core_atmosphere
make intel CORE=atmosphere
