#!/bin/bash

    /glade/u/home/youngsun/repos/github/KGen/test/../bin/kgen \
    --timing repeat=1 \
    --outdir /glade/scratch/youngsun/kgensystest/kext_sys_ys_homme_gnu_edgevpack_Test \
    -i include.ini \
    --kernel-compile FC="gfortran",FC_FLAGS="-ffree-line-length-none -O3 -g -fopenmp" \
    --invocation 0:0-1:10,0:0-1:50,10:0-1:10,10:0-1:50 \
    -I /glade/scratch/youngsun/kgensystest/homme_work/src:/glade/scratch/youngsun/kgensystest/homme_work/src/share:/glade/scratch/youngsun/kgensystest/kext_sys_ys_homme_gnu_edgevpack_Test/bld/test_execs/perfTest \
    --openmp enable \
    -e exclude.ini \
    -D HAVE_CONFIG_H \
    --mpi enable \
    /glade/scratch/youngsun/kgensystest/homme_work/src/share/prim_advection_mod.F90:prim_advection_mod:euler_step:edgevpack