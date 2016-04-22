#!/bin/bash

    /glade/u/home/youngsun/repos/github/KGen/test/../bin/kgen \
    --timing repeat=1 \
    --outdir /glade/scratch/youngsun/kgensystest/kext_sys_ys_cesm_intel_adv_clubb_core_Test \
    --kernel-compile FC="ifort",FC_FLAGS="-no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2 -mkl",PRERUN="module purge;module try-load ncarenv/1.0;module try-load ncarbinlibs/1.1;module try-load ncarcompilers/1.0;module try-load intel/16.0.1" \
    --invocation "100:0-1:10,100:0-1:50,300:0-1:10,300:0-1:50,500:0-1:10,500:0-1:50" \
    --source format=free,strict=no,alias=/glade/scratch/youngsun:/glade/u/home/youngsun/trepo/temp \
    -i include.ini \
    --openmp enable \
    --mpi comm=mpicom,use="spmd_utils:mpicom" \
    /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90:clubb_intr:clubb_tend_cam:advance_clubb_core