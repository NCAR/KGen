#!/bin/bash

    module purge; \
    module try-load ncarenv/1.0; \
    module try-load ncarbinlibs/1.1; \
    module try-load ncarcompilers/1.0; \
    module try-load gnu/4.8.2; \
    module try-load netcdf/4.3.0; \
    module try-load pnetcdf/1.4.1; \
    module try-load cmake/2.8.10.2; \
    rm -rf CMakeFiles CMakeCache.txt; \
    cmake; \
    -DHOMME_PROJID="NTDD0004"; \
    -DENABLE_PERFTEST=TRUE; \
    -DENABLE_OPENMP=TRUE; \
    -DUSE_MPIEXEC="mpirun"; \
    -DCMAKE_C_COMPILER="mpcc"; \
    -DCMAKE_CXX_COMPILER="mpCC"; \
    -DCMAKE_Fortran_COMPILER="mpfort"; \
    -DNETCDF_DIR:PATH=$NETCDF; \
    -DPNETCDF_DIR:PATH=$PNETCDF; \
    -DHDF5_DIR:PATH=/glade/apps/opt/hdf5/1.8.12/gnu/4.8.2; \
    /glade/scratch/youngsun/kgensystest/homme_work &> config.log
