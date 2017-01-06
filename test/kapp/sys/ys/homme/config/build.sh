#!/bin/bash

rm -rf CMakeFiles CMakeCache.txt

. /usr/local/intel/2015_update1/bin/compilervars.sh intel64

export HOMME_ROOT=${PWD}

NETCDF_DIR=/ncar/asap/opt/netcdf-c/4.3.3.1/snb/intel/15.0.0/hdf5

cmake \
  -DCMAKE_BUILD_TYPE=RELEASE \
  -DCMAKE_SYSTEM_NAME=Linux \
  -DCMAKE_Fortran_COMPILER=mpif90 \
  -DCMAKE_C_COMPILER=mpicc \
  -DCMAKE_CXX_COMPILER=mpicxx \
  -DNETCDF_DIR=${NETCDF_DIR} \
  -DWITH_PNETCDF=FALSE \
  -DENABLE_PERFTEST=TRUE \
  -DHOMME_PROJID=STDD0002 \
  -DFORCE_Fortran_FLAGS="-assume byterecl -O3  -I./ -no-prec-sqrt -no-prec-div -qoverride-limits -qopenmp -xMIC-AVX512" \
  -DFORCE_C_FLAGS="-O3 -qopenmp -xMIC-AVX512 -g -traceback" \


