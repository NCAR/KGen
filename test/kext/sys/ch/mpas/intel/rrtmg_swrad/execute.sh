#!/bin/bash -l
#PBS -A NTDD0004
#PBS -l walltime=00:30:00
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -N KINTMPAS
#PBS -o kintmpas
#PBS -e kintmpas
#PBS -q premium

ulimit -s unlimited

mpiexec_mpt dplace -s 1  EXEC
