#!/bin/bash -l
#SBATCH -p debug
#SBATCH -N 1
#SBATCH -t 00:05:00
#SBATCH -L SCRATCH     #note: specify license need for the file systems your job needs, such as SCRATCH,project
#SBATCH -J KGCALC

mpirun -n 4 ./demo.exe
