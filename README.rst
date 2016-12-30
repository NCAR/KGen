KGEN: Fortran Kernel Generator
==============================

A Python tool that extracts partial codes out of a large Fortran application and converts them into a standalone/verifiable/executable kernel 

:AUTHORS: Youngsung Kim and John Dennis
:VERSION: 0.7.2
:COPYRIGHT: See the document entitled LICENSE.txt

Send questions and comments to KGEN Dev. Team (kgen@ucar.edu).

Documentation
----------------------------
   https://ncar.github.io/kgendocs

Discussion-group
   https://groups.google.com/forum/#!forum/kgen-discuss

Changes from KGEN ver. 0.7.1
----------------------------

[ User Interface ]

* added "write" KGen directive to support manual state generation within source code
* added "exclude" KGen directive to support manual exclusion information within source code
* added "kernel-in-critical-region" openmp subflag to control usage of OpenMP critical region for kernel.

[ Major Improvements ]

* With using both of "write" and "exclude" directives, user can extract a kernel that contains MPI "receiving" library calls. 
* OpenMPI is supported
* Associate construct Fortran spec. is supported.


Overview
--------

* KGEN extracts an arbitrary region of Fortran source code as a stand-alone executable software
* In addition, it generates input & output data for executing and verifying the generated kernel
* All KGEN-generated kernels include correctness check and timing measurement


Dependencies
------------

* Linux OS
* Python (>=2.7 and < 3.0)
* C Pre-Processor(cpp)
* Make build tool(make)
* System call trace tool(strace)


Obtaining the Source Code
-------------------------

The latest KGEN can be obtained from the Git repository.

    git clone https://github.com/NCAR/KGen.git


Building & Installation
-----------------------

Current KGEN does not require to build or to install.


Instructions & Use
------------------

1. Download from KGen Github repository.
	>> git clone https://github.com/NCAR/KGen.git

2. Try a kernel generation example in "examples/simple" directory

	>> cd examples/simple;	# move to an example directory

	>> vi src/Makefile;			# Modify FC if required

	>> make;				# extract a kernel

	>> cd kernel;		# move to a kernel directory

	>> make;				# build and run a kernel
