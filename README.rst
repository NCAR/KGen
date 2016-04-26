KGEN: Fortran Kernel Generator
==============================

A package for extracting a part of Fortran source codes out of a large Fortran application.

:AUTHORS: Youngsung Kim and John Dennis
:VERSION: 0.6.3
:COPYRIGHT: See the document entitled LICENSE.txt

Send questions and comments to KGEN Dev. Team (kgen@ucar.edu).

Changes from KGEN ver. 0.6.2
----------------------------

[User Interface]

* Python version > 2.7 and < 3.0 is required
* Invocation KGEN option is changed to support MPI rank, OpenMP number and invocations more precisely.
* enable sub-option for --mpi KGEN flag should be used for kernel extraction from MPI application
* enable sub-option for --openmp KGEN flag should be used for kernel extraction from MPI application

[Major Improvements]

* Added support for class type declaration type in state generation
* Added unit tests for GNU and PGI compilers
* Supported detail control for state generation from MPI ranks and OpenMP threads
* Supported Statement Function Statement
* Fixed a but for Sumcheck having NaN entities
* Splitted codebase to  KGen core codes and KGen applications
( Refactored to support plugins framework for source-to-source translation on Abstract Syntax Tree

Overview
--------

* KGEN extracts a Fortran subprogram as a stand-alone software out of a large software application
* In addition, it generates instrumented files that save input & output data for the generated kernel
* Correctness check and timing measurement are included in the generated kernel


Dependencies
------------

* Python (>=2.7 and < 3.0) and Python standard library
* Fortran Pre-Processor(fpp) or C Pre-Processor(cpp)


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

2. Read Kgen documentation in "doc" directory under the top Kgen directory.
	>> evince KGEN_Users_Guide_V0.6.3.pdf 

3. Try a kernel generation example in "example/simple" directory

	>> cd example/simple;	# move to an example directory

	>> vi Makefile;			# Modify FC if required

	>> make;				# extract a kernel

	>> cd state;			# move to a directory for instrumentation files

	>> make;				# generate state data files

	>> cd ../kernel;		# move to a kernel directory

	>> make;				# build and run a kernel
