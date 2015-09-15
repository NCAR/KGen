KGEN: Fortran Kernel Generator
==============================

A package for extracting a part of Fortran source codes out of a large Fortran application.

:AUTHORS: Youngsung Kim, John Dennis, Raghu R. Kumar and Amogh Simha
:VERSION: 0.5.0
:COPYRIGHT: See the document entitled LICENSE.txt

Send questions and comments to KGEN Dev. Team (kgen@ucar.edu).


Overview
--------

* KGEN extracts a Fortran subprogram as a stand-alone software out of a large software application
* In addition, it generates instrumented files that save input & output data for the generated kernel
* Correctness check and timing measurement are included in the generated kernel


Dependencies
------------

* Python (>=2.6.6) and Python standard library
* Fortran Pre-Processor(fpp) or C Pre-Processor(cpp)


Obtaining the Source Code
-------------------------

The latest KGEN can be obtained from the Git repository.

    git clone https://github.com/NCAR-CISL-ASAP/KGen.git


Building & Installation
-----------------------

Current KGEN does not require to build or to install.

Before Use
----------

T.B.D.

Instructions & Use
------------------

1. Download from KGen Github repository.
	>> git clone https://github.com/NCAR-CISL-ASAP/KGen.git

2. Read Kgen documentation in "doc" directory under the top Kgen directory.
	>> evince KGEN_Users_Guide_V0.5.0.pdf 

3. Try "simple" in "example/simple" directory
	>> cd example/simple;	# move to an example directory
	>> vi Makefile;			# Modify FC and FC_FLAGS if required
	>> vi src/Makefile;		# Modify FC and FC_FLAGS if required
	>> make;				# extract a kernel
	>> cd state;			# move to a directory for instrumentation files
	>> make;				# generate state data files
	>> cd ../kernel;		# move to a kernel directory
	>> make;				# build and run a kernel
