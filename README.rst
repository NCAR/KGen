KGEN: Fortran Kernel Generator
==============================

A package for extracting a part of Fortran source codes out of a large Fortran application.

:AUTHORS: Youngsung Kim, John Dennis, Raghu R. Kumar and Amogh Simha
:VERSION: 0.6.0
:COPYRIGHT: See the document entitled LICENSE.txt

Send questions and comments to KGEN Dev. Team (kgen@ucar.edu).

Changes from KGEN ver. 0.5.3
----------------------------

[User Interface]

* To invoke KGEN, user can use "kgen" command without having "python" in command line. The script is located in "bin" sub-directory
* "begin_callsite" and "end_callsite" KGEN directives are added to support a kernel extraction from a region of source codes.
* "comment" action in exclusion INI file is removed. And "remove" actions in the file is renamed to "remove_state"
* Two python scripts that automate the creation of inclusion INI file is added in "bin" sub-directory. One is for CESM(http://www2.cesm.ucar.edu/) log files and the other for MPAS(https://mpas-dev.github.io/) log files.
* "object" action in inclusion INI file is added to import external object files to kernel
* --verbose KGEN flag is added. 

* "--skip-intrinsic" and "--noskip-intrinsic" flags are discarded. Instead, please use "--intrinsic skip" and "--intrinsic noskip" each
* The syntax of numbers in "--invocation" flag changed. Please use colon instead of comma as a delimiter of numbers
* "--source" flag is added to inform KGEN with the Fortran source format.
* "common" section is renamed to "namepath" in exclusion INI file for "-e" flag
* The syntax of "namepath" is changed. Please see the section 2.3.1 of user's guide for details
* Several actions are added in "namepath" section of exclusion INI file for "-e" flag. Please see the section 2.3.3 of user's guide for details

[Major Improvements]

* Support for extracting a region of codes as a kernel.
* Better preserving the original formatting
* Better output information according to verbosity level.
* Support Fortran external subprograms
* fixed a bug on importing variables across multiple Fortran modules

[Release Notes]

* KGEN Github repository is moved to "https://github.com/NCAR/KGen.git"
* Kernels extracted using KGEN can be accessed from "https://github.com/NCAR/kernelOptimization.git"
* MPAS(https://github.com/MPAS-Dev/MPAS-Release.git) is being tested with this version of KGEN. As of this writing, two MPAS kernels are extracted and located in the above "kernelOptimization" github repo.

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


Instructions & Use
------------------

1. Download from KGen Github repository.
	>> git clone https://github.com/NCAR-CISL-ASAP/KGen.git

2. Read Kgen documentation in "doc" directory under the top Kgen directory.
	>> evince KGEN_Users_Guide_V0.0.0.pdf 

3. Try "simple" in "example/simple" directory
	>> cd example/simple;	# move to an example directory

	>> vi Makefile;			# Modify FC and FC_FLAGS if required

	>> vi src/Makefile;		# Modify FC and FC_FLAGS if required

	>> make;				# extract a kernel

	>> cd state;			# move to a directory for instrumentation files

	>> make;				# generate state data files

	>> cd ../kernel;		# move to a kernel directory

	>> make;				# build and run a kernel
