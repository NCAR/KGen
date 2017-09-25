
=====================
Simple Region Example
=====================

This example is to explain how to generate a kernel from a block of source codes.
This example can be found at $KGEN_HOME/examples/simple-region of KGen distribution.

-----------
Preparation
-----------

If you have not installed KGen, please see :doc:`Getting-started <../getting-started>`.

Following Linux utilities are required to use KGen.

::

    - make      : Software builder
    - cpp       : C preprocessor
    - strace    : System call tracer

--------------------
Files in the example
--------------------

Following tree explains the files in this example.

::

    $KGEN_HOME/examples/simple-region 
    |
    |-- Makefile            : contains KGen command to generate a KGen kernel
    |-- README              : an introduction of the example
    `-- src                 : contains target software that a kernel will be generated from
        |-- calc_mod.F90    : contains "calc" subroutine that is a target of kernel
        |-- Makefile        : contains commands to build/run the software
        |-- program.F90     : an entry of program
        `-- update_mod.F90  : contains a call-site to the kernel subroutine in calc_mod.F90

---------------------------------
KGen command to generate a kernel
---------------------------------

When user runs "make" command in top directory of the example, following KGen command is executed.

::

    $KGEN_HOME/bin/kgen \
        --cmd-clean "cd src; make clean" \
        --cmd-build "cd src; make build" \
        --cmd-run "cd src; make run" \
        src/update_mod.F90

**NOTE**: Several KGen options shown in the example of KGen distribution are omitted. The options are explained below.


First three options in the command let KGen know how to **"clean/build/run"** the target software. The commands in this options can be any Linux command including ones from other complex building system such as CMake or GNU Automake. **"clean"** command is to make sure that all the source files required for KGen analysis will be actually compiled. **"build"** command is to build the software so that KGen collect CPP macros and file paths automatically. **"run"** command is to run the software that is built with KGen-instrumented source files so that KGen can automatically generate state input/output data that are used for kernel execution and verification. Please see :doc:`User guide <../user-guide>` for more details.

The last line of the KGen command is a path to a source file that contains KGen directives. The KGen directives marks the region of source code for kernel generation. Following code shows how to use the directives.

::

    MODULE update_mod
        USE calc_mod, only : calc
        PUBLIC update
    CONTAINS
        SUBROUTINE update()
            INTEGER :: i, j
            real, dimension(ROW, COL) :: out2, out3, output
            DO i=1, COL
                DO j=1, ROW
                    !$kgen begin_callsite calc              ! start of region
                    CALL calc(i, j, output, out2, out3)     ! calling a subroutine of interest
                    !$kgen end_callsite                     ! end of region
                END DO
            END DO
            print *, 'SUM(output) = ', SUM(output)
        END SUBROUTINE
    END MODULE

The usage of KGen directives is similar to OpenMP directives. It starts with !$kgen followed by directive name and clauses. In this case, two directives, "begin_callsite" and "end_callsite", are used to mark a region as a callsite block. Please see :doc:`User guide <../user-guide>` for more details on specifying callsite with other methods.

----------------
Running a kernel
----------------

Once a kernel is generated successfully, a kernel can be executed immediately as following.

::

     >>> cd kernel
     >>> make

        ifort  -c -o kgen_utils.o kgen_utils.f90
        ifort  -c -o tprof_mod.o tprof_mod.f90
        ifort  -c -o calc_mod.o calc_mod.F90
        ifort  -c -o update_mod.o update_mod.F90
        ifort  -c -o kernel_driver.o kernel_driver.f90
        ifort    -o kernel.exe update_mod.o calc_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

        ./kernel.exe

     ***************** Verification against 'calc.0.0.1' *****************
     
     Number of output variables:            3
     Number of identical variables:            3
     Number of non-identical variables within tolerance:            0
     Number of non-identical variables out of tolerance:            0
     Tolerance:   1.000000000000000E-014
     
     Verification PASSED
     
     calc : Time per call (usec):   1.999999955296516E-002
     
     ***************** Verification against 'calc.0.0.2' *****************

     ...     
     
     ***************** Verification against 'calc.0.0.3' *****************
     
     ...     
     
    ****************************************************
        kernel execution summary: calc
    ****************************************************
        Total number of verification cases  :     3
        Number of verification-passed cases :     3
     
        Average call time (usec):  0.200E-01
        Minimum call time (usec):  0.200E-01
        Maximum call time (usec):  0.200E-01
    ****************************************************

KGen generates kernel source files and utility files in "kernel" subdirectory. "Makefile" in the directory helps user to build/run the kernel conviniently.
All KGen-generated kernels verify its output against state data generated from original software execution and provide verification results with detail information.

----------------------------------------
Using Representative Feature
----------------------------------------

From KGen Version 0.8.0, KGen supports three representative features.


Elapsedtime-based representative feature is one of them. The other two are: PAPI counter-based and source-code converage-based.

To turn on the feature, following KGen command-line flags is used.

::

    --repr-etime enable

With this option, KGen generate 20 (default) data files that can best represent the elapsed time characteristics of original application. "ndata" sub-flag can be used to change the default number of data files.

Similarly, user can use source code coverage representative feature as following:

::

    --repr-code enable

Once completed, KGen will create a new directory of "coverage" in output directory. In "coverage" directory, KGen creates source file(s) with "coverage" extension that contains "visit" information per each IF construct blocks.

Finally, PAPI-based representative feature is used as following:

::

    --repr-papi header=${PAPI_DIR}/include/f90papi.h,
       static=${PAPI_DIR}/lib/libpapi.a,event=PAPI_TOT_INS

PAPI_DIR is used to specify the location of papi library. You may try a papi counter other than PAPI_TOT_INS such as PAPI_TOT_CYC.
To use PAPI representative feature, you need to modify Makefile in src directory to build the example with PAPI library.


Once a kernel is generated successfully, a kernel can be executed immediately as following.

::

     >>> cd kernel
     >>> make papi

        ...

        ./kernel.exe

     ***************** Verification against 'calc.0.0.1' *****************
     
     Number of output variables:            3
     Number of identical variables:            3
     Number of non-identical variables within tolerance:            0
     Number of non-identical variables out of tolerance:            0
     Tolerance:   1.000000000000000E-014
     
     Verification PASSED
     
     calc : PAPI_TOT_INS per call:  311 
     
     ***************** Verification against 'calc.0.0.2' *****************

     ...     
     
     ***************** Verification against 'calc.0.0.3' *****************
     
     ...     
     
    ****************************************************
        kernel execution summary: calc
    ****************************************************
        Total number of verification cases  :     20
        Number of verification-passed cases :     20
     
        Average PAPI_TOT_INS:  308
        Minimum PAPI_TOT_INS:  305
        Maximum PAPI_TOT_INS:  311
    ****************************************************

-----------
What's next
-----------

:doc:`An exmaple for MPI application <simple-MPI>`
