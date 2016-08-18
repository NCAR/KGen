
=====================
Simple MPI Example
=====================

This example is to explain how to generate a kernel from a block of source codes in a MPI program.
This example can be found at $KGEN_HOME/examples/simple-MPI of KGen distribution.

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
        |-- job.lsf         : contains batch command to run the software using IBM Platform LSF
        |-- Makefile.lsf    : contains commands to build/run the software using IBM Platform LSF
        |-- Makefile.mpirun : contains commands to build/run the software on a local machine using mpirun
        |-- program.F90     : an entry of program
        `-- update_mod.F90  : contains a call-site to the kernel subroutine in calc_mod.F90

---------------------------------
KGen command to generate a kernel
---------------------------------

When user runs "make" command in top directory of the example, following KGen command is executed.

::

    $KGEN_HOME/bin/kgen \
        --cmd-clean "cd src; make -f Makefile.mpirun clean" \
        --cmd-build "cd src; make -f Makefile.mpirun build" \
        --cmd-run "cd src; make -f Makefile.mpirun run" \
        src/update_mod.F90:update_mod:update:calc

**NOTE**: Several KGen options shown in the example of KGen distribution are omitted. The options are explained below.


First three options in the command let KGen know how to **"clean/build/run"** the target software. The commands in this options can be any Linux command including ones from other complex building system such as CMake or GNU Automake. **"clean"** command is to make sure that all the source files required for KGen analysis will be actually compiled. **"build"** command is to build the software so that KGen collect CPP macros and file paths automatically. **"run"** command is to run the software that is built with KGen-instrumented source files so that KGen can automatically generate state input/output data that are used for kernel execution and verification. Please see :doc:`User guide <../user-guide>` for more details.

The last line of the KGen command is a path to a source file and KGen "namepath". KGen namepath is generally composed of module name, subprogram name and call target name. For example, in this example namepath to a callsite is "update_mod:update:calc". Please see :doc:`User guide <../user-guide>` for more details.

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
                    CALL calc(i, j, output, out2, out3)     ! calling a subroutine of interest
                END DO
            END DO
            print *, 'SUM(output) = ', SUM(output)
        END SUBROUTINE
    END MODULE


Other options in the example of KGen distribution are explained below.

::

    --timing repeat=100                  : repeats kernel execution 100 times to increase timing measurment resolution
    --invocation 0:0:1,0:0:3,1:0:1,1:0:3 : generates state data from first and third invocations of MPI rank 0 and 1
    --check tolerance=1.0D-14            : generates failed verification if kernel output is different more than 1.0D-14
    --rebuild all                        : prevents using cached information from previous kernel generation


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
         
        Number of output variables:            1
        Number of identical variables:            1
        Number of non-identical variables within tolerance:            0
        Number of non-identical variables out of tolerance:            0
        Tolerance:   1.000000000000000E-014

        Verification PASSED

        calc : Time per call (usec):   1.999999955296516E-002

        ***************** Verification against 'calc.0.0.3' *****************

        ...
         
        ***************** Verification against 'calc.1.0.1' *****************
         
        ...
         
        ***************** Verification against 'calc.1.0.3' *****************
         
        ...
         
        ****************************************************
            kernel execution summary: calc
        ****************************************************
            Total number of verification cases  :     4
            Number of verification-passed cases :     4
         
            Average call time (usec):  0.125E-01
            Minimum call time (usec):  0.100E-01
            Maximum call time (usec):  0.200E-01
        ****************************************************


KGen generates kernel source files and utility files in "kernel" subdirectory. "Makefile" in the directory helps user to build/run the kernel conviniently.
All KGen-generated kernels verify its output against state data generated from original software execution and provide verification results with detail information.

-----------
What's next
-----------

:doc:`User guide <../user-guide>`
