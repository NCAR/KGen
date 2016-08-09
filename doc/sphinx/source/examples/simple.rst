..  -*- coding: utf-8 -*-

=================
"Simple" Example
=================
  
This example shows how to generate a kernel from a simple Fortran program.
In this example, we are interested in generating a kernel of "calc" subroutine in "calc_mod.F90".

-------------
Prerequisites
-------------

KGen makes use of following Linux commands.

    - make
    - cpp
    - strace

-------------
Source files
-------------

Three source files and one Makefile are in "src" sub-directory.

::

    - program.F90       : an entry of program.
    - update_mod.F90    : a module that contains a call statement to a kernel block
    - calc_mod.F90      : a module that contains a kernel block, "calc" subroutine
    - Makefile          : having targets of clean/build/run for this program

"program.F90" is a mere place holder that makes a call to a subroutine in "update_mod.F90."

"update_mod.F90"
================

In this file, there is a callsite line to a kernel block as shown below.
User needs to specify the location of the callsite using KGen command-line.

.. code:: fortran

    MODULE update_mod
        USE calc_mod, only : calc
    CONTAINS
        SUBROUTINE update()
            ...
            CALL calc(i, j, output, out2, out3) ! callsite
            ...
        END SUBROUTINE
    END MODULE

"calc_mod.F90"
==============

This is a Fortran module that contains "calc" subroutine, which is our target of kernel extraction.

.. code:: fortran

    MODULE calc_mod
        PUBLIC calc
    CONTAINS
        SUBROUTINE calc(i, j, output, out2, out3)
            ...
        END SUBROUTINE
    END MODULE

-----------------
KGen command line
-----------------

In Makefile at "examples/simple" directory, 
