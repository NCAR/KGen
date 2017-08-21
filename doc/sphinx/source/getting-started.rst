===============
Getting-started
===============


--------
Download
--------

.. Source and binary releases: http://pypi.python.org/pypi/kgen/

The latest KGen version can be downloaded from KGen github repository.

::

    git clone git://github.com/kgen/kgen.git

.. Github (latest development): https://github.com/kgen/kgen/

----------
Installing
----------

Current version does not need installation step. User can run KGen as following

::

    $KGEN/bin/kgen [options] <path to source file>

------------
Requirements
------------

- Linux OS
- Python 2.7 or later but less than 3.0
- Make building tool(make)
- C Preprocessor(cpp)
- Stream editor(sed)
- System Call Tracer(strace)

----------------------------------
First Kernel-generation using KGen
----------------------------------

KGen distribution comes with several examples. Following shows how to run one of examples.

Current version runs only on Linux.

::

    >>> cd $KGEN/examples/simple    # Move to an example
    >>> vi src/Makefile             # Modify FC if required
    >>> make                        # Generate a kernel
    >>> cd kernel                   # Move to a kernel directory
    >>> make                        # Run a kernel

First make command acutally runs a KGen command with several options and an argument. Please see an :doc:`example <examples/simple>` section for details.

