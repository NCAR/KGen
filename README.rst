KGEN: Fortran Kernel Generator
==============================

A package for extracting a part of Fortran source codes out of a large Fortran application.

:AUTHORS: Youngsung Kim, John Dennis, Raghu R. Kumar and Amogh Simha
:VERSION: 0.4.13
:COPYRIGHT: See the document entitled LICENSE.txt

Send questions and comments to KGEN Dev. Team (kgen@ucar.edu).


Overview
--------

* KGEN extracts a Fortran subprogram as a stand-alone software out of a large software application
* In addition, it generates instrumented files that save input & output data for the generated kernel
* Correctness check and timing measurement are included in the generated kernel


Dependencies
------------

This section should list, with version numbers, all of the
dependencies of the project package.  These dependencies may
be Python-based tools themselves, or other libraries that this
package uses.

These dependencies should, ideally, be listed for clarity:

* Python #1 (>=2.6.6)


Obtaining the Source Code
-------------------------

The latest KGEN can be obtained from the Git repository.

    git clone https://github.com/NCAR-CISL-ASAP/KGen.git


Building & Installation
-----------------------

..
This section details how to build and install your project.  If
your project is pure-Python, then the build and install will be
simple.
..
After obtaining the source (see above), simply install with
distutils.  On unix, this involves::
..
    $  cd PyTemplate
    $  python setup.py install [--prefix=/path/to/install/location]
..
The prefix is optional, as the default prefix is typically `/usr/local` on
linux machines.  However, you must have permissions to write to the prefix
location, so you may want to choose a prefix location where you have write
permissions.  Like most distutils installations, you can alternatively
install the PyReshaper with the `--user` option, which will automatically
select (and create if it does not exist) the `$HOME/.local` directory in which
to install.  To do this, type (on unix machines)::
..
    $  python setup.py install --user
..
This can be handy since the site-packages directory will be common for all
user installs, and therefore only needs to be added to the `PYTHONPATH` once.
..
To install the documentation, you must have Sphinx installed on your system.
Sphinx can be easily installed with pip, via::
..
    $  pip install Sphinx
..
Once Sphinx is installed, you can build the PyReshaper HTML documentation
with::
..
    $  cd docs
    $  make html
..
The resulting HTML documentation will be placed in the `docs/build/html`
directory, and the main page can be loaded with any browser pointing to
`docs/build/html/index.html`.

..
Before Use
----------
..
This section should detail everything that needs to be done before the
newly installed package (see above) can be used.  Generally, this is
fairly simple, but you probably want to clearly explain these steps,
anyway.  It generally goes like the following.
..
Before the package can be used, you must make sure that the
site-packages directory containing the 'PyTemplate' installation
directory is in your `PYTHONPATH`.  Depending on the `PREFIX` used during
installation, this path will be::
..
    $PREFIX/lib/python2.X/site-packages
..
where `X` will be 6 or 7 (or other) depending on the version of Python
that you are using to install the package.
..
To use the scripts, you must add the script binary directory to your
`PATH`.  Depending on the `PREFIX` used during installation, this path will
be::
..
    $PREFIX/bin/
..
Once the script binary directory has been added to your `PATH` and the
site-packages directory has been added to your `PYTHONPATH`, you may use
the package without issue.
..
If installation was done to a location that is already in the `PYTHONPATH`,
and the binaries are installed to a location already in the `PATH`, then
everything should work after installation.

..
Instructions & Use
------------------
..
This section should point the reader to the more detailed user
manual and additional documentation.
