===============
Getting-started
===============


--------
Download
--------

Source and binary releases: http://pypi.python.org/pypi/kgen/

Github (latest development): https://github.com/kgen/kgen/

----------
Installing
----------

Installing with pip
```````````````````
Try to install it with

::

   pip install kgen

and an attempt will be made to find and install an appropriate version
that matches your operating system and Python version.

You can also get KGen from the Python Package Index manually
at http://pypi.python.org/pypi/kgen
To use pip, you need to have `setuptools <https://pypi.python.org/pypi/setuptools>`_ installed.

You can install the development version (at github.com) with

::

  pip install git://github.com/kgen/kgen.git#egg=kgen

More download file options are at http://kgen.github.io/download.html.


Installing from source
``````````````````````

You can install from source by downloading a source archive file
(tar.gz or zip) or by checking out the source files from the
Git source code repository.

KGen is a pure Python package; you don't need a compiler to build
or install it.

Source archive file
~~~~~~~~~~~~~~~~~~~

  1. Download the source (tar.gz or zip file) from
     https://pypi.python.org/pypi/kgen/
     or get the latest development version from
     https://github.com/kgen/kgen/

  2. Unpack and change directory to the source directory
     (it should have the files README.txt and setup.py).

  3. Run :samp:`python setup.py install` to build and install

  4. (Optional) Run :samp:`nosetests` to execute the tests if you have
     `nose <https://pypi.python.org/pypi/nose>`_ installed.


GitHub
~~~~~~

  1. Clone the kgen repository
     (see https://github.com/kgen/kgen/ for options)
     ::

       git clone https://github.com/kgen/kgen.git


  2. Change directory to :samp:`kgen`

  3. Run :samp:`python setup.py install` to build and install

  4. (Optional) Run :samp:`nosetests` to execute the tests if you have
     `nose <https://pypi.python.org/pypi/nose>`_ installed.


If you don't have permission to install software on your
system, you can install into another directory using
the :samp:`--user`, :samp:`--prefix`, or :samp:`--home` flags to setup.py.

For example

::

    python setup.py install --prefix=/home/username/python

or

::

    python setup.py install --home=~

or

::

    python setup.py install --user

If you didn't install in the standard Python site-packages directory
you will need to set your PYTHONPATH variable to the alternate location.
See http://docs.python.org/2/install/index.html#search-path for further details.


Requirements
````````````

Python
~~~~~~

To use KGen you need Python 2.7 or later, but less than 3.0.

----------------------------------
First Kernel-generation using KGen
----------------------------------

----------------
Further reading
----------------

