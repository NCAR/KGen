#! /usr/bin/env python

import sys
import os.path

tool_dir = os.path.dirname(os.path.realpath(__file__))
package_dir = '%s/site-packages'%tool_dir
sys.path.insert(0, package_dir)

"""
doflakes is an application that execute pyflakes as doit tasks.

Advantages over plain pyflakes
--------------------------------

 * do not re-execute check for files without modification since last run
 * multiprocess support
 * watch file system support - auto execute whenever a file changes

Install
---------

  $ pip install pyflakes
  $ pip install doit

copy this script to somewhere in your PATH

Usage
--------

  $ doflakes [-w] <file/folder>

use option `-w` to run in "watch mode"

"""


from doit.loader import generate_tasks
from doit.doit_cmd import DoitMain
from doit.cmd_base import TaskLoader
from pyflakes.scripts.pyflakes import checkPath


class FlakeTaskLoader(TaskLoader):
    """create pyflakes tasks on the fly based on cmd-line arguments"""
    DOIT_CONFIG = {
        'verbosity': 2,
        'continue': True,
        #'reporter': 'zero',
        'dep_file': os.path.join(os.path.expanduser("~"), '.doflakes'),
        'num_process': 2,
        }

    def __init__(self, args):
        """set list of files to be checked
        @param args (list - str) file/folder path to apply pyflakes
        """
        self.files = []
        for arg in args:
            if os.path.isdir(arg):
                for dirpath, dirnames, filenames in os.walk(arg):
                    for filename in filenames:
                        if filename.endswith('.py'):
                            self.files.append(os.path.join(dirpath, filename))
            else:
                self.files.append(arg)

        for path in self.files:
            if not os.path.exists(path):
                sys.stderr.write('%s: No such file or directory\n' % path)

    @staticmethod
    def check_path(filename):
        """execute pyflakes checker"""
        return not bool(checkPath(filename))

    def _gen_tasks(self):
        """generate doit tasks for each file to be checked"""
        for filename in self.files:
            path = os.path.abspath(filename)
            yield {
                'name': path,
                'file_dep': [path],
                'actions': [(self.check_path, (filename,))],
                }

    def load_tasks(self, cmd, params, args):
        """implements loader interface, return (tasks, config)"""
        return generate_tasks('pyflake', self._gen_tasks()), self.DOIT_CONFIG


if __name__ == "__main__":
    cmd = 'run' # default doit command
    args = [] # list of positional args from cmd line
    for arg in sys.argv[1:]:
        if arg == '-w': # watch for changes
            cmd = 'auto'
        else:
            args.append(arg)
    doit_main = DoitMain(FlakeTaskLoader(args))
    sys.exit(doit_main.run([cmd]))
