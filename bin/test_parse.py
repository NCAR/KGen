#!/bin/env python

import os
import sys

KGEN_HOME = '%s/..'%os.path.dirname(os.path.realpath(__file__))
KGEN_BASE = '%s/base'%KGEN_HOME

sys.path.insert(0, KGEN_BASE)
from api import parse

flags = '-w -traditional'
includes = ''
macros = ''

def exec_cmd(cmd, show_error_msg=True, input=None):
    import subprocess

    proc = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    out, err = proc.communicate(input=input)

    ret_code = proc.wait()
    if ret_code != 0 and show_error_msg:
        print '>> %s' % cmd
        print 'returned non-zero code from shell('+str(ret_code)+')\n OUTPUT: '+str(out)+'\n ERROR: '+str(err)+'\n'

    return out

#def handle_include(self, lines):
#    import re
#    import os
#
#    insert_lines = []
#    for i, line in enumerate(lines):
#        match = re.match(r'^\s*include\s*("[^"]+"|\'[^\']+\')\s*\Z', line, re.I)
#        #if not match:
#        #    match = re.match(r'\s*#include\s*("[^"]+"|\<[^\']+\>)\s*\Z', line, re.I)
#        if match:
#            if Config.include['file'].has_key(self.abspath):
#                include_dirs = Config.include['file'][self.abspath]['path']+Config.include['path']
#            else:
#                include_dirs = Config.include['path']
#            filename = match.group(1)[1:-1].strip()
#            path = filename
#            for incl_dir in include_dirs+[os.path.dirname(self.abspath)]:
#                path = os.path.join(incl_dir, filename)
#                if os.path.exists(path):
#                    break
#            if os.path.isfile(path):
#                with open(path, 'r') as f:
#                    included_lines = f.read()
#                    insert_lines.extend(handle_include(included_lines.split('\n')))
#            else:
#                raise UserException('Can not find %s in include paths of %s.'%(filename, self.abspath))
#        else:
#            insert_lines.append(line)
#
#    return insert_lines


new_lines = []
with open(sys.argv[1], 'r') as f:
    output = exec_cmd('cpp %s %s %s' % (flags, includes, macros), input=f.read())
    prep = map(lambda l: '!KGEN'+l if l.startswith('#') else l, output.split('\n'))
    #new_lines = handle_include(prep)

tree = parse('\n'.join(prep), ignore_comments=False, analyze=True, isfree=True, \
            isstrict=False, source_only=None )
print tree


