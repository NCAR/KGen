#!/usr/bin/python
#
# gen_include.py: generates include INI file from MPAS build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )
# Usage  : gen_include.py [flags] <CESM build log>
#          flags: -o or --outfile <output filename>

import re
import sys
import ConfigParser
import optparse

cwd = []

parser = optparse.OptionParser()
parser.add_option("-o", "--outfile", dest="outfile", action='store', type='string', help="output file")
options, args = parser.parse_args()

Config = ConfigParser.RawConfigParser()
Config.optionxform = str
with open(args[0], 'r') as f:
    for l, line in enumerate(f):
        entering = re.match(r"make.+Entering\sdirectory\s`(.+)'$", line)
        if entering:
            cwd.append(entering.group(1))
        leaving = re.match(r"make.+Leaving\sdirectory\s`(.+)'$", line)
        if leaving:
            if leaving.group(1)==cwd[-1]:
                del cwd[-1]
            else:
                raise "Leaving directory path mismatch with the last entering directory"

        include = re.findall(r'\-I\s*([^\s]+)\s+', line)
        ppvar = re.findall(r'\-D\s*([^\s]+)\s+', line)
        #source = re.findall(r'\-c\s+([^\s]+\.(?:F77|f77|F90|f90|F|f|C|c|cpp|CPP)\b$)', line)
        source = re.findall(r'\-c\s+([^\s]+\.(?:F77|f77|F90|f90|F|f|C|c|cpp|CPP))\s', line)

        ppvarlist = []
        if ppvar:
            for item in ppvar:
                pos = item.find('=')
                if pos>0:
                    ppvarlist += [ (item[:pos], item[pos+1:]) ]
                else:
                    ppvarlist += [ (item, '1') ]
        inclist = []
        if include:
            for inc in include:
                if inc.startswith('.'):
                    inclist.append(cwd[-1]+'/'+inc)
                else:
                    inclist.append(inc)

        if source:
            if len(source)>1: raise Exception('Multiple source files in a line')
            for item in source:
                path = cwd[-1]+'/'+item
                if Config.has_section(path):
                    print 'Warning: %s section is dupulicated at line# %d.' % (path, l+1)
                else:
                    Config.add_section(path)
                    # may not need include paths
                    Config.set(path,'include',':'.join(inclist))
                    for name, value in ppvarlist:
                        Config.set(path, name, value)

if options.outfile:
    cfgfile = open(options.outfile,'w')
else:
    cfgfile = open('include.ini','w')
Config.write(cfgfile)
cfgfile.close()
