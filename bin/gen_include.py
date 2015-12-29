#!/usr/bin/python
#
# geninc.py: generates include INI file from CESM build log
#
# Version: 0.1
# Author : Youngsung Kim ( kgen@ucra.edu )
# Usage  : gen_include.py [flags] <CESM build log>
#          flags: -o or --outfile <output filename>

import re
import sys
import ConfigParser
import optparse

parser = optparse.OptionParser()
parser.add_option("-o", "--outfile", dest="outfile", action='store', type='string', help="output file")
options, args = parser.parse_args()

Config = ConfigParser.RawConfigParser()
Config.optionxform = str
with open(args[0], 'r') as f:
    for l, line in enumerate(f):
        include = re.findall(r'\-I\s*([^\s]+)\s+', line)
        ppvar = re.findall(r'\-D\s*([^\s]+)\s+', line)
        source = re.findall(r'[^>]\s+([^\s]+\.(?:F77|f77|F90|f90|F|f|C|c|cpp|CPP)\b$)', line)

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
            inclist = include

        if source:
            if len(source)>1: raise Exception('Multiple source files in a line')
            for item in source:
                if Config.has_section(item):
                    print 'Warning: %s section is dupulicated at line# %d.' % (item, l+1)
                else:
                    Config.add_section(item)
                    # may not need include paths
                    Config.set(item,'include',':'.join(inclist))
                    for name, value in ppvarlist:
                        Config.set(item, name, value)

if options.outfile:
    cfgfile = open(options.outfile,'w')
else:
    cfgfile = open('include.ini','w')
Config.write(cfgfile)
cfgfile.close()
