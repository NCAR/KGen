# kgen_compiler.py

import os

COMPILERS = [ 'intel', 'gnu', 'pgi', 'cray', 'pathscale', 'ibm' ]
FORT_EXTS = ['f', 'f90', 'f95', 'f03', 'f08', 'F', 'F90', 'F95', 'F03', 'F08' ]
OPENMP_FLAGS = [ '-fopenmp', '-openmp', '-mp', '-qsmp' ] # NOTE: -mp(PGI) -qsmp(IBM) may have suboptions. PathScale:-mp only, Cray turn on openmp default: noomp for turn off, 

#Portland Group  Pathscale   Cray    Intel   GNU Explanation
#-mp=nonuma  -mp -Oomp (default) -openmp -fopenmp    Activate OpenMP directives and pragmas in the code



def _getmacro(macro):
    splitmacro = macro.split('=')
    if len(splitmacro)==1:
       return (splitmacro[0], '')
    elif len(splitmacro)==2:
        return tuple(splitmacro)
    else: raise

class GenericCompiler(object):
    @classmethod
    def parse_option(cls, options, pwd):
        incs = []
        macros = []
        srcs = []
        iflag = False
        dflag = False
        for item in options[1:]:
            if iflag:
                for p in item.split(':'):
                    if p[0]=='/':
                        incs.append(p)
                    else:
                        incs.append(os.path.realpath('%s/%s'%(pwd,p)))
                iflag = False
                continue
            if dflag:
                macros.append(_getmacro(item))
                dflag = False
                continue

            if item.startswith('-I'):
                if len(item)>2:
                    for p in item[2:].split(':'):
                        if p[0]=='/':
                            incs.append(p)
                        else:
                            incs.append(os.path.realpath('%s/%s'%(pwd,p)))
                else: iflag = True
            elif item.startswith('-D'):
                if len(item)>2:
                    macros.append(_getmacro(item[2:]))
                else: dflag = True
            elif item.split('.')[-1] in FORT_EXTS:
                if item[0]=='/':
                    srcs.append(item)
                else:
                    srcs.append(os.path.realpath('%s/%s'%(pwd,item)))
            else: pass
        return srcs, incs, macros 
