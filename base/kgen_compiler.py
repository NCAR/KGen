# kgen_compiler.py

import os

class GenericFortranCompiler(object):
    FORT_EXTS = ['f', 'f90', 'f95', 'f03', 'f08', 'F', 'F90', 'F95', 'F03', 'F08' ]
    COMPILERS = [ 'intel', 'gnu', 'pgi', 'cray', 'pathscale', 'ibm' ]
    OPENMP_FLAGS = [ '-fopenmp', '-openmp', '-mp', '-qsmp' ] # NOTE: -mp(PGI) -qsmp(IBM) may have suboptions. PathScale:-mp only, Cray turn on openmp default: noomp for turn off, 

    #Portland Group  Pathscale   Cray    Intel   GNU Explanation
    #-mp=nonuma  -mp -Oomp (default) -openmp -fopenmp    Activate OpenMP directives and pragmas in the code

    @classmethod
    def _getmacro(cls, macro):
        splitmacro = macro.split('=')
        if len(splitmacro)==1:
           return (splitmacro[0], '')
        elif len(splitmacro)==2:
            return tuple(splitmacro)
        else: raise

    @classmethod
    def parse_option(cls, options, pwd):
        incs = []
        macros = []
        openmp = []
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
                macros.append(cls._getmacro(item))
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
                    macros.append(cls._getmacro(item[2:]))
                else: dflag = True
            elif item in cls.OPENMP_FLAGS:
                openmp.append(item)
            elif item.split('.')[-1] in cls.FORT_EXTS:
                if item[0]=='/':
                    srcs.append(item)
                else:
                    srcs.append(os.path.realpath('%s/%s'%(pwd,item)))
            else:
                pass
        if len(srcs)>0:
            return (srcs, incs, macros, openmp)
        else:
            return ([], [], [], [])

class IntelFortranCompiler(GenericFortranCompiler):
    # space: False-no space, True-space required, None - any
    # args: None-none, [|]-optional string, 
    OPTIONS = { \
        '-mmic': { 'args': None, 'default': False },
        '-falias': { 'args': None, 'default': True },
        '-fast': { 'args': None, 'default': False },
        '-arch': { 'args': '[CORE-AVX2|CORE-AVX-I|AVX|SSE4.2|SSE4.1|SSSE3|SSE3|SSE2|SSE|IA32]', 'default': 'SSE2', 'space': True },
        '-ax': { 'args': '[COMMON-AVX512|MIC-AVX512|CORE-AVX512|CORE-AVX2|CORE-AVX-I|AVX|SSE4.2|SSE4.1|SSSE3|SSE3|SSE2]', 'default': False, 'space': False },
        '-m32': { 'args': None, 'default': False },
        '-m64': { 'args': None, 'default': False },
        '-xHost': { 'args': None, 'default': False },
        '-ip': { 'args': None, 'default': False },
        '-ipo': { 'args': '\d+', 'default': False, 'space': False },
        '-no-ipo': { 'args': None, 'default': True },
        '-mkl': { 'args': '[|=parallel|=sequential|=cluster]', 'default': False, 'space': False },
        '-pad': { 'args': None, 'default': False },
        '-no-pad': { 'args': None, 'default': False },
        '-qopt-prefetch': { 'args': '[|=0|=1|=2|=3|=4]', 'default': False, 'space': False },
        '-qno-opt-prefetch': { 'args': None, 'default': False },
        '-simd': { 'args': None, 'default': True },
        '-no-simd': { 'args': None, 'default': False },
        '-unroll': { 'args': '[|=\d+]', 'default': False, 'space': False },
        '-vec': { 'args': None, 'default': True },
        '-no-vec': { 'args': None, 'default': False },
        '-qopt-report': { 'args': '[|=0|=1|=2|=3|=4|=5]', 'default': False, 'space': False },
        '-parallel': { 'args': None, 'default': False },
        '-qopenmp': { 'args': None, 'default': False },
        '-qno-openmp': { 'args': None, 'default': True },
        '-fma': { 'args': None, 'default': True },
        '-no-fma': { 'args': None, 'default': False },
        '-fp-model': { 'args': '[precise|strict|source|fast=1|fast=2|except|no-except]', 'default': 'fast=1', 'space': True },
        '-ftz': { 'args': None, 'default': True },
        '-no-ftz': { 'args': None, 'default': False },
        '-prec-div': { 'args': None, 'default': True },
        '-no-prec-div': { 'args': None, 'default': False },
        '-prec-sqrt': { 'args': None, 'default': True },
        '-no-prec-sqrt': { 'args': None, 'default': False },
        '-finline': { 'args': None, 'default': False },
        '-fno-inline': { 'args': None, 'default': True },

    }
    pass

