# kgen_compiler.py

import os
import re

class GenericCompiler(object):
    compid = None
    compnames = []
    openmp = None
    file_exts = None
    fpp = ''

    discard_opts_noarg = [ ]
    discard_opts_arg = [ ]

    def get_discard_opts_noarg(self):
        return [ '-c' ]

    def get_discard_opts_arg(self):
        return [ '-o', '-l', '-L', '-W' ]

    def _getmacro(self, macro):
        splitmacro = macro.split('=')
        if len(splitmacro)==1:
           return (splitmacro[0], None)
        elif len(splitmacro)==2:
            return tuple(splitmacro)
        else:
            raise

    def parse_option(self, options, pwd):
        incs = []
        macros = []
        openmp = []
        srcs = []
        flags = []

        discard_flag = False

        iflag = False
        dflag = False

        for item in options[1:]:

            if discard_flag:
                discard_flag = False
                continue

            if iflag:
                for p in item.split(':'):
                    if p[0]=='/':
                        incs.append(p)
                    else:
                        incs.append(os.path.realpath('%s/%s'%(pwd,p)))
                iflag = False
                continue
            if dflag:
                macros.append(self._getmacro(item))
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
                    macros.append(self._getmacro(item[2:]))
                else: dflag = True
            elif self.openmp_opt and any(not re.match(r'%s\Z'%pattern, item) is None for pattern in self.openmp_opt):
                    openmp.append(item)
            elif item.startswith('-'):
                if item in self.get_discard_opts_noarg():
                    pass
                elif any( item.startswith(f) for f in self.get_discard_opts_arg() ):
                    for f in self.get_discard_opts_arg():
                        if item==f:
                            if len(item)==len(f):
                                discard_flag = True
                            break
                else:
                    flags.append(item)
            elif self.file_exts and item.split('.')[-1] in self.file_exts:
                if item[0]=='/':
                    srcs.append(item)
                else:
                    srcs.append(os.path.realpath('%s/%s'%(pwd,item)))
            else:
                flags.append(item)

        if len(srcs)>0:
            return (srcs, incs, macros, openmp, flags)
        else:
            return ([], [], [], [], [])

class GenericFortranCompiler(GenericCompiler):
    file_exts = ['f', 'f90', 'f95', 'f03', 'f08', '.ftn', 'F', 'F90', 'F95', 'F03', 'F08', '.FTN' ]

    def get_discard_opts_noarg(self):
        return super(GenericFortranCompiler, self).get_discard_opts_noarg()

    def get_discard_opts_arg(self):
        return super(GenericFortranCompiler, self).get_discard_opts_arg()

class IntelFortranCompiler(GenericFortranCompiler):
    compid = 'ifort'
    compnames = ['ifort']
    openmp_opt = [ r'-openmp' ]
    fpp = '-fpp'

    discard_opts_arg = [ '-module' ]

    def get_discard_opts_noarg(self):
        return super(IntelFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(IntelFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class GnuFortranCompiler(GenericFortranCompiler):
    compid = 'gfortran'
    compnames = ['gfortran']
    openmp_opt = [ r'-fopenmp' ]
    fpp = '-cpp'

    discard_opts_arg = [ '-J' ]

    def get_discard_opts_noarg(self):
        return super(GnuFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(GnuFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class PgiFortranCompiler(GenericFortranCompiler):
    compid = 'pgfortran'
    compnames = ['pgfortran', 'pgf77', 'pgf90', 'pgf95', 'pghpf']
    openmp_opt = [ r'-mp' ]
    fpp = '-Mpreprocess'

    discard_opts_arg = [ '-module' ]

    def get_discard_opts_noarg(self):
        return super(PgiFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(PgiFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class PathscaleFortranCompiler(GenericFortranCompiler):
    compid = 'path90'
    compnames = ['path90', 'path95']
    openmp_opt = [ r'-mp' ]

    def get_discard_opts_noarg(self):
        return super(PathscaleFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(PathscaleFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class NagFortranCompiler(GenericFortranCompiler):
    compid = 'nagfor'
    compnames = ['nagfor']
    openmp_opt = [ r'-openmp' ]

    def get_discard_opts_noarg(self):
        return super(NagFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(NagFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class IbmxlFortranCompiler(GenericFortranCompiler):
    compid = 'xlf'
    compnames = ['xlf', 'xlf90', 'xlf95', 'xlf2003', 'xlf2008']
    openmp_opt = [ r'-qsmp' ]

    def get_discard_opts_noarg(self):
        return super(IbmxlFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(IbmxlFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class CrayFortranCompiler(GenericFortranCompiler):
    compid = 'crayftn'
    compnames = ['crayftn', 'ftn']
    openmp_opt = [ r'-omp', r'-h\s+omp' ]

    discard_opts_arg = [ '-J' ]

    def get_discard_opts_noarg(self):
        return super(IbmxlFortranCompiler, self).get_discard_opts_noarg() + self.discard_opts_noarg

    def get_discard_opts_arg(self):
        return super(IbmxlFortranCompiler, self).get_discard_opts_arg() + self.discard_opts_arg

class CompilerFactory(object):

    factories = {}

    @staticmethod
    def get_subclasses(cls):
        subclasses = set()
        work = [cls]
        while work:
            parent = work.pop()
            for child in parent.__subclasses__():
                if child not in subclasses:
                    subclasses.add(child)
                    work.append(child)
        return subclasses

    @staticmethod
    def createCompiler(compid):
        for subc in CompilerFactory.get_subclasses(GenericCompiler):
            if subc.compnames and compid in subc.compnames:
                if not CompilerFactory.factories.has_key(subc.compid):
                    CompilerFactory.factories[subc.compid] = subc()
                return CompilerFactory.factories[subc.compid]

#class IntelFortranCompiler(GenericFortranCompiler):
#    # space: False-no space, True-space required, None - any
#    # args: None-none, [|]-optional string, 
#    OPTIONS = { \
#        '-mmic': { 'args': None, 'default': False },
#        '-falias': { 'args': None, 'default': True },
#        '-fast': { 'args': None, 'default': False },
#        '-arch': { 'args': '[CORE-AVX2|CORE-AVX-I|AVX|SSE4.2|SSE4.1|SSSE3|SSE3|SSE2|SSE|IA32]', 'default': 'SSE2', 'space': True },
#        '-ax': { 'args': '[COMMON-AVX512|MIC-AVX512|CORE-AVX512|CORE-AVX2|CORE-AVX-I|AVX|SSE4.2|SSE4.1|SSSE3|SSE3|SSE2]', 'default': False, 'space': False },
#        '-m32': { 'args': None, 'default': False },
#        '-m64': { 'args': None, 'default': False },
#        '-xHost': { 'args': None, 'default': False },
#        '-ip': { 'args': None, 'default': False },
#        '-ipo': { 'args': '\d+', 'default': False, 'space': False },
#        '-no-ipo': { 'args': None, 'default': True },
#        '-mkl': { 'args': '[|=parallel|=sequential|=cluster]', 'default': False, 'space': False },
#        '-pad': { 'args': None, 'default': False },
#        '-no-pad': { 'args': None, 'default': False },
#        '-qopt-prefetch': { 'args': '[|=0|=1|=2|=3|=4]', 'default': False, 'space': False },
#        '-qno-opt-prefetch': { 'args': None, 'default': False },
#        '-simd': { 'args': None, 'default': True },
#        '-no-simd': { 'args': None, 'default': False },
#        '-unroll': { 'args': '[|=\d+]', 'default': False, 'space': False },
#        '-vec': { 'args': None, 'default': True },
#        '-no-vec': { 'args': None, 'default': False },
#        '-qopt-report': { 'args': '[|=0|=1|=2|=3|=4|=5]', 'default': False, 'space': False },
#        '-parallel': { 'args': None, 'default': False },
#        '-qopenmp': { 'args': None, 'default': False },
#        '-qno-openmp': { 'args': None, 'default': True },
#        '-fma': { 'args': None, 'default': True },
#        '-no-fma': { 'args': None, 'default': False },
#        '-fp-model': { 'args': '[precise|strict|source|fast=1|fast=2|except|no-except]', 'default': 'fast=1', 'space': True },
#        '-ftz': { 'args': None, 'default': True },
#        '-no-ftz': { 'args': None, 'default': False },
#        '-prec-div': { 'args': None, 'default': True },
#        '-no-prec-div': { 'args': None, 'default': False },
#        '-prec-sqrt': { 'args': None, 'default': True },
#        '-no-prec-sqrt': { 'args': None, 'default': False },
#        '-finline': { 'args': None, 'default': False },
#        '-fno-inline': { 'args': None, 'default': True },
#
#    }
#    pass
#
