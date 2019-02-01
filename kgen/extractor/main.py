'''KGen kernel extractor
'''

import os
import glob
import shutil
import kgutils
from collections import OrderedDict
from kgtool import KGTool
from kgconfig import Config
from kggenfile import genkobj, gensobj, KERNEL_ID_0, event_register, create_rootnode, create_programnode, \
    append_program_in_root, set_indent
from parser.kgparse import KGGenType
from parser.kgextra import kgen_utils_file_head, kgen_utils_file_checksubr, kgen_get_newunit, kgen_error_stop, \
    kgen_utils_file_tostr, kgen_utils_array_sumcheck, kgen_rankthread

KGUTIL = 'kgen_utils.f90'
TPROF = 'tprof_mod.f90'

class Extractor(KGTool):

    def run(self):

        self._trees = []
        self.genfiles = []

        kgutils.logger.info('Starting KExtract')

        # clear shared resources
        Config.used_srcfiles.clear()

        # create kernel directory
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.path['kernel'])):
            os.makedirs('%s/%s'%(Config.path['outdir'], Config.path['kernel']))

        # create state directory
        if not os.path.exists('%s/%s'%(Config.path['outdir'], Config.path['state'])):
            os.makedirs('%s/%s'%(Config.path['outdir'], Config.path['state']))

        # generate kernel and instrumentation
        if 'all' in Config.rebuild or 'extract' in Config.rebuild or \
            not os.path.exists('%s/%s/Makefile'%(Config.path['outdir'], Config.path['state'])) or \
            len(glob.glob('%s/%s.*'%(Config.path['outdir'], Config.kernel['name']))) == 0:

            # generate kgen_driver.f90 in kernel directory
            driver = create_rootnode(KERNEL_ID_0)
            self._trees.append(driver)
            program = create_programnode(driver, KERNEL_ID_0)
            program.name = Config.kernel_driver['name']
            append_program_in_root(driver, program)

            # generate instrumentation
            for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():
                if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                    kfile = genkobj(None, srcobj.tree, KERNEL_ID_0)
                    sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                    sfile.kgen_stmt.used4genstate = False
                    if kfile is None or sfile is None:
                        raise kgutils.ProgramException('Kernel source file is not generated for %s.'%filepath)
                    self.genfiles.append((kfile, sfile, filepath))
                    Config.used_srcfiles[filepath] = (kfile, sfile, mods_used, units_used)

            # process each nodes in the tree
            for plugin_name in event_register.keys():
                if not plugin_name.startswith('ext'): continue

                for kfile, sfile, filepath in self.genfiles:
                    kfile.created([plugin_name])
                    sfile.created([plugin_name])
                for tree in self._trees:
                    tree.created([plugin_name])

            for plugin_name in event_register.keys():
                if not plugin_name.startswith('ext'): continue

                for kfile, sfile, filepath in self.genfiles:
                    kfile.process([plugin_name])
                    sfile.process([plugin_name])
                for tree in self._trees:
                    tree.process([plugin_name])

            for plugin_name in event_register.keys():
                if not plugin_name.startswith('ext'): continue

                for kfile, sfile, filepath in self.genfiles:
                    kfile.finalize([plugin_name])
                    sfile.finalize([plugin_name])
                for tree in self._trees:
                    tree.finalize([plugin_name])

            for plugin_name in event_register.keys():
                if not plugin_name.startswith('ext'): continue

                for kfile, sfile, filepath in self.genfiles:
                    kfile.flatten(KERNEL_ID_0, [plugin_name])
                    sfile.flatten(KERNEL_ID_0, [plugin_name])
                for tree in self._trees:
                    tree.flatten(KERNEL_ID_0, [plugin_name])

            # generate source files from each node of the tree
            kernel_files = []
            state_files = []
            for kfile, sfile, filepath in self.genfiles:
                filename = os.path.basename(filepath)
                set_indent('')
                klines = kfile.tostring()
                if klines is not None:
                    klines = kgutils.remove_multiblanklines(klines)
                    kernel_files.append(filename)
                    with open('%s/%s/%s'%(Config.path['outdir'], Config.path['kernel'], filename), 'wb') as fd:
                        fd.write(klines)

                if sfile.kgen_stmt.used4genstate:
                    set_indent('')
                    slines = sfile.tostring()
                    if slines is not None:
                        slines = kgutils.remove_multiblanklines(slines)
                        state_files.append(filename)
                        with open('%s/%s/%s'%(Config.path['outdir'], Config.path['state'], filename), 'wb') as fd:
                            fd.write(slines)

            with open('%s/%s/%s'%(Config.path['outdir'], Config.path['kernel'], '%s.f90'%Config.kernel_driver['name']), 'wb') as fd:
                set_indent('')
                lines = driver.tostring()
                if lines is not None:
                    lines = kgutils.remove_multiblanklines(lines)
                    fd.write(lines)
            kernel_files.append(Config.kernel['name'])

            kgutils.logger.info('Kernel generation and instrumentation is completed.')

            # generate kgen_utils.f90 in kernel directory
            kernel_files.append(KGUTIL)
            self.generate_kgen_utils()

            shutil.copyfile('%s/%s'%(os.path.dirname(os.path.realpath(__file__)), TPROF), \
                '%s/%s/%s'%(Config.path['outdir'], Config.path['kernel'], TPROF))
            kernel_files.append(TPROF)

            self.generate_kernel_makefile()
            kernel_files.append('Makefile')

            self.generate_state_makefile()
            state_files.append('Makefile')

            kgutils.logger.info('Makefiles are generated')


            # TODO: wait until state data generation is completed
            # use -K option for bsub to wait for job completion

            # clean app
            if Config.cmd_clean['cmds']:
                kgutils.run_shcmd(Config.cmd_clean['cmds'])
            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])

            # build and run app with state instrumentation
            kgutils.logger.info('Application is being built/run with state generation instrumentation.')
            out, err, retcode = kgutils.run_shcmd('make', cwd='%s/%s'%(Config.path['outdir'], Config.path['state']))

            out, err, retcode = kgutils.run_shcmd('make recover', cwd='%s/%s'%(Config.path['outdir'], Config.path['state']))
            if Config.state_switch['clean']:
                kgutils.run_shcmd(Config.state_switch['clean'])

            kgutils.logger.info('Application built/run is finished.')

    def write(self, f, line, n=True, t=False):
        nl = ''
        tab = ''
        if n: nl = '\n'
        if t: tab = '\t'
        f.write(tab + line + nl)

    def obj(self, file):
        l = file.split('.')
        if len(l)>1:
            l[-1] = 'o'
        return '.'.join(l)

    def generate_kernel_makefile(self):
        #NOTE: for gfortran, use -ffixed-line-length-none and -ffree-line-length-none

        openmp_flags = ["-fopenmp", "-qopenmp", "-mp", "-xopenmp", "-qsmp=omp"]

        #basenames
        callsite_base = os.path.basename(Config.callsite['filepath'])
        driver_base = '%s.f90'%Config.kernel_driver['name']
        dep_base_srcfiles = [ os.path.basename(filepath) for filepath, srclist in Config.used_srcfiles.iteritems() ]
        dep_bases = dep_base_srcfiles + [ driver_base ]

        # all object files
        all_objs_srcfiles = [ self.obj(dep_base_srcfile) for dep_base_srcfile in dep_base_srcfiles ]
        all_objs = all_objs_srcfiles + [ self.obj(driver_base), self.obj(KGUTIL), self.obj(TPROF) ]

        # dependency
        depends = OrderedDict()

        # dependency for kernel_driver.f90
        depends[driver_base] = ' '.join(all_objs_srcfiles + [self.obj(KGUTIL), self.obj(TPROF) ])

        # dependency for other files
        for filepath, (kfile, sfile, mods_used, units_used) in Config.used_srcfiles.iteritems():
            dep = [ self.obj(KGUTIL), self.obj(TPROF) ]
            for mod in mods_used:
                if mod.reader.id!=filepath and \
                    not self.obj(os.path.basename(mod.reader.id)) in dep:
                    dep.append(self.obj(os.path.basename(mod.reader.id)))
            for unit in units_used:
                if unit.item.reader.id!=filepath and \
                    not self.obj(os.path.basename(unit.item.reader.id)) in dep:
                    dep.append(self.obj(os.path.basename(unit.item.reader.id)))

            basename = os.path.basename(filepath)
            if basename==callsite_base:
                dobjs = all_objs[:]
                dobjs.remove(self.obj(callsite_base))
                dobjs.remove(self.obj(driver_base))
                depends[basename] = ' '.join(dobjs)
            else:
                depends[basename] = ' '.join(dep)

        # find fc flag sets
        compilers = OrderedDict()
        compiler_options = OrderedDict()
        for path, kfile in Config.include['file'].items():

            base = os.path.basename(path)
            if base not in dep_bases: continue

            comp = None
            if kfile.has_key('compiler'):
                comp = kfile['compiler']
            elif Config.include['compiler'].has_key('compiler'):
                comp = Config.include['compiler']['compiler']

            if comp:
                if comp in compilers:
                    if base not in compilers[comp]:
                        compilers[comp].append(base)
                else:
                    compilers[comp] = [ base, driver_base]

            opts = ''
            if Config.include['compiler'].has_key('compiler_options'):
                opts = opts + ' ' + Config.include['compiler']['compiler_options']

            if kfile.has_key('compiler_options') and kfile['compiler_options']:
                opts = opts + ' ' + kfile['compiler_options']

            if Config.model['types']['code']['enabled']:
                if comp:
                    compname = os.path.basename(comp)
                    if compname =='ifort':
                        opts += ' -fpp '
                    elif compname =='gfortran':
                        opts += ' -cpp '
                    elif compname.startswith('pg'):
                        opts += ' -Mpreprocess '
                opts += ' -D KGEN_COVERAGE # Comment out "-D KGEN_COVERAGE" to turn off coverage feature.'

            if Config.add_mpi_frame["enabled"]:

                if comp:
                    compname = os.path.basename(comp)
                    if compname =='ifort' and "-fpp" not in opts:
                        opts += ' -fpp '
                    elif compname =='gfortran' and "-cpp" not in opts:
                        opts += ' -cpp '
                    elif compname.startswith('pg') and "-Mpreprocess" not in opts:
                        opts += ' -Mpreprocess '
                    opts += " -D_MPI "
 
            if len(opts)>0:
                if opts in compiler_options:
                    #if kfile['compiler_options'] and base not in compiler_options[kfile['compiler_options']]:
                    if base not in compiler_options[opts]:
                        compiler_options[opts].append(base)
                else:
                    compiler_options[opts] = [ base, driver_base ]

        # link flags and objects
        link_flags = ' '.join(Config.kernel_option['linker']['add'])
        objects = ''
        if Config.include.has_key('import'):
            for path, import_type in Config.include['import'].iteritems():
                if import_type == 'library' or import_type == 'shared_library':
                    inc = '-L'+path
                    pos1 = import_type.find('(')
                    pos2 = import_type.find(')')
                    lib = '-l'+import_type[(pos1+1):pos2].strip()
                    link_flags += ' %s %s'%(inc, lib)
                elif import_type == 'static-library':
                    link_flags += ' %s'%path
                elif import_type=='object':
                    objects += ' %s'%os.path.basename(path)
                    shutil.copy(path, '%s/%s'%(Config.path['outdir'], Config.path['kernel']))

        with open('%s/%s/Makefile'%(Config.path['outdir'], Config.path['kernel']), 'wb') as f:
            self.write(f, '# Makefile for KGEN-generated kernel')
            self.write(f, '')

            if Config.kernel_option['FC']:
                #self.write(f, 'FC := %s'%Config.kernel_option['FC'])
                self.write(f, 'FC_0 := %s'%Config.kernel_option['FC'])
            elif Config.add_mpi_frame["enabled"]:
                self.write(f, '# Originally used compiler(s)')
                for i, compiler in enumerate(compilers):
                    self.write(f, '#FC_%d := %s'%(i, compiler))
                self.write(f, 'FC_0 := %s'%Config.add_mpi_frame["mpifc"])
            else:
                #self.write(f, 'FC := ')
                for i, compiler in enumerate(compilers):
                    self.write(f, 'FC_%d := %s'%(i, compiler))

            if Config.kernel_option['FC_FLAGS']:
                #self.write(f, 'FC_FLAGS := %s'%Config.kernel_option['FC_FLAGS'])
                self.write(f, 'FC_FLAGS_SET_0 := %s'%Config.kernel_option['FC_FLAGS'])
            else:
                #self.write(f, 'FC_FLAGS := ')
                for i, options in enumerate(compiler_options):
                    opt_list = options.split()
                    L = len(opt_list)
                    new_options = []
                    if L>1:
                        skip_next = False
                        for opt in opt_list:
                            if skip_next:
                                skip_next = False
                                continue
                            if opt in Config.kernel_option['compiler']['remove']:
                                pass
                            elif '%s+'%opt in Config.kernel_option['compiler']['remove']:
                                skip_next = True
                            elif opt not in openmp_flags:
                                new_options.append(opt)

                    for add_opt in Config.kernel_option['compiler']['add']:
                        if add_opt not in new_options:
                            new_options.append(add_opt)
                    self.write(f, 'FC_FLAGS_SET_%d := %s'%(i, ' '.join(new_options)))

            prerun_build_str = ''
            if Config.prerun['kernel_build']:
                self.write(f, 'PRERUN_BUILD := %s'%Config.prerun['kernel_build'])
                prerun_build_str = '${PRERUN_BUILD}; '
            elif Config.prerun['build']:
                self.write(f, 'PRERUN_BUILD := %s'%Config.prerun['build'])
                prerun_build_str = '${PRERUN_BUILD}; '

            prerun_run_str = ''
            if Config.prerun['kernel_run']:
                self.write(f, 'PRERUN_RUN := %s'%Config.prerun['kernel_run'])
                prerun_run_str = '${PRERUN_RUN}; '
            elif Config.prerun['run']:
                self.write(f, 'PRERUN_RUN := %s'%Config.prerun['run'])
                prerun_run_str = '${PRERUN_RUN}; '

            self.write(f, '')
            self.write(f, 'ALL_OBJS := %s'%' '.join(all_objs))
            self.write(f, '')

            if Config.model['types']['papi']['enabled']:
                self.write(f, 'PAPI_EVENT := %s'% Config.model['types']['papi']['event'] )
                self.write(f, '')

            self.write(f, 'run: build')
            if Config.add_mpi_frame['enabled']:
                self.write(f, '%s%s -np %s ./kernel.exe'%(prerun_run_str, Config.add_mpi_frame['mpirun'], Config.add_mpi_frame['np']), t=True)
            else:
                self.write(f, '%s./kernel.exe'%prerun_run_str, t=True)
            self.write(f, '')

            if Config.model['types']['papi']['enabled']:
                self.write(f, 'papi: build-papi')
                if Config.add_mpi_frame['enabled']:
                    self.write(f, '%s%s -np %s ./kernel.exe'%(prerun_run_str, Config.add_mpi_frame['mpirun'], Config.add_mpi_frame['np']), t=True)
                else:
                    self.write(f, '%s./kernel.exe'%prerun_run_str, t=True)
                self.write(f, '')

            self.write(f, 'build: ${ALL_OBJS}')

            fc_str = 'FC_0'
            fc_flags_str = 'FC_FLAGS_SET_0'

            #if len(compilers)>0 and not Config.kernel_option['FC']: fc_str += '_0'
            #if len(compiler_options)>0 and not Config.kernel_option['FC_FLAGS']: fc_flags_str += '_SET_0'

            self.write(f, '%s${%s} ${%s} -o kernel.exe $^ %s %s'%(prerun_build_str, fc_str, fc_flags_str, link_flags, objects), t=True)
            self.write(f, '')

            if Config.model['types']['papi']['enabled']:

                self.write(f, 'build-papi: ${ALL_OBJS}')

                fc_str = 'FC_0'
                fc_flags_str = 'FC_FLAGS_SET_0'

                #if len(compilers)>0 and not Config.kernel_option['FC']: fc_str += '_0'
                #if len(compiler_options)>0 and not Config.kernel_option['FC_FLAGS']: fc_flags_str += '_SET_0'

                if Config.model['types']['papi']['static'] is not None:
                    link_flags += ' %s'%Config.model['types']['papi']['static']

                if Config.model['types']['papi']['dynamic'] is not None:
                    ddir, dlib = os.path.split( Config.model['types']['papi']['dynamic'] )
                    root, ext = dlib.split('.', 1)
                    if len(root) > 3:
                        link_flags += ' -L%s -l%s'%( ddir, root[3:] )

                self.write(f, '%s${%s} ${%s} -o kernel.exe $^ %s %s'%(prerun_build_str, fc_str, fc_flags_str, link_flags, objects), t=True)
                self.write(f, '')


            for dep_base in dep_bases:
                self.write(f, '%s: %s %s' % (self.obj(dep_base), dep_base, depends[dep_base]))

                dfc_str = 'FC'
                dfc_flags_str = 'FC_FLAGS'

                if Config.kernel_option['FC'] is None:
                    for i, (compiler, files) in enumerate(compilers.items()):
                        if dep_base in files:
                            dfc_str += '_%d'%i
                            break
                if Config.kernel_option['FC_FLAGS'] is None:
                    for i, (compiler_option, files) in enumerate(compiler_options.items()):
                        if dep_base in files:
                            dfc_flags_str += '_SET_%d'%i
                            break

                if dfc_str == 'FC' or Config.add_mpi_frame["enabled"]:
                    dfc_str = 'FC_0'

                if dfc_flags_str == 'FC_FLAGS': dfc_flags_str = 'FC_FLAGS_SET_0'

                if Config.model['types']['papi']['enabled'] and Config.model['types']['papi']['header'] is not None and \
                    dep_base in [ callsite_base, driver_base ]:
                    self.write(f, 'ifeq (${MAKECMDGOALS}, papi)')
                    papi_flags_str = ' -DKGEN_PAPI -I%s'%os.path.split( Config.model['types']['papi']['header'])[0]
                    self.write(f, '%s %s %s $< | sed "s/KGENPAPIEVENT/${PAPI_EVENT}/g" > tmp.$<'%(Config.bin['pp'], Config.bin['cpp_flags'], papi_flags_str), t=True)
                    self.write(f, 'else')
                    self.write(f, '%s %s $< tmp.$<'%(Config.bin['pp'], Config.bin['cpp_flags']), t=True)
                    self.write(f, 'endif')
                    self.write(f, '%s${%s} ${%s} -c -o $@ tmp.$<'%(prerun_build_str, dfc_str, dfc_flags_str), t=True)
                    self.write(f, 'rm -f tmp.$<', t=True)
                else:
                    self.write(f, '%s${%s} ${%s} -c -o $@ $<'%(prerun_build_str, dfc_str, dfc_flags_str), t=True)
                self.write(f, '')

            self.write(f, '%s: %s' % (self.obj(KGUTIL), KGUTIL))
            self.write(f, '%s${%s} ${%s} -c -o $@ $<'%(prerun_build_str, fc_str, fc_flags_str), t=True)
            self.write(f, '')

            self.write(f, '%s: %s' % (self.obj(TPROF), TPROF))
            self.write(f, '%s${%s} ${%s} -c -o $@ $<'%(prerun_build_str, fc_str, fc_flags_str), t=True)
            self.write(f, '')

            self.write(f, 'clean:')
            self.write(f, 'rm -f kernel.exe *.mod ${ALL_OBJS}', t=True)

    def generate_state_makefile(self):

        org_files = [ filepath for filepath, (kfile, sfile, mods_used, units_used) in Config.used_srcfiles.items() if sfile.kgen_stmt.used4genstate ]
        if not Config.topblock['stmt'].reader.id in org_files:
            org_files.append(Config.topblock['filepath'])

        with open('%s/%s/Makefile'%(Config.path['outdir'], Config.path['state']), 'wb') as f:

            self.write(f, '# Makefile for KGEN-generated instrumentation')
            self.write(f, '')

            cwd = os.path.realpath(Config.cwd)

            prerun_build_str = ''
            if Config.prerun['build']:
                self.write(f, 'PRERUN_BUILD := %s'%Config.prerun['build'])
                prerun_build_str = '${PRERUN_BUILD}; '

            prerun_run_str = ''
            if Config.prerun['run']:
                self.write(f, 'PRERUN_RUN := %s'%Config.prerun['run'])
                prerun_run_str = '${PRERUN_RUN}; '

            self.write(f, '')

            if Config.cmd_run['cmds']>0:
                self.write(f, 'run: build')
                self.write(f, '%scd %s; %s'%(prerun_run_str, cwd, Config.cmd_run['cmds']), t=True)
            else:
                self.write(f, 'echo "No information is provided to run. Please specify run commands using \'state-run\' command line option"; exit -1', t=True)
            self.write(f, '')

            if Config.cmd_build['cmds']>0:
                self.write(f, 'build: %s'%Config.state_switch['type'])
                self.write(f, '%scd %s; %s'%(prerun_build_str, cwd, Config.cmd_build['cmds']), t=True)
                for org_file in org_files:
                    self.write(f, 'mv -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
            else:
                self.write(f, 'echo "No information is provided to build. Please specify build commands using \'state-build\' command line option"; exit -1', t=True)
            self.write(f, '')

            self.write(f, '%s: save'%Config.state_switch['type'])
            if Config.state_switch['type']=='replace':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'rm -f %s'%org_file, t=True)
                    self.write(f, 'cp -f %(f1)s %(f2)s'%{'f1':basename, 'f2':org_file}, t=True)
            elif Config.state_switch['type']=='copy':
                for org_file in org_files:
                    basename = os.path.basename(org_file)
                    self.write(f, 'rm -f %s/%s'%(Config.state_switch['directory'], basename), t=True)
                    self.write(f, 'cp -f %s/%s/%s %s'%(Config.path['outdir'], Config.path['state'], \
                        basename, Config.state_switch['directory']), t=True)
            self.write(f, '')

            self.write(f, 'recover:')
            for org_file in org_files:
                self.write(f, 'rm -f %s'%org_file, t=True)
                self.write(f, 'cp -f %s.kgen_org %s'%(os.path.basename(org_file), org_file), t=True)
            self.write(f, '')

            self.write(f, 'recover_from_srcdir:')
            for org_file in org_files:
                self.write(f, 'rm -f %s'%org_file, t=True)
                self.write(f, 'cp -f %(f)s.kgen_org %(f)s'%{'f':org_file}, t=True)
            self.write(f, '')

            self.write(f, 'save:')
            for org_file in org_files:
                self.write(f, 'if [ ! -f %(f)s.kgen_org ]; then cp -f %(f)s %(f)s.kgen_org; fi'%{'f':org_file}, t=True)
                self.write(f, 'if [ ! -f %(g)s.kgen_org ]; then cp -f %(f)s %(g)s.kgen_org; fi'%{'f':org_file, 'g':os.path.basename(org_file)}, t=True)
            self.write(f, '')

            if Config.cmd_clean['cmds']>0:
                self.write(f, 'clean:')
                self.write(f, '%s'%(Config.cmd_clean['cmds']), t=True)
            self.write(f, '')

    def generate_kgen_utils(self):

        with open('%s/%s/%s'%(Config.path['outdir'], Config.path['kernel'], KGUTIL), 'wb') as f:
            f.write('MODULE kgen_utils_mod')
            f.write(kgen_utils_file_head)
            f.write('\n')
            f.write('CONTAINS')
            f.write('\n')
            f.write(kgen_utils_array_sumcheck)
            f.write(kgen_utils_file_tostr)
            f.write(kgen_utils_file_checksubr)
            f.write(kgen_get_newunit)
            f.write(kgen_error_stop)
            f.write(kgen_rankthread)
            f.write('END MODULE kgen_utils_mod\n')
