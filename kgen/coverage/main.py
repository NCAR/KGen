'''KGen coverage detector
'''

import os
from kgtool import KGTool
from parser.kgparse import KGGenType
from kggenfile import gensobj, KERNEL_ID_0, event_register
import kgutils
from kgconfig import Config

class Coverage(KGTool):

    def run(self):

        self.genfiles = []

        kgutils.logger.info('Starting KCover')

        # create coverage directory
        if not os.path.exists(Config.path['coverage']):
            os.makedirs(Config.path['coverage'])

        # build app with instrumntation
        if not os.path.exists(Config.coveragefile) or 'all' in Config.rebuild or 'coverage' in Config.rebuild:

            # generate instrumentation
            for filepath, (srcobj, mods_used, units_used) in Config.srcfiles.iteritems():
                if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):
                    sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
                    if sfile is None:
                        raise kgutils.ProgramException('Kernel source file is not generated for %s.'%filepath)
                    self.genfiles.append((sfile, filepath))
                    Config.used_srcfiles[filepath] = (srcobj, mods_used, units_used)

            # process each nodes in the tree
            for plugin_name in event_register.keys():
                if not plugin_name.startswith('cover'): continue

                for sfile, filepath in self.genfiles:
                    sfile.created([plugin_name])
                for tree in self._trees:
                    tree.created([plugin_name])

                for sfile, filepath in self.genfiles:
                    sfile.process([plugin_name])
                for tree in self._trees:
                    tree.process([plugin_name])

                for sfile, filepath in self.genfiles:
                    sfile.finalize([plugin_name])
                for tree in self._trees:
                    tree.finalize([plugin_name])

                for sfile, filepath in self.genfiles:
                    sfile.flatten(KERNEL_ID_0, [plugin_name])
                for tree in self._trees:
                    tree.flatten(KERNEL_ID_0, [plugin_name])

            # clean app
            #if Config.cmd_clean['cmds']:
            #    kgutils.run_shcmd(Config.cmd_clean['cmds'])

            #bld_cmd = 'strace -o strace.log -f -q -s 100000 -e trace=execve -v -- %s/_kgen_compflag_cmdwrapper.sh'%Config.cwd
            #kgutils.logger.info('Creating KGen strace logfile: %s'%self.straceoutfile)
            kgutils.run_shcmd(bld_cmd)
        else:
            kgutils.logger.info('Reusing KGen coverage file: %s'%Config.coveragefile)

        # run app
        if Config.cmd_run['cmds']:
            kgutils.run_shcmd(Config.cmd_run['cmds'])

        # save info to cfg

        pass
