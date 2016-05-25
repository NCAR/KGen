''' KGen Application '''

import sys
import os
import glob
import optparse
import subprocess

KGEN_APP = os.path.dirname(os.path.realpath(__file__))
KGEN_HOME = '%s/../..'%KGEN_APP
KEXT_TOOL = '%s/kgenapps/kernel_extractor'%KGEN_HOME
COMPFLAG_TOOL = '%s/kgenapps/compflag_detector'%KGEN_HOME

sys.path.insert(0, '%s/base'%KGEN_HOME)
sys.path.insert(0, COMPFLAG_TOOL)
sys.path.insert(0, KEXT_TOOL)
sys.path.insert(0, KGEN_APP)

from kgen_utils import UserException, ProgramException, Logger, Config, run_shcmd
from kgen_state import State

def pack_arg(opt):
    if opt is None: return ''
    if isinstance(opt, str):
        opt = [ opt ]

    args = []
    for o in opt:
        if o is None: args.append('')
        elif isinstance(o, str):
            args.append(o)
#            if o.find('"')>=0:
#                args.append(o)
#            else:
#                args.append('"%s"'%o)
        else:
            args.append(str(o))

    return args

def main():
    from compflag_tool import CompFlagDetect
    from kext_tool import KExtTool

    version = [ 0, 0, '0' ]
    outdir = '.'
    retval = 0

    try:
        # option parser
        parser = optparse.OptionParser(version='KGEN version %d.%d.%s'%tuple(version))

        # common options
        parser.add_option("--outdir", dest="outdir", action='store', type='string', default=None, help="path to create outputs")
        parser.add_option("--rebuild", dest="rebuild", action='append', type='string', default=None, help="force to rebuild")

        # compflag options
        parser.add_option("--strace", dest="strace", action='append', type='string', default=None, help="strace options")
        parser.add_option("--ini", dest="ini", action='append', type='string', default=None, help="INI options")

        # kext options
        parser.add_option("--invocation", dest="invocation", action='append', type='string', default=None, help="(process, thread, invocation) pairs of kernel for data collection")
        parser.add_option("--exclude", dest="exclude", action='store', type='string', default=None, help="information excluded for analysis")
        parser.add_option("--openmp", dest="openmp", action='append', type='string', default=None, help="Specifying OpenMP options")
        parser.add_option("--mpi", dest="mpi", action='append', type='string', default=None, help="MPI information for data collection")
        parser.add_option("--timing", dest="timing", action='append', type='string', default=None, help="Timing measurement information")
        parser.add_option("--intrinsic", dest="intrinsic", action='append', type='string', default=None, help="Specifying resolution for intrinsic procedures during searching")
        parser.add_option("--prerun", dest="prerun", action='append', type='string', default=None, help="prerun commands")

        opts, args = parser.parse_args()

        if len(args)<4:
            print 'ERROR: At least four arguments are required.'
            print 'Usage: kgen <target file path[:namepath]> <target initialize commands> <target build commands> <target run commands>'
            sys.exit(-1)

        kext_argv = []
        compflag_argv = []

        # collect common options
        if opts.outdir:
            kext_argv.append('--outdir')
            kext_argv.append(opts.outdir)
            compflag_argv.append('--build')
            compflag_argv.append('cwd=%s'%opts.outdir)
            outdir = opts.outdir

        # collect compflag options
        if opts.strace:
            compflag_argv.append('--strace')
            compflag_argv.extend(pack_arg(opts.strace))
        if opts.ini:
            compflag_argv.append('--ini')
            compflag_argv.extend(pack_arg(opts.ini))
        if opts.rebuild:
            compflag_argv.append('--rebuild')
            compflag_argv.extend(pack_arg(opts.rebuild))
        compflag_argv.extend(pack_arg(args[1]))
        compflag_argv.extend(pack_arg(args[2]))

        # collect kext options
        if opts.invocation:
            kext_argv.append('--invocation')
            kext_argv.extend(opts.invocation)
        if opts.exclude:
            kext_argv.append('--exclude-ini')
            kext_argv.append(opts.exclude)
        if opts.mpi:
            kext_argv.append('--mpi')
            kext_argv.extend(opts.mpi)
        if opts.openmp:
            kext_argv.append('--openmp')
            kext_argv.extend(opts.openmp)
        if opts.timing:
            kext_argv.append('--timing')
            kext_argv.extend(opts.timing)
        if opts.intrinsic:
            kext_argv.append('--intrinsic')
            kext_argv.extend(opts.intrinsic)
        if opts.prerun:
            kext_argv.append('--prerun')
            kext_argv.extend(opts.prerun)
        kext_argv.append('--state-build')
        kext_argv.append('cmds=%s'%args[2])
        kext_argv.append('--state-run')
        kext_argv.append('cmds=%s'%args[3])
        kext_argv.append(args[0])

        # run compflag
        compflag = CompFlagDetect()
        compflag.init(argv=compflag_argv)
        compflag.main()
        flags = compflag.fini()

        # run kext
        kext = KExtTool()
        kext.init()
        kext_argv.extend( [ '-i', flags['incini'] ] )
        Config.apply(argv=kext_argv)
        kext.main()
        extracts = kext.fini()
        # extracts contain kernel files, state files

        # parse rebuild option
        is_rebuild = False
        if opts.rebuild:
            for r in opts.rebuild:
                if isinstance(r, str):
                    subopts = r.split(',')
                    for subopt in subopts:
                        if subopt in [ 'all', 'state' ]:
                            is_rebuild = True
                if is_rebuild: break
                         
        # check if state files exist
        has_statefiles = False
        if os.path.exists('%s/kernel'%outdir):
            statefiles = glob.glob('%s/kernel/%s.*.*.*'%(outdir, State.kernel['name']))
            if len(statefiles)>0:
                has_statefiles = True
                
        # generate state
        if is_rebuild or not has_statefiles:
            print 'Generating state data files.' 
            out, err, retcode = run_shcmd('make', cwd='%s/state'%outdir)

    except UserException as e:
        print 'ERROR: %s'%str(e)
        Logger.info(e)
        #Logger.critical(e)
        retval = -1
    except ProgramException as e:
        Logger.critical(e)
        retval = -1
    except Exception as e:
        Logger.critical(e)
        retval = -1
    finally:
        if os.path.exists('%s/state/Makefile'%outdir):
            out, err, retcode = run_shcmd('make recover_from_locals', cwd='%s/state'%outdir)

    return retval

if __name__ == '__main__':
    main()
