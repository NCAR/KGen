from kgen_genfile import KERNEL_ID_0

def generate_srcfiles():
    """Generate files."""

    # setup plugin framework
    init_plugins([KERNEL_ID_0])

    # generate kgen_driver.f90 in kernel directory
    driver = genkobj(None, block_statements.BeginSource, KERNEL_ID_0)
    program = genkobj(driver, block_statements.Program, KERNEL_ID_0)
    program.name = getinfo('kernel_driver_name')
    append_item_in_part(driver, UNIT_PART, program)

    # construct a generation tree
    genfiles = []
    for filepath, (srcobj, mods_used, units_used) in State.srcfiles.iteritems():
        if hasattr(srcobj.tree, 'geninfo') and KGGenType.has_state(srcobj.tree.geninfo):

            kfile = genkobj(None, srcobj.tree, KERNEL_ID_0)
            sfile = gensobj(None, srcobj.tree, KERNEL_ID_0)
            if kfile is None or sfile is None:
                raise ProgramException('Kernel source file is not generated for %s.'%filepath)
            genfiles.append((kfile, sfile, filepath))
            State.used_srcfiles[filepath] = (srcobj, mods_used, units_used)

    # process each nodes in the tree
    for plugin_name in event_register.keys():
        for kfile, sfile, filepath in genfiles:
            kfile.created([plugin_name])
            sfile.created([plugin_name])
        driver.created([plugin_name])

        for kfile, sfile, filepath in genfiles:
            kfile.process([plugin_name])
            sfile.process([plugin_name])
        driver.process([plugin_name])

        for kfile, sfile, filepath in genfiles:
            kfile.finalize([plugin_name])
            sfile.finalize([plugin_name])
        driver.finalize([plugin_name])

        for kfile, sfile, filepath in genfiles:
            kfile.flatten(KERNEL_ID_0, [plugin_name])
            sfile.flatten(KERNEL_ID_0, [plugin_name])
        driver.flatten(KERNEL_ID_0, [plugin_name])
        #named_part = OrderedDict()

    # generate source files from each node of the tree
    for kfile, sfile, filepath in genfiles:
        filename = os.path.basename(filepath)
        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
        klines = kfile.tostring()
        if klines is not None:
            with open('%s/%s'%(Config.path['kernel'], filename), 'wb') as fd:
                fd.write(klines)

        if sfile.kgen_stmt.used4genstate:
            Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
            slines = sfile.tostring()
            if slines is not None:
                with open('%s/%s'%(Config.path['state'], filename), 'wb') as fd:
                    fd.write(slines)

    with open('%s/%s.f90'%(Config.path['kernel'], getinfo('kernel_driver_name')), 'wb') as fd:
        Gen_Statement.kgen_gen_attrs = {'indent': '', 'span': None}
        lines = driver.tostring()
        if lines is not None: fd.write(lines)

    # generate kgen_utils.f90 in kernel directory
    generate_kgen_utils(KERNEL_ID_0)

    State.state = State.STATE_GENERATED
