# kgen_genkernel.py
#

def generate_kernel():
    """Generate kernel source files."""
    import os
    from kgen_utils import Config
    from kgen_state import State
    from block_statements import Program, Module

    # create kernel directories
    if not os.path.exists(Config.path['kernel']):
        os.makedirs(Config.path['kernel'])

    if isinstance(State.topblock['stmt'], Program):
        Logger.major("Callsite statement can not be in Program unit.", stdout=True)
    elif isinstance(State.topblock['stmt'], Module):
        generate_kernel_module()
    else:
        raise ProgramException('Unknown top module type: %s' % State.topblock['stmt'].__class__)

    #generate_kernel_module_files()

    State.state = State.KERNEL_GENERATED

def generate_kernel_module():
    """Generate kernel source file for a module of callsite."""
