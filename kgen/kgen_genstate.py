# kgen_genstate.py
#

def generate_state():
    """Generate instrumentation source files."""
    import os
    from kgen_utils import Config
    from kgen_state import State
    from block_statements import Program, Module

    # create state and kernel directories
    if not os.path.exists(Config.path['state']):
        os.makedirs(Config.path['state'])

    if isinstance(State.topblock['stmt'], Program):
        Logger.major("Callsite statement can not be in Program unit.", stdout=True)
    elif isinstance(State.topblock['stmt'], Module):
        generate_callsite_module()
    else:
        raise ProgramException('Unknown parent type: %s' % State.topblock['stmt'].__class__)

    # generate instrumented files except callsite file
    #generate_state_files()

    State.state = State.STATE_GENERATED

def generate_callsite_module():
    """Generate instrumentation source file for a module of callsite."""
    

