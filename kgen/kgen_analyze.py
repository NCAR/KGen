# kgen_analyze.py


from kgen_utils import KGName, Logger, Config, ProgramException, UserException, show_tree, KGGenType, \
    match_namepath, pack_exnamepath, traverse
from kgen_state import State, SrcFile, ResState
from Fortran2003 import Name, Call_Stmt, Function_Reference, Part_Ref
from ordereddict import OrderedDict
from typedecl_statements import TypeDeclarationStatement
from api import walk

def update_state_info(parent):

    def get_nodes(node, bag, depth):
        from Fortran2003 import Name
        if isinstance(node, Name) and node.string==bag['name'] and not node.parent in bag:
            anc = []
            while hasattr(node, 'parent'):
                anc.append(node.parent)
                node = node.parent
            bag['node'].append(anc)

    if hasattr(parent, 'content'):
        for stmt in parent.content:
            if isinstance(stmt, TypeDeclarationStatement) and \
                "parameter" not in stmt.attrspec and hasattr(stmt, 'geninfo') and len(stmt.geninfo)>0 :
                for uname, req in KGGenType.get_state_in(stmt.geninfo):
                    org = req.originator
                    if org in State.callsite['stmts']:
                        bag = {'name': uname.firstpartname(), 'node': [] }
                        traverse(org.f2003, get_nodes, bag)
                        for used in bag['node']:
                            # return if found state out case
                            for anc in used:
                                if anc.__class__ in [ Call_Stmt, Function_Reference, Part_Ref ]:
                                    
                            # check if uname is in actual argument
                        
                            import pdb; pdb.set_trace()

#                res_stmt = req.res_stmts[0]
#                if res_stmt.__class__ in [Subroutine, Function, Interface, SpecificBinding ]:
#                    # find actual args
#                    
#                    # find dummy args
#
#                    # update state of actual args according to intent of dummy args
#                    import pdb; pdb.set_trace()
#                    pass
#                    #update state in/out for arguments
#                    # if in/out can not be decided, use inout
    if hasattr(parent, 'parent') and not parent.parent in [ Module, Program ]:
        update_state_info(parent.parent)


def analyze():
    from block_statements import EndStatement, Subroutine, Function, Interface
    from statements import SpecificBinding
    from kgen_search import f2003_search_unknowns

    # read source file that contains callsite stmt
    cs_file = SrcFile(Config.callsite['filepath'])

    locate_callsite(cs_file.tree)

    # ancestors of callsite stmt
    ancs = State.callsite['stmts'][0].ancestors()

    # add geninfo for ancestors
    prevstmt = State.callsite['stmts'][0]
    prevname = None

    for anc in reversed(ancs):
        if not hasattr(anc, 'geninfo'):
            anc.geninfo = OrderedDict()
        if len(anc.content)>0 and isinstance(anc.content[-1], EndStatement) and \
            not hasattr(anc.content[-1], 'geninfo'):
            anc.content[-1].geninfo = OrderedDict()

        if prevname:
            dummy_req = ResState(KGGenType.STATE_IN, KGName(prevname), None, [anc])
            dummy_req.res_stmts = [ prevstmt ]
            anc.check_spec_stmts(dummy_req.uname, dummy_req)

        if hasattr(anc, 'name'): prevname = anc.name
        else: prevname = None
        prevstmt = anc

    # populate parent block parameters
    State.parentblock['stmt'] = ancs[-1]
    #State.parentblock['expr'] = State.parentblock['stmt'].f2003
    #collect_args_from_subpstmt(State.parentblock['stmt'], State.parentblock['dummy_arg'])

    # populate top block parameters
    #State.topblock['file'] = cs_file
    #State.topblock['path'] = cs_file.abspath
    State.topblock['stmt'] = ancs[0]
    #State.topblock['expr'] = State.topblock['stmt'].f2003

    for cs_stmt in State.callsite['stmts']:
        #resolve cs_stmt
        f2003_search_unknowns(cs_stmt, cs_stmt.f2003)
        for uname, req in cs_stmt.unknowns.iteritems():
            cs_stmt.resolve(req)
            if not req.res_stmts:
                raise ProgramException('Resolution fail.')


    update_state_info(State.parentblock['stmt'])

def locate_callsite(cs_tree):
    from statements import Comment
    from block_statements import executable_construct
    import re

    def get_next_non_comment(stmt):
        if not stmt: return
        if not hasattr(stmt, 'parent'): return

        started = False
        for s in stmt.parent.content:
            if s==stmt:
                if not isinstance(s, Comment): return s
                started = True
            elif started:
                if not isinstance(s, Comment): return s

    def get_names(node, bag, depth):
        from Fortran2003 import Name
        if isinstance(node, Name) and not node.string in bag:
            bag.append(node.string)
       
    # collect directives
    directs = []
    for stmt, depth in walk(cs_tree):
        if isinstance(stmt, Comment):
            line = stmt.item.comment.strip()
            match = re.match(r'^[c!*]\$kgen\s+(.+)$', line, re.IGNORECASE)
            if match:
                directs.append( (match.group(1).strip(), stmt))
        elif Config.callsite['namepath'] and stmt.__class__ in executable_construct:
            names = []
            traverse(stmt.f2003, get_names, names)
            for name in names:
                if match_namepath(Config.callsite['namepath'], pack_exnamepath(stmt, name), internal=False):
                    directs.append( ('callsite %s'%name, stmt))
                    break

    for direct, stmt in directs:
        match = re.match(r'^(\w[\w\d]*)\s+(.+)$', direct, re.IGNORECASE)
        if match:
            dname = match.group(1).lower()
            clause = match.group(2)
            if dname.startswith('begin_'):
                pass
                # TODO: block kernel extraction
            else:
                if dname=='callsite':
                    if isinstance(stmt, Comment):
                        next_fort_stmt = get_next_non_comment(stmt)
                        if next_fort_stmt:
                            State.kernel['name'] = clause
                            State.callsite['stmts'].append(get_next_non_comment(stmt))
                            break
                        else:
                            raise UserException('WARNING: callsite is not found')
                    else:
                        State.kernel['name'] = clause
                        State.callsite['stmts'].append(stmt)
                        break
                else:
                    raise UserException('WARNING: Not supported KGEN directive: %s'%direct)


    if len(State.callsite['stmts'])==0: raise UserException('Can not find callsite')
