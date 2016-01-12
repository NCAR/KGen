# kgen_analyze.py


from kgen_utils import KGName, Logger, Config, ProgramException, UserException, show_tree, KGGenType, \
    match_namepath, pack_exnamepath, traverse
from kgen_state import State, SrcFile, ResState
from Fortran2003 import Name, Call_Stmt, Function_Reference, Part_Ref, Interface_Stmt, Actual_Arg_Spec_List, \
    Section_Subscript_List, Actual_Arg_Spec, Structure_Constructor_2
from ordereddict import OrderedDict
from typedecl_statements import TypeDeclarationStatement
from block_statements import SubProgramStatement
from api import walk

def update_state_info(parent):

    def get_nodes(node, bag, depth):
        from Fortran2003 import Name
        if isinstance(node, Name) and node.string==bag['name'] and not node.parent in bag:
            anc = [node]
            while hasattr(node, 'parent'):
                anc.insert(0, node.parent)
                node = node.parent
            bag['lineage'].append(anc)

    if hasattr(parent, 'content'):
        for stmt in parent.content:
            if isinstance(stmt, TypeDeclarationStatement) and \
                "parameter" not in stmt.attrspec and hasattr(stmt, 'geninfo') and len(stmt.geninfo)>0 :
                for uname, req in KGGenType.get_state_in(stmt.geninfo):
                    if KGGenType.has_uname_out(uname, stmt.geninfo): continue

                    org = req.originator
                    if org in State.callsite['stmts']:
                        bag = {'name': uname.firstpartname(), 'lineage': [] }
                        traverse(org.f2003, get_nodes, bag)
                        for lineage in bag['lineage']:
                            copied = False
                            for lidx, anc in enumerate(lineage):
                                # get callname
                                callname = None
                                if anc.__class__ in [ Call_Stmt, Function_Reference ]:
                                    callname = anc.items[0].string
                                elif anc.__class__ == Part_Ref:
                                    callname = anc.items[0].string
                                elif anc.__class__ == Interface_Stmt:
                                    callname = anc.items[0].string

                                # get caller and callee objects
                                callobj = None
                                subpobj = None
                                if callname:
                                    for org_uname, org_req in org.unknowns.iteritems():
                                        if org_uname.firstpartname()==callname:
                                            if isinstance(org_req.res_stmts[0], SubProgramStatement):
                                                callobj = anc
                                                subpobj = org_req.res_stmts[0]
                                            break
                                    
                                # get argument index
                                argidx = -1
                                is_keyword = False
                                if callobj and subpobj:
                                    if callobj.__class__ in [ Call_Stmt, Function_Reference ]:
                                        arglist = callobj.items[1]
                                        if arglist is None: pass
                                        elif isinstance(arglist, Actual_Arg_Spec):
                                            argobj = lineage[lidx+1]
                                            kword = argobj.items[0].string
                                            argidx = subpobj.args.index(kword)
                                        elif isinstance(arglist, Actual_Arg_Spec_List):
                                            #if len(lineage)<(lidx+3): import pdb; pdb.set_trace()
                                            argobj = lineage[lidx+2]
                                            argidx = arglist.items.index(argobj)
                                            if isinstance(argobj, Actual_Arg_Spec):
                                                kword = argobj.items[0].string
                                                argidx = subpobj.args.index(kword)
                                        else:
                                            argidx = 0
                                    elif anc.__class__ == Part_Ref:
                                        arglist = callobj.items[1]
                                        if arglist is None: pass
                                        elif isinstance(arglist, Structure_Constructor_2):
                                            argobj = lineage[lidx+1]
                                            kword = argobj.items[0].string
                                            argidx = subpobj.args.index(kword)
                                        elif isinstance(arglist, Section_Subscript_List):
                                            #if len(lineage)<(lidx+3): import pdb; pdb.set_trace()
                                            argobj = lineage[lidx+2]
                                            argidx = arglist.items.index(argobj)
                                            if isinstance(argobj, Structure_Constructor_2):
                                                kword = argobj.items[0].string
                                                argidx = subpobj.args.index(kword)
                                        else:
                                            argidx = 0
                                    elif anc.__class__ == Interface_Stmt:
                                        print 'III'
                                        import pdb; pdb.set_trace()

                                # get intent
                                if argidx>=0:
                                    argname = subpobj.args[argidx]
                                    var = subpobj.a.variables[subpobj.args[argidx]]
                                    if var.is_intent_out() or var.is_intent_inout():
                                        req.gentype = KGGenType.STATE_OUT
                                        stmt.add_geninfo(uname, req)
                                        copied = True
                                        break
                            if copied: break

    if hasattr(parent, 'parent'):
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


    # update state info of callsite and its upper blocks
    update_state_info(State.parentblock['stmt'])

    # update state info of modules
    for modname, moddict in State.modules.iteritems():
        modstmt = moddict['stmt']
        if modstmt != State.topblock['stmt']:
            update_state_info(moddict['stmt'])

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
                dsplit = match.group(1).split(' ', 1)
                dname = dsplit[0].strip()
                if len(dsplit)>1: clause = dsplit[1].strip()
                else: clause = None

                if dname.startswith('begin_'):
                    sname = dname[6:]
                    directs.append(sname)
                    State.kernel['name'] = clause
                elif dname.startswith('end_'):
                    ename = dname[4:]
                    if directs[-1]==ename:
                        directs.pop()
                        if ename=='callsite':
                            pass
                        else:
                            raise UserException('WARNING: Not supported KGEN directive: %s'%ename)
                    else:
                        raise UserException('Directive name mismatch: %s, %s'%(dname_stack[-1], ename))
                elif dname=='callsite':
                    next_fort_stmt = get_next_non_comment(stmt)
                    if next_fort_stmt:
                        State.kernel['name'] = clause
                        State.callsite['stmts'].append(next_fort_stmt)
                    else:
                        raise UserException('WARNING: callsite is not found')
        else:
            if Config.callsite['namepath'] and stmt.__class__ in executable_construct:
                names = []
                traverse(stmt.f2003, get_names, names)
                for name in names:
                    if match_namepath(Config.callsite['namepath'], pack_exnamepath(stmt, name), internal=False):
                        State.kernel['name'] = name
                        for _s, _d in walk(stmt):
                            State.callsite['stmts'].append(_s)
                        return
            elif len(directs)>0 and directs[-1]=='callsite':
                State.callsite['stmts'].append(stmt)

    if len(State.callsite['stmts'])==0: raise UserException('Can not find callsite')
