# kgen_analyze.py


from kgen_utils import KGName, Logger, Config, ProgramException, UserException, KGGenType, traverse
from kgen_state import State, SrcFile, ResState
from Fortran2003 import Name, Call_Stmt, Function_Reference, Part_Ref, Interface_Stmt, Actual_Arg_Spec_List, \
    Section_Subscript_List, Actual_Arg_Spec, Structure_Constructor_2
from ordereddict import OrderedDict
from typedecl_statements import TypeDeclarationStatement
from block_statements import SubProgramStatement, Associate

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
                "parameter" not in stmt.attrspec and hasattr(stmt, 'geninfo') and \
                any(len(v)>0 for v in stmt.geninfo.values()):
                for uname, req in KGGenType.get_state_in(stmt.geninfo):
                    if KGGenType.has_uname_out(uname, stmt.geninfo): continue
                    # select names for searching
                    respairs = []
                    if req.originator in State.callsite['stmts']:
                        respairs.append((uname, req.originator))
                    elif isinstance(req.originator, Associate):
                        if uname in req.originator.assoc_map:
                            for auname in req.originator.assoc_map[uname]:
                                for aruname, arreq in KGGenType.get_state_in(req.originator.geninfo):
                                    if auname == aruname:
                                        respairs.append((auname, arreq.originator))

                    if len(respairs) > 0:
                        copied = False
                        for varname, org in respairs:
                            bag = {'name': varname.firstpartname(), 'lineage': [] }
                            traverse(org.f2003, get_nodes, bag)
                            for lineage in bag['lineage']:
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
                            if copied: break

    if hasattr(parent, 'parent'):
        update_state_info(parent.parent)


def analyze():

    analyze_callsite()

def analyze_callsite():
    from block_statements import EndStatement, Subroutine, Function, Interface
    from statements import SpecificBinding
    from kgen_search import f2003_search_unknowns

    # read source file that contains callsite stmt
    cs_file = SrcFile(Config.callsite['filepath'])

    #process_directive(cs_file.tree)

    if len(State.callsite['stmts'])==0:
        raise UserException('Can not find callsite')

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

    # populate top block parameters
    State.topblock['stmt'] = ancs[0]

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
