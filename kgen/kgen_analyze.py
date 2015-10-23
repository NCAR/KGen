# kgen_analyze.py

from kgen_utils import KGName, Logger, Config, ProgramException, UserException, show_tree, KGGenType
from kgen_state import State, SrcFile
from Fortran2003 import Name

class ActualArg(object):
    def __init__(self, arg):
        from Fortran2003 import Name, Actual_Arg_Spec
        self.kgnames = []
        self.is_keyword = False
        self.arg_spec = Actual_Arg_Spec(arg)
        self.collect_kgnames(self.arg_spec, False)

    def collect_kgnames(self, node, part_collected=False):

        clsname = node.__class__.__name__
        if clsname=='Name':
            self.kgnames.append(KGName(node.string.lower()))
            return

        try:
            if clsname.endswith('_List'):
                for item in node.items:
                    if item is None: continue
                    itemclsname = item.__class__.__name__
                    if itemclsname=='Name':
                        self.kgnames.append(KGName(item.string.lower()))
                    else:
                        exec('self.kgname_%s(item, part_collected)' % itemclsname)
            elif clsname.startswith('End_'):
                pass
            else:
                exec('self.kgname_%s(node, part_collected)' % clsname)
        except Exception as e:
            errname = clsname
            if itemclsname:
                errname = itemclsname
            errmsg = "Error: Fortran specification of %s at callsite is not supported yet."%errname

            Logger.exception(errmsg, node=node)

            if Config.search['promote_exception']:
                raise
            else:
                print ''
                print errmsg
                print ''
                print "'kgen.log' in output folder contains detail information of this error."
                print "If you send the log file to 'kgen@ucar.edu', that could be very"
                print "helpful for us to support this Fortran spec. in future KGEN version."
                print ''
                import sys
                sys.exit(-1)


    def kgname_NoneType(self, node, part_collected=False):
        pass
 
    def kgname_Logical_Literal_Constant(self, node, part_collected=False):
        pass
    
    def kgname_Int_Literal_Constant(self, node, part_collected=False):
        pass

    def kgname_str(self, node, part_collected=False):
        pass

    def kgname_Add_Operand(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[2])

    def kgname_Level_2_Expr(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[2])

    def kgname_Or_Operand(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[2])

    def kgname_Part_Ref(self, node, part_collected=False):
        if not part_collected:
            self.collect_kgnames(node.items[0], True)
            part_collected = True
        self.collect_kgnames(node.items[1], part_collected=part_collected)

    def kgname_Subscript_Triplet(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[1])
        self.collect_kgnames(node.items[2])

    def kgname_Data_Ref(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        for item in node.items[1:]:
            if not isinstance(item, Name):
                self.collect_kgnames(item, part_collected=True)

    def kgname_Proc_Component_Ref(self, node, part_collected=False):
        if not part_collected:
            self.collect_kgnames(node.items[0], True)
            part_collected = True
        self.collect_kgnames(node.items[2], part_collected=part_collected)

    def kgname_Actual_Arg_Spec(self, node, part_collected=False):
        self.collect_kgnames(node.items[1])
        self.is_keyword = True
        self.keyword = node.items[0].string.lower()

    def kgname_Array_Section(self, node, part_collected=False):
        for item in node.items:
            self.collect_kgnames(item)

    def kgname_Substring_Range(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[1])

    def kgname_Level_4_Expr(self, node, part_collected=False):
        self.collect_kgnames(node.items[0])
        self.collect_kgnames(node.items[1])
        self.collect_kgnames(node.items[2])

    def get_kgnames(self):
        return self.kgnames

    def has_firstpartname(self, fpn):
        for kgname in self.kgnames:
            if isinstance(fpn, str):
                if fpn==kgname.firstpartname():
                    return True
            elif isinstance(fpn, KGName):
                if fpn.firstpartname()==kgname.firstpartname():
                    return True
        return False

class ActualArgList(object):
    def __init__(self):
        self.arglist = []

    def get_arg(self, idx):
        return self.arglist[idx]

    def add_arg(self, arg):
        self.arglist.append(arg)
 
    def has_firstpartname(self, fpn):
        for arg in self.arglist:
            if arg.has_firstpartname(fpn):
                return True
        return False

    def index_firstpartname(self, fpn):
        for i, arg in enumerate(self.arglist):
            if arg.has_firstpartname(fpn):
                return i
        raise ProgramException('Out of index')

    def sort(self):
        self.arglist.sort()
   
def collect_intent_names(var, kgname, param):
    if var.is_intent_in():
        param['in_names'].append(kgname)
    elif var.is_intent_out():
        param['out_names'].append(kgname)
    elif var.is_intent_inout():
        param['inout_names'].append(kgname)

def collect_args_from_subpstmt(stmt, param):
    import sys
    from block_statements import Interface

    if isinstance(stmt, Interface):
        errmsg = 'Current version of KGEN does not extract a kernel from a Fortran Interface yet.'
        Logger.critical(errmsg+'\nIn %s\n'%stmt.reader.id+str(stmt))
        sys.exit(-1)

    for arg in stmt.args:
        kgname = KGName(arg)
        param['names'].append(kgname)
        var = stmt.a.variables[kgname.firstpartname()]
        param['typedecl_stmt'][kgname] = var.parent
        collect_intent_names(var, kgname, param)

def collect_args_from_expr(expr, param, stmt):
    from typedecl_statements import TypeDeclarationStatement

    unames = [ n.firstpartname() for n in stmt.unknowns.keys() ]    
    res_stmts = [ v.res_stmts[-1] for v in stmt.unknowns.values() ]    

    # if there is no actual argument
    if expr.items[1] is None:
        pass
    # if there is one actual argument
    elif isinstance(expr.items[1], Name):
        actual_arg = ActualArg(expr.items[1].string.lower())
        param['names'] = ActualArgList()
        param['names'].add_arg(actual_arg)
        for kgname in actual_arg.get_kgnames():
            if kgname.firstpartname() in unames:
                idx = unames.index(kgname.firstpartname())
                param['typedecl_stmt'][kgname] = res_stmts[idx]
                if isinstance(param['typedecl_stmt'][kgname], TypeDeclarationStatement):
                    var = param['typedecl_stmt'][kgname].parent.a.variables[kgname.firstpartname()]
                    collect_intent_names(var, kgname, param)
    # if there are multiple actual arguments
    elif hasattr(expr.items[1], 'items'):
        param['names'] = ActualArgList()
        for item in expr.items[1].items:
            if isinstance(item, str): continue
            actual_arg = ActualArg(item.string.lower())
            param['names'].add_arg(actual_arg)
            for kgname in actual_arg.get_kgnames():
                if kgname.firstpartname() in unames:
                    idx = unames.index(kgname.firstpartname())
                    param['typedecl_stmt'][kgname] = res_stmts[idx]
                    if isinstance(param['typedecl_stmt'][kgname], TypeDeclarationStatement):
                        var = param['typedecl_stmt'][kgname].parent.a.variables[kgname.firstpartname()]
                        collect_intent_names(var, kgname, param)

def locate_callsite():
    from block_statements import Module, action_stmt
    from statements import Assignment, Call

    # read source file that contains callsite stmt
    cs_file = SrcFile(Config.callsite['filename'])

    #[Call, Assignment]
    State.callsite['stmt'], State.callsite['expr'] = cs_file.stmt_by_name(Config.callsite['subpname'], cls=action_stmt, \
        lineafter=Config.callsite['lineafter'])
    if State.callsite['stmt'] is None or State.callsite['expr'] is None:
        raise UserException('Subprogram %s is not found.' % Config.callsite['subpname'].list())

    # ancestors of callsite stmt
    anc = State.callsite['stmt'].ancestors()

    # TODO: support for Program block
    if not isinstance(anc[0], Module):
        raise UserException('Only module block is allowed as a top block in call-site source file.')

    # populate parent block parameters
    State.parentblock['stmt'] = anc[-1]
    State.parentblock['expr'] = State.parentblock['stmt'].f2003
    collect_args_from_subpstmt(State.parentblock['stmt'], State.parentblock['dummy_arg'])

    # populate top block parameters
    State.topblock['file'] = cs_file
    State.topblock['path'] = cs_file.abspath
    State.topblock['stmt'] = anc[0]
    State.topblock['expr'] = State.topblock['stmt'].f2003

    # test suite
    if Config.test['suite']:
        if Config.test['suite']=='1':
            assert cs_stmt.items[0]=='nit', 'Test suite 1: wrong callsite stmt'
            assert cs_expr.items[0].string=='grad_dg', 'Test suite 1: wrong callsite expr'

    State.state = State.CALLSITE_LOCATED

def collect_kernel_info():
    from kgen_search import f2003_search_unknowns
    from base_classes import EndStatement
    from block_statements import SubProgramStatement, Subroutine, Function, Interface, Type, TypeDecl
    from statements import Use, Assignment
    from Fortran2003 import Call_Stmt, Part_Ref, Procedure_Designator, Name, Assignment_Stmt
    from kgen_state import ResState

    # mark callsite, and its ancestors
    anc_callsite = State.callsite['stmt'].ancestors(include_beginsource=True)
    for anc in anc_callsite:
        anc.geninfo = {}
        anc.geninfo[KGGenType.KERNEL] = []
        if isinstance(anc.content[-1], EndStatement):
            anc.content[-1].geninfo = {}
            anc.content[-1].geninfo[KGGenType.KERNEL] = []

    # resolve kernel subprogram and save arguments matching
    if isinstance(State.callsite['expr'], Call_Stmt):
        f2003_search_unknowns(KGGenType.KERNEL, State.callsite['stmt'], State.callsite['expr'].items[0], [ Subroutine, Interface ])
    elif isinstance(State.callsite['expr'], Part_Ref):
        f2003_search_unknowns(KGGenType.KERNEL, State.callsite['stmt'], State.callsite['expr'].items[0], [ Function, Interface ])
    elif isinstance(State.callsite['expr'], Procedure_Designator):
        f2003_search_unknowns(KGGenType.KERNEL, State.callsite['stmt'], State.callsite['expr'].items[0], [ Type, TypeDecl])
    else:
        raise ProgramException('Unknown expr type is found: %s' % State.callsite['expr'].__class__)
    for unknown, request in State.callsite['stmt'].unknowns.iteritems():
        if request.state != ResState.RESOLVED:
            State.callsite['stmt'].resolve(request)

    # populate kernel parameters
    if len(State.callsite['stmt'].unknowns)==1:

        State.kernel['stmt'] = State.callsite['stmt'].unknowns.values()[0].res_stmts[0]
        if isinstance(State.kernel['stmt'], SubProgramStatement):
            pass
        elif isinstance(State.kernel['stmt'], Use):
            kname = str(State.callsite['expr'].items[0])
            for newname, oldname in State.kernel['stmt'].renames:
                if kname==newname:
                    kname = oldname
                    break
            module = State.kernel['stmt'].module
            if module.a.module_provides.has_key(kname):
                State.kernel['stmt'] = module.a.module_provides[kname]
            elif module.a.module_interface.has_key(kname):
                State.kernel['stmt'] = module.a.module_interface[kname]
            else:
                raise ProgramException('Can not find %s in module %s at %s'%(kname, module.name, module.reader.id))
        else: raise ProgramException('Unknown res_stmt: %s'%State.kernel['stmt'].__class__)

        anc = State.kernel['stmt'].ancestors()

        State.kernel['expr'] = State.kernel['stmt'].f2003
        State.kernel['parent'] = anc[-1]
        State.kernel['top'] = anc[0]

        collect_args_from_subpstmt(State.kernel['stmt'], State.kernel['dummy_arg'])
    else:
        raise ProgramException('More than one unknown at callsite stmt')

    # resolve actual arguments
    State.callsite['stmt'].top.geninfo[KGGenType.STATE] = []
    f2003_search_unknowns(KGGenType.STATE, State.callsite['stmt'], State.callsite['expr'].items[1])
    for unknown, request in State.callsite['stmt'].unknowns.iteritems():
        if request.state != ResState.RESOLVED:
            State.callsite['stmt'].resolve(request)

    # resolve lhs of assignment stmt
#    if isinstance(State.callsite['stmt'], Assignment):
#        f2003_search_unknowns(State.callsite['stmt'], State.callsite['stmt'].f2003.items[0])
#        for unknown, request in State.callsite['stmt'].unknowns.iteritems():
#            if request.state != ResState.RESOLVED:
#                State.callsite['stmt'].resolve(request)

    expr = State.callsite['expr']
    if isinstance(expr, Part_Ref):
        if hasattr(expr, 'parent') and isinstance(expr.parent, Assignment_Stmt):
            f2003_search_unknowns(KGGenType.STATE, State.callsite['stmt'], expr.parent.items[0])
            for unknown, request in State.callsite['stmt'].unknowns.iteritems():
                if request.state != ResState.RESOLVED:
                    State.callsite['stmt'].resolve(request)
        else:
            raise ProgramException("Only assignment statement is allowed for Function callsite yet.")




    # populate callsite parameters
    collect_args_from_expr(State.callsite['expr'], State.callsite['actual_arg'], State.callsite['stmt'])

    State.state = State.KERNELINFO_COLLECTED

import unittest
class Test_kgen_callsite(unittest.TestCase):

    def setUp(self):
        locate_callsite()

    def test_located(self):
        #self.assertIsNotNone(State.callsite['file'])  # self.assertIsNotNone is available from Python 2.7
        #self.assertIsNotNone(State.callsite['stmt']) 
        #self.assertIsNotNone(State.callsite['expr']) 
        self.assertTrue(State.callsite['file'] is not None) 
        self.assertTrue(State.callsite['stmt'] is not None) 
        self.assertTrue(State.callsite['expr'] is not None) 

        self.assertTrue(len(State.callsite['file'].prep) > 0)

if __name__ == "__main__":
    import sys
    #unittest.main(argv=[sys.argv[0]], verbosity=2) # verbosity is available from Python 2.7
    unittest.main(argv=[sys.argv[0]])
