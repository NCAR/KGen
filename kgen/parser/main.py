'''KGen source code parser
'''

import kgtool
import kgutils
import kgparse
import collections
from kgconfig import Config

class Parser(kgtool.KGTool):

    def run(self):
        from kgsearch import f2003_search_unknowns
        import kganalyze

        # preprocess if required

        # read source file that contains callsite stmt
        cs_file = kgparse.SrcFile(Config.callsite['filepath'])
        if len(Config.callsite['stmts'])==0:
            raise kgutils.UserException('Can not find callsite')


        # add geninfo to ancestors
        ancs = Config.callsite['stmts'][0].ancestors()

        self.add_geninfo_ancestors(Config.callsite['stmts'][0])

        # populate parent block parameters
        Config.parentblock['stmt'] = ancs[-1]

        # populate top block parameters
        Config.topblock['stmt'] = ancs[0]

        # resolve
        for cs_stmt in Config.callsite['stmts']:
            #resolve cs_stmt
            f2003_search_unknowns(cs_stmt, cs_stmt.f2003)
            for uname, req in cs_stmt.unknowns.iteritems():
                cs_stmt.resolve(req)
                if not req.res_stmts:
                    raise kgutils.ProgramException('Resolution fail.')

        # update state info of callsite and its upper blocks
        kganalyze.update_state_info(Config.parentblock['stmt'])

        # update state info of modules
        for modname, moddict in Config.modules.iteritems():
            modstmt = moddict['stmt']
            if modstmt != Config.topblock['stmt']:
                kganalyze.update_state_info(moddict['stmt'])


    def add_geninfo_ancestors(self, stmt):
        from block_statements import EndStatement

        ancs = stmt.ancestors()

        prevstmt = stmt
        prevname = None

        for anc in reversed(ancs):
            if not hasattr(anc, 'geninfo'):
                anc.geninfo = collections.OrderedDict()
            if len(anc.content)>0 and isinstance(anc.content[-1], EndStatement) and \
                not hasattr(anc.content[-1], 'geninfo'):
                anc.content[-1].geninfo = collections.OrderedDict()

            if prevname:
                dummy_req = kgparse.ResState(kgparse.KGGenType.STATE_IN, kgutils.KGName(prevname), None, [anc])
                dummy_req.res_stmts = [ prevstmt ]
                anc.check_spec_stmts(dummy_req.uname, dummy_req)

            if hasattr(anc, 'name'): prevname = anc.name
            else: prevname = None
            prevstmt = anc

