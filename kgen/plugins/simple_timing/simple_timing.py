# simple_timing.py

#import os 
import statements
import block_statements
import typedecl_statements
from kgen_plugin import Kgen_Plugin


class Simple_Timing(Kgen_Plugin):
    def __init__(self):
        self.frame_msg = None

    # registration
    def register(self, msg):
        self.frame_msg = msg

        # register event per function 
        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
            getinfo('parentblock_stmt'), None, self.add_simple_timing) 

    def add_simple_timing(self, node):

        attrs = {'type_spec': 'INTEGER', 'selector': ('8', None), \
            'entity_decls': ['kgen_intvar', 'kgen_start_clock', 'kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'INTEGER', 'attrspec': ['PARAMETER'], 'entity_decls': ['kgen_maxiter = 100']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'type_spec': 'REAL', 'selector': (None, 'kgen_dp'), 'entity_decls': ['kgen_elapsed_time']}
        part_append_genknode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_start_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'loopcontrol': 'kgen_intvar = 1, kgen_maxiter'}
        doobj = part_append_genknode(node, EXEC_PART, block_statements.Do, attrs=attrs)

        dummy_node = part_append_genknode(doobj, EXEC_PART, statements.Call)
        dummy_node.kgen_stmt = getinfo('callsite_stmt')
        dummy_node.kgen_forced_line = dummy_node.kgen_stmt.genkpair.tostring()

        attrs = {'designator': 'SYSTEM_CLOCK', 'items': ['kgen_stop_clock', 'kgen_rate_clock']}
        part_append_genknode(node, EXEC_PART, statements.Call, attrs=attrs)

        attrs = {'variable': 'kgen_elapsed_time', 'sign': '=', 'expr': '1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'items': ['"micro_mg_tend2_0 : Time per call (usec): "', 'kgen_elapsed_time']}
        part_append_gensnode(node, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'variable': 'kgen_total_time', 'sign': '=', 'expr': 'kgen_total_time + kgen_elapsed_time'}
        part_append_genknode(node, EXEC_PART, statements.Assignment, attrs=attrs)
       


#        
#                        CALL system_clock(start_clock, rate_clock)
#!                       call vprof_start()
#                        DO kgen_intvar=1,maxiter
#                            CALL micro_mg_tend2_0(mgncol, nlev, dtime / num_steps, packed_t, packed_q, packed_qc, packed_qi, packed_nc, packed_ni, packed_qr, packed_qs, packed_nr, packed_ns, packed_relvar, packed_accre_enhan, packed_p, packed_pdel, packed_cldn, packed_liqcldf, packed_icecldf, packed_rate1ord_cw2pr_st, packed_naai, packed_npccn, packed_rndst, packed_nacon, packed_tlat, packed_qvlat, packed_qctend, packed_qitend, packed_nctend, packed_nitend, packed_qrtend, packed_qstend, packed_nrtend, packed_nstend, packed_rel, rel_fn_dum, packed_rei, packed_prect, packed_preci, packed_nevapr, packed_evapsnow, packed_prain, packed_prodsnow, packed_cmeout, packed_dei, packed_mu, packed_lambdac, packed_qsout, packed_des, packed_rflx, packed_sflx, packed_qrout, reff_rain_dum, reff_snow_dum, packed_qcsevap, packed_qisevap, packed_qvres, packed_cmei, packed_vtrmc, packed_vtrmi, packed_umr, packed_ums, packed_qcsedten, packed_qisedten, packed_qrsedten, packed_qssedten, packed_pra,
#packed_prc, packed_mnuccc, packed_mnucct, packed_msacwi, packed_psacws, packed_bergs, packed_berg, packed_melt, packed_homo, packed_qcres, packed_prci, packed_prai, packed_qires, packed_mnuccr, packed_pracs, packed_meltsdt, packed_frzrdt, packed_mnuccd, packed_nrout, packed_nsout, packed_refl, packed_arefl, packed_areflz, packed_frefl, packed_csrfl, packed_acsrfl, packed_fcsrfl, packed_rercld, packed_ncai, packed_ncal, packed_qrout2, packed_qsout2, packed_nrout2, packed_nsout2, drout_dum, dsout2_dum, packed_freqs, packed_freqr, packed_nfice, packed_qcrat, errstring, packed_tnd_qsnow, packed_tnd_nsnow, packed_re_ice, packed_prer_evap, packed_frzimm, packed_frzcnt, packed_frzdep)
#                        END DO
#!                       call vprof_stop()
#                        CALL system_clock(stop_clock, rate_clock)
#                        WRITE(*,*)
#                        PRINT *, "micro_mg_tend2_0 : Time per call (usec): ", 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
#!                       call mpi_finalize(info)
#
#class Debug_Add_Sum_Assignment(Kgen_Plugin):
#    def __init__(self):
#        self.frame_msg = None
#
#    # registration
#    def register(self, msg):
#        self.frame_msg = msg
#
#        # register events
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.BeginSource, self.has_topblock, self.create_common_block) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.BeginSource, self.has_topblock, self.create_common_block) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.Function, None, self.add_common_stmt) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.Function, has_state, self.add_common_stmt) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.Subroutine, None, self.add_common_stmt) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#            block_statements.Subroutine, has_state, self.add_common_stmt) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.NODE_CREATED, \
#            statements.Assignment, None, self.add_write_stmt) 
#
#        self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.NODE_CREATED, \
#            statements.Assignment, has_state, self.add_write_stmt) 
#
#    def has_topblock(self, node):
#        checks = lambda n: n.kgen_stmt and n.kgen_stmt==getinfo('topblock_stmt')
#        if part_has_node(node, UNIT_PART, checks):
#            return True
#        else:
#            return False
#
#    def create_common_block(self, node):
#        # add block data
#        attrs = {'name': 'kgen_block_data'}
#        bdata = part_append_genknode(node, UNIT_PART, block_statements.BlockData, attrs=attrs)
#
#        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_region_started = .FALSE.', 'kgen_collect_log = .FALSE.']}
#        part_append_genknode(bdata, DECL_PART, typedecl_statements.Logical, attrs=attrs) 
#
#        attrs = {'items': [('kgen_common_region', ['kgen_region_started', 'kgen_collect_log'])]}
#        part_append_genknode(bdata, DECL_PART, statements.Common, attrs=attrs) 
#
#        # register event for topblock file 
#        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.KERNEL, GENERATION_STAGE.BEGIN_PROCESS, \
#        #    block_statements.BeginSource, self.has_topblock, self.create_common_block) 
#
#        #self.frame_msg.add_event(KERNEL_SELECTION.ALL, FILE_TYPE.STATE, GENERATION_STAGE.BEGIN_PROCESS, \
#        #    block_statements.BeginSource, self.has_topblock, self.create_common_block) 
#
#    def add_common_stmt(self, node):
#
#        if node.kgen_stmt is None: return
#
#        node.kgen_stmt.top.used4genstate = True
#
#        if not (node.kgen_stmt.is_pure() or node.kgen_stmt.is_elemental()):
#            attrs = {'type_spec': 'INTEGER', 'attrspec': ['SAVE'], 'entity_decls': ['kgen_invoc_count = 0']}
#            part_append_gensnode(node, DECL_PART, typedecl_statements.Integer, attrs=attrs) 
#
#        attrs = {'type_spec': 'LOGICAL', 'entity_decls': ['kgen_region_started', 'kgen_collect_log']}
#        part_append_gensnode(node, DECL_PART, typedecl_statements.Logical, attrs=attrs) 
#
#        attrs = {'items': [('kgen_common_region', ['kgen_region_started', 'kgen_collect_log'])]}
#        part_append_gensnode(node, DECL_PART, statements.Common, attrs=attrs)
#
#    def add_write_stmt(self, node):
#        if node.kgen_stmt is None: return
#
#        # find typedecl for lhs
#        typedecl_stmt = None
#        varname = None
#        for uname, req in node.kgen_stmt.unknowns.items():
#            if node.kgen_stmt.variable.startswith(uname.firstpartname()):
#                if isinstance(req.res_stmts[0], typedecl_statements.TypeDeclarationStatement):
#                    typedecl_stmt = req.res_stmts[0]
#                    varname = uname.firstpartname()
#                    break
#
#        if typedecl_stmt and typedecl_stmt.is_numeric():
#            var = typedecl_stmt.get_variable(varname)
#            path = os.path.basename(node.kgen_stmt.item.reader.id)
#            index, partid, part = get_part_index(node)
#            upperblock_stmt = node.kgen_stmt.ancestors()[-1]
#            if isinstance(upperblock_stmt, block_statements.SubProgramStatement) and (upperblock_stmt.is_pure() or \
#                upperblock_stmt.is_elemental()):
#                return
#
#            # reduce output log size
#            if not node.kgen_stmt.parent is upperblock_stmt: return
#
#            node.kgen_stmt.top.used4genstate = True
#
#            attrs = {'expr': 'kgen_region_started .AND. kgen_collect_log'}
#            ifobj = part_insert_gensnode(node.kgen_parent, partid, block_statements.IfThen, attrs=attrs, index=(index+1))
#
#            if var.is_array():
#                attrs = {'items': ['"SUM(%s) at function %s in %s: "'%(varname, upperblock_stmt.name, path),'SUM(%s)'%varname]}
#                part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)
#            else:
#                attrs = {'items': ['"%s at function %s in %s: "'%(varname, upperblock_stmt.name, path), varname]}
#                part_append_gensnode(ifobj, EXEC_PART, statements.Write, attrs=attrs)
#
#
