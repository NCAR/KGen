!KGEN-generated Fortran source file 
  
!Generated at : 2017-11-30 22:38:01 
!KGEN version : 0.7.3 
  


module raddiff_mod !jgw!
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    IMPLICIT NONE 
    PUBLIC rad_diffusion 
contains
SUBROUTINE rad_diffusion(kgen_unit, kgen_measure, kgen_isverified) 
    !  Calculates radiation transport using multi-group flux-limited 
    !  diffusion  model
    !
    !
    USE def_kind, ONLY: ik, rk 
    USE def_rad, ONLY: grid 
    USE multigrid_solver_mod, ONLY: multigrid_solver 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE mpi 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE def_rad, ONLY: kr_def_rad_grid 
    USE def_vars, ONLY: kr_externs_out_def_vars 
    USE def_rad, ONLY: kv_def_rad_grid 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    USE def_vars, ONLY: kv_externs_def_vars 
    IMPLICIT NONE 
    !-----------------------------------------------------------------------
    !  Local variables
    !-----------------------------------------------------------------------
    INTEGER(KIND=ik) :: ifg 
    INTEGER(KIND=ik), pointer :: nfg 


    real (rk), parameter :: errmax = 1.0e-5_rk

    TYPE(grid), pointer :: sg, top_grid 

    !-----------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kgen_unit 
    REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
    LOGICAL, INTENT(OUT) :: kgen_isverified 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
    LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    INTEGER :: send(1)=-1, recv(1)=-1, kgen_ierr 
      
    TYPE(check_t) :: check_status 
    INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
    INTEGER(KIND=ik) :: kgenref_ifg 
    TYPE(grid), pointer :: kgenref_sg 
      
    !local input variables 
    READ (UNIT = kgen_unit) ifg 
    CALL kr_rad_diffusion_integer__ik_ptr(nfg, kgen_unit, "nfg", .FALSE.) 
    CALL kr_rad_diffusion_grid__grid_ptr(sg, kgen_unit, "sg", .FALSE.) 
    CALL kr_rad_diffusion_grid__grid_ptr(top_grid, kgen_unit, "top_grid", .FALSE.) 
      
    !extern output variables 
    CALL kr_externs_out_def_vars(kgen_unit) 
      
    !local output variables 
    READ (UNIT = kgen_unit) kgenref_ifg 
    CALL kr_rad_diffusion_grid__grid_ptr(kgenref_sg, kgen_unit, "kgenref_sg", .FALSE.) 

    !-----------------------------------------------------------------------
    ! Initialize the top subgrid
    !
    !
    !
    ! Allocate and define global coordinates sg%x2c and sg%x3c,
    ! and global indeces sg%i2s, sg%i2e, sg%i3s, sg%i3e

    !

    ! Calculate flux-limited radiation diffusion coefficient

    !

    !

    !jgw!forall(i1=i1s:i1e,i2=i2s:i2e,i3=i3s:i3e,ifg=1:nfg) sg%opp(i1,i2,i3,ifg)=var%opap(i1,i2,i3,ifg)
    !


    !-----------------------------------------------------------------------
    !  Build the whole subgrid hierarchy
    !-----------------------------------------------------------------------
    ! The top level subgrid is of the zero level.
    ! The lowest subgrid can be up to sg%level=N+1,
    ! where 2**N is the number of theta/phi zones in subdomain.
    ! If Ndomain2 is the odd number then the lowest sg%level=N.
    ! The lowest subgrid level = bottom_level

    !
    !


    !


    !bottom_level = sg%level ! remember the bottom grid
    !-----------------------------------------------------------------------
    !  This is to print out the subgrid coordinates
    !
    !


    !-----------------------------------------------------------------------
    !$kgen begin_callsite multigrid
    IF (kgen_evalstage) THEN 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_mainstage) THEN 
    END IF   
      
    !Uncomment following call statement to turn on perturbation experiment. 
    !Adjust perturbation value and/or kind parameter if required. 
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
      
      
    !call to kgen kernel 
    !jgw! %fine is not written to kgen files so this association is addeded
    sg => top_grid
    do while(associated(sg%crse))
       sg%crse%fine => sg
       sg           => sg%crse
    end do

    ! the real kernel
    ! remove "freq_groups_loop" label as kgen can't handle it.
    !freq_groups_loop: do ifg=1,nfg
    do ifg=1,nfg
       sg => top_grid
       call multigrid_solver(ifg,errmax,sg)
    end do !freq_groups_loop
    IF (kgen_mainstage) THEN 
          
        !verify init 
        CALL kgen_init_check(check_status, tolerance=1.0d-10, verboseLevel=3) 
          
        !extern verify variables 
        CALL kv_externs_def_vars(check_status) 
          
        !local verify variables 
        CALL kv_rad_diffusion_integer__ik("ifg", check_status, ifg, kgenref_ifg) 
        CALL kv_rad_diffusion_grid__grid_ptr("sg", check_status, sg, kgenref_sg) 
        WRITE (*, *) "" 
        IF (check_status%verboseLevel > 0) THEN 
            WRITE (*, *) "Number of output variables: ", check_status%numTotal 
            WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
            WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
            WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
            WRITE (*, *) "Tolerance: ", kgen_tolerance 
        END IF   
        WRITE (*, *) "" 
          
        send(1) = check_status%numOutTol 
        CALL mpi_allreduce(send, recv, 1, MPI_INT, MPI_MAX, 1140850688, kgen_ierr) 
        check_status%numOutTol = recv(1) 
        CALL mpi_comm_rank(1140850688, kgen_mpirank, kgen_ierr) 
          
        IF (check_status%numOutTol > 0) THEN 
            IF (kgen_mpirank == 0) THEN 
                WRITE (*, *) "Verification FAILED" 
            END IF   
            check_status%Passed = .FALSE. 
            kgen_isverified = .FALSE. 
        ELSE 
            IF (kgen_mpirank == 0) THEN 
                WRITE (*, *) "Verification PASSED" 
            END IF   
            check_status%Passed = .TRUE. 
            kgen_isverified = .TRUE. 
        END IF   
        WRITE (*, *) "" 
        CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
    !jgw! %fine is not written to kgen files so this association is addeded
    sg => top_grid
    do while(associated(sg%crse))
       sg%crse%fine => sg
       sg           => sg%crse
    end do

    ! the real kernel
    ! remove "freq_groups_loop" label as kgen can't handle it.
    !freq_groups_loop: do ifg=1,nfg
    do ifg=1,nfg
       sg => top_grid
       call multigrid_solver(ifg,errmax,sg)
    end do !freq_groups_loop
    CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
    kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock) 
    WRITE (*, *) "multigrid : Time per call (usec): ", kgen_mpirank, kgen_measure 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_evalstage) THEN 
    END IF   
    !


    !$kgen end_callsite multigrid
    !-----------------------------------------------------------------------
    !  Deallocate subgrids
    !-----------------------------------------------------------------------
    ! Cycle down to the coarsest (bottom) subgrid

    !
    !

    ! Cosequently deallocate subgrids from bottom to top
    !
    !

    !-----------------------------------------------------------------------

    !   do i1=i1s,i1e
    !     do i2=i2s,i2e
    !       do i3=i3s,i3e
    !         do ifg=1,nfg
    !           if(var%fr(i1,i2,i3,ifg) < 0.0_rk) then
    !             write(*,"('fr < 0 exiting rad_diffusion, fr=',1pd12.5)") var%fr(i1,i2,i3,ifg)
    !             write(*,"('ProcID=',i4,' i1=',i4,' i1s=',i4,' i1e=',i4)") ProcID, i1, i1s, i1e
    !             write(*,"('ProcID=',i4,' i2=',i4,' i2s=',i4,' i2e=',i4)") ProcID, i2, i2s, i2e
    !             write(*,"('ProcID=',i4,' i3=',i4,' i3s=',i4,' i3e=',i4)") ProcID, i3, i3s, i3e
    !             stop
    !           end if
    !         end do
    !       end do
    !     end do
    !   end do
      
    CONTAINS 
      


    !read state subroutine for kr_rad_diffusion_integer__ik_ptr 
    SUBROUTINE kr_rad_diffusion_integer__ik_ptr(var, kgen_unit, printname, printvar) 
        INTEGER(KIND=ik), INTENT(INOUT), POINTER :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        CHARACTER(LEN=*), INTENT(IN) :: printname 
        LOGICAL, INTENT(IN), OPTIONAL :: printvar 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        READ (UNIT = kgen_unit) kgen_istrue 
        IF (kgen_istrue) THEN 
            IF (ASSOCIATED( var )) THEN 
                NULLIFY (var) 
            END IF   
            ALLOCATE (var) 
            READ (UNIT = kgen_unit) var 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                WRITE (*, *) "KGEN DEBUG: " // printname // " = ", var 
            END IF   
        END IF   
    END SUBROUTINE kr_rad_diffusion_integer__ik_ptr 
      
    !read state subroutine for kr_rad_diffusion_grid__grid_ptr 
    SUBROUTINE kr_rad_diffusion_grid__grid_ptr(var, kgen_unit, printname, printvar) 
        TYPE(grid), INTENT(INOUT), POINTER :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        CHARACTER(LEN=*), INTENT(IN) :: printname 
        LOGICAL, INTENT(IN), OPTIONAL :: printvar 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        READ (UNIT = kgen_unit) kgen_istrue 
        IF (kgen_istrue) THEN 
            IF (ASSOCIATED( var )) THEN 
                NULLIFY (var) 
            END IF   
            ALLOCATE (var) 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                CALL kr_def_rad_grid(var, kgen_unit, printname, .TRUE.) 
            ELSE 
                CALL kr_def_rad_grid(var, kgen_unit, printname, .FALSE.) 
            END IF   
        END IF   
    END SUBROUTINE kr_rad_diffusion_grid__grid_ptr 
      
    !verify state subroutine for kv_rad_diffusion_integer__ik 
    RECURSIVE SUBROUTINE kv_rad_diffusion_integer__ik(varname, check_status, var, kgenref_var) 
        CHARACTER(LEN=*), INTENT(IN) :: varname 
        TYPE(check_t), INTENT(INOUT) :: check_status 
        INTEGER(KIND=ik), INTENT(IN) :: var, kgenref_var 
        INTEGER :: check_result = -1 
        LOGICAL :: is_print = .FALSE. 
          
        integer(KIND=ik) :: diff 
          
        check_status%numTotal = check_status%numTotal + 1 
          
        IF (var == kgenref_var) THEN 
            check_status%numIdentical = check_status%numIdentical + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            diff = ABS(var - kgenref_var) 
            IF (diff <= kgen_tolerance) THEN 
                check_status%numInTol = check_status%numInTol + 1 
                IF (check_status%verboseLevel > 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                END IF   
                check_result = CHECK_IN_TOL 
            ELSE 
                check_status%numOutTol = check_status%numOutTol + 1 
                IF (check_status%verboseLevel > 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                END IF   
                check_result = CHECK_OUT_TOL 
            END IF   
        END IF   
        IF (check_result == CHECK_IDENTICAL) THEN 
            IF (check_status%verboseLevel > 2) THEN 
                WRITE (*, *) "Difference is ", 0 
                WRITE (*, *) "" 
            END IF   
        ELSE IF (check_result == CHECK_OUT_TOL) THEN 
            IF (check_status%verboseLevel > 0) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        END IF   
          
    END SUBROUTINE kv_rad_diffusion_integer__ik 
      
    !verify state subroutine for kv_rad_diffusion_grid__grid_ptr 
    RECURSIVE SUBROUTINE kv_rad_diffusion_grid__grid_ptr(varname, check_status, var, kgenref_var) 
        CHARACTER(LEN=*), INTENT(IN) :: varname 
        TYPE(check_t), INTENT(INOUT) :: check_status 
        TYPE(grid), pointer, INTENT(IN) :: var, kgenref_var 
        INTEGER :: check_result = -1 
        LOGICAL :: is_print = .FALSE. 
          
          
        IF (ASSOCIATED(var)) THEN 
            check_status%numTotal = check_status%numTotal + 1 
              
            IF (check_result == CHECK_IDENTICAL) THEN 
                IF (check_status%verboseLevel > 2) THEN 
                    WRITE (*, *) "NOT IMPLEMENTED" 
                    WRITE (*, *) "" 
                END IF   
            ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                IF (check_status%verboseLevel > 0) THEN 
                    WRITE (*, *) "NOT IMPLEMENTED" 
                    WRITE (*, *) "" 
                END IF   
            ELSE IF (check_result == CHECK_IN_TOL) THEN 
                IF (check_status%verboseLevel > 1) THEN 
                    WRITE (*, *) "NOT IMPLEMENTED" 
                    WRITE (*, *) "" 
                END IF   
            END IF   
              
        END IF   
    END SUBROUTINE kv_rad_diffusion_grid__grid_ptr 
      
END SUBROUTINE rad_diffusion 
end module raddiff_mod !jgw!