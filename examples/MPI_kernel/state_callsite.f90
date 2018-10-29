

module raddiff_mod !jgw!
  implicit none
contains
  subroutine rad_diffusion
    !  Calculates radiation transport using multi-group flux-limited 
    !  diffusion  model
    !
    !
    USE def_kind,  ONLY : ik, rk
    USE def_mpi,   ONLY : NProcs, ProcID, MASTER, &
         Idomain1, Idomain2, Idomain3, &
         Ndomain1, Ndomain2, Ndomain3
    USE def_vars,  ONLY : var, dtime, clight, TINY_NUMBER, PI1
    USE def_rad,   ONLY : opac_table_groups, grid, fr
    USE def_ursos, ONLY : n_e
    USE multigrid_solver_mod, ONLY : multigrid_solver !jgw!
    USE s_b_z_1,  ONLY : set_boundary_zone_1 !jgw!
    USE s_b_z_2,  ONLY : set_boundary_zone_2 !jgw!
    USE s_b_z_3,  ONLY : set_boundary_zone_3 !jgw!
    USE mpi
    USE def_rad, ONLY: kw_def_rad_grid 
    USE def_rad, ONLY: kw_externs_in_def_rad 
    USE def_vars, ONLY: kw_externs_in_def_vars 
    USE def_vars, ONLY: kw_externs_out_def_vars 
    USE def_mpi, ONLY: kw_externs_in_def_mpi 
    IMPLICIT NONE
    !-----------------------------------------------------------------------
    !  Local variables
    !-----------------------------------------------------------------------
    !kgen variables 
    INTEGER :: kgen_openmp_issave 
    INTEGER :: kgen_mpirank 
    COMMON / state / kgen_mpirank, kgen_openmp_issave 
    LOGICAL :: kgen_istrue 
    INTEGER :: kgen_count 
    INTEGER, SAVE :: kgen_unit, kgen_stopunit 
    INTEGER, SAVE :: kgen_mymid, kgen_msize, kgen_osize, kgen_ierr 
    REAL(KIND=8), SAVE :: kgen_array_sum, kgen_realnum 
    LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: kgen_isstop 
    CHARACTER(LEN=1024), SAVE, ALLOCATABLE, DIMENSION(:,:) :: kgen_filepath, kgen_lockpath 
    LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: kgen_islast, kgen_issave, kgen_ischecked 
    INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: kgen_invoke, kgen_last_invoke 
    integer (ik) :: i1, i2, i3, ierr, ifg, n2, n3
    integer (ik) :: icycle, ilevel, bottom_level
    integer (ik), pointer :: i1s, i1e, i2s, i2e, i3s, i3e, nfg

    real (rk) :: tmp, tmp1, tmp2, tmp3, tmp4, cdt, err

    real (rk), parameter :: errmax = 1.0e-5_rk
    real (rk), parameter :: zone_optical_depth_floor = 1.0e-7_rk

    type (grid), pointer :: sg, top_grid

    logical, save :: first_call = .true.
    !-----------------------------------------------------------------------

    i1s => var%i1s
    i1e => var%i1e
    i2s => var%i2s
    i2e => var%i2e
    i3s => var%i3s
    i3e => var%i3e
    nfg => opac_table_groups
    !-----------------------------------------------------------------------
    ! Initialize the top subgrid
    !
    !
    !
    allocate(sg)
    top_grid => sg ! remember the top grid
    sg%level=0     ! the top-subgrid level is zero
    ! Allocate and define global coordinates sg%x2c and sg%x3c,
    ! and global indeces sg%i2s, sg%i2e, sg%i3s, sg%i3e

    !
    call get_global_coordinates_2 (sg)
    call get_global_coordinates_3 (sg)

    call allocate_subgrid(sg)
    ! Calculate flux-limited radiation diffusion coefficient

    !
    cdt=clight*dtime
    do i1=i1s,i1e
       tmp1=var%x1c(i1+1)-var%x1c(i1-1)
       do i2=i2s,i2e
          tmp2=var%x1c(i1)*(var%x2c(i2+1)-var%x2c(i2-1))
          tmp4=var%x1c(i1)*var%x2c_sin(i2)
          do i3=i3s,i3e
             tmp3=tmp4*(var%x3c(i3+1)-var%x3c(i3-1))
             do ifg=1,nfg
                if(fr(i1,i2,i3,ifg) < 0.0_rk) then
                   write(*,"('fr < 0 entering rad_diffusion, fr=',1pd12.5)") var%fr(i1,i2,i3,ifg)
                   write(*,"('ProcID=',i4,' i1=',i4,' i1s=',i4,' i1e=',i4)") ProcID, i1, i1s, i1e
                   write(*,"('ProcID=',i4,' i2=',i4,' i2s=',i4,' i2e=',i4)") ProcID, i2, i2s, i2e
                   write(*,"('ProcID=',i4,' i3=',i4,' i3s=',i4,' i3e=',i4)") ProcID, i3, i3s, i3e
                   stop
                end if
                tmp=((fr(i1+1,i2,i3,ifg)-fr(i1-1,i2,i3,ifg))/tmp1)**2+ &
                     ((fr(i1,i2+1,i3,ifg)-fr(i1,i2-1,i3,ifg))/tmp2)**2+ &
                     ((fr(i1,i2,i3+1,ifg)-fr(i1,i2,i3-1,ifg))/tmp3)**2
                tmp=sqrt(tmp)/(fr(i1,i2,i3,ifg)+TINY_NUMBER) ! inverse spatial scale of rad. en. density
                ! assume a floor for the zone's optical depth
                !
                !
                tmp=tmp+zone_optical_depth_floor/(0.5_rk*sqrt(tmp1**2+tmp2**2+tmp3**2))
                ! Photon mean-free path with flux limitation
                !
                !
                tmp=1.0_rk/(3.0_rk*var%opar(i1,i2,i3,ifg)+tmp)
                !
                sg%dif(i1,i2,i3,ifg)=cdt*tmp ! diff. coef. in [cm^2]
             end do
          end do
       end do
    end do
    !
    do ifg=1,nfg
       call set_boundary_zone_1(i2s,i2e,i3s,i3e,sg%dif(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
       call set_boundary_zone_2(i2s,i2e,i3s,i3e,sg%dif(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
       call set_boundary_zone_3(i2s,i2e,i3s,i3e,sg%dif(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
    end do
    !jgw!forall(i1=i1s:i1e,i2=i2s:i2e,i3=i3s:i3e,ifg=1:nfg) sg%opp(i1,i2,i3,ifg)=var%opap(i1,i2,i3,ifg)
    !
    sg%opp(i1s:i1e,i2s:i2e,i3s:i3e,1:nfg)=var%opap(i1s:i1e,i2s:i2e,i3s:i3e,1:nfg)
    do ifg=1,nfg
       call set_boundary_zone_1(i2s,i2e,i3s,i3e,sg%opp(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
       call set_boundary_zone_2(i2s,i2e,i3s,i3e,sg%opp(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
       call set_boundary_zone_3(i2s,i2e,i3s,i3e,sg%opp(i1s-1:i1e+1,i2s-1:i2e+1,i3s-1:i3e+1,ifg))
    end do

    call get_diff_coefs (sg)
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
    select case (i2e-i2s+1)
    case (  2)
       bottom_level=2
    case (  4)
       bottom_level=3
    case (  8)
       bottom_level=4
    case ( 16) 
       bottom_level=5
    case ( 32)
       bottom_level=6
    case ( 64)
       bottom_level=7
    case (128)
       bottom_level=8
    case (256)
       bottom_level=9
    case default
       write(*,"('*** Wrong bottom_level in rad_diffusion')")
       call terminate
    end select
    if(Ndomain2 /= 2*(Ndomain2/2)) bottom_level=bottom_level-1
    !
    do
       if(.not.associated(sg%crse)) allocate(sg%crse)
       sg%crse%fine => sg
       sg           => sg%crse
       sg%level=sg%fine%level+1
       call build_coarse_subgrid(sg)
       call allocate_subgrid(sg)
       call restrict_dif(sg)
       call get_diff_coefs (sg)
       if(sg%level == bottom_level) exit
       ! The top level subgrid is of the zero level.
       ! The lowest subgrid can be up to sg%level=N+1,
       ! where 2**N is the number of phi zones in subdomain.
       ! If Ndomain2 is the odd number then the lowest sg%level=N.
       !if(sg%level==4) exit ! for  8 zones
       !if(sg%level==5) exit ! for 16 zones
       !if(sg%level==6) exit ! for 32 zones
       !if(sg%level==7) exit ! for 64 zones
       !if(maxval(sg%i2e(1:Ndomain2)-sg%i2s(1:Ndomain2)) == 0) exit
       !
       !
    end do
    !bottom_level = sg%level ! remember the bottom grid
    !-----------------------------------------------------------------------
    !  This is to print out the subgrid coordinates
    !
    !
    if(.false.) then
       if(ProcID==MASTER) then
          write(*,*)'bottom_level =',bottom_level
          sg => top_grid
          do

             write(*,"('*** grid_level=',g12.5)") sg%level
             i2=sg%i2s(1)-1
             write(*,"('i2=',i3,' x2c=',1pd12.5)") i2, sg%x2c(i2)/PI1
             if(sg%level==0) then
                i2=sg%i2s(1)-1
                write(*,"('i2=',i3,' x2c=',1pd12.5)") i2, sg%x2c(i2)/PI1
                do n2=1,Ndomain2
                   do i2=sg%i2s(n2),sg%i2e(n2)
                      write(*,"('i2=',i3,' x2c=',1pd12.5,' i2=',i3,' ',i3,' ii2=',i3,' ',i3,'  Idomain2=',i3)") &
                           i2, sg%x2c(i2)/PI1, 0, 0, sg%ii2m(i2), sg%ii2p(i2), n2
                   end do
                end do
             else if(sg%level==bottom_level) then
                do n2=1,Ndomain2
                   do i2=sg%i2s(n2),sg%i2e(n2)
                      write(*,"('i2=',i3,' x2c=',1pd12.5,' i2=',i3,' ',i3,' ii2=',i3,' ',i3,'  Idomain2=',i3)") &
                           i2, sg%x2c(i2)/PI1, sg%i2m(i2), sg%i2p(i2), 0, 0, n2
                   end do
                end do
             else
                do n2=1,Ndomain2
                   do i2=sg%i2s(n2),sg%i2e(n2)
                      write(*,"('i2=',i3,' x2c=',1pd12.5,' i2=',i3,' ',i3,' ii2=',i3,' ',i3,'  Idomain2=',i3)") &
                           i2, sg%x2c(i2)/PI1, sg%i2m(i2), sg%i2p(i2), sg%ii2m(i2), sg%ii2p(i2), n2
                   end do
                end do
             end if
             i2=sg%i2e(Ndomain2)+1
             write(*,"('i2=',i3,' x2c=',1pd12.5)") i2, sg%x2c(i2)/PI1

             write(*,"('*** ')")
             i3=sg%i3s(1)-1
             write(*,"('i3=',i3,' x3c=',1pd12.5)") i3, sg%x3c(i3)/PI1
             if(sg%level==0) then
                do n3=1,Ndomain3
                   do i3=sg%i3s(n3),sg%i3e(n3)
                      write(*,"('i3=',i3,' x3c=',1pd12.5,' i3=',i3,' ',i3,' ii3=',i3,' ',i3,'  Idomain3=',i3)") &
                           i3, sg%x3c(i3)/PI1, 0, 0, sg%ii3m(i3), sg%ii3p(i3), n3
                   end do
                end do
             else if(sg%level==bottom_level) then
                do n3=1,Ndomain3
                   do i3=sg%i3s(n3),sg%i3e(n3)
                      write(*,"('i3=',i3,' x3c=',1pd12.5,' i3=',i3,' ',i3,' ii3=',i3,' ',i3,'  Idomain3=',i3)") &
                           i3, sg%x3c(i3)/PI1, sg%i3m(i3), sg%i3p(i3), 0, 0, n3
                   end do
                end do
             else
                do n3=1,Ndomain3
                   do i3=sg%i3s(n3),sg%i3e(n3)
                      write(*,"('i3=',i3,' x3c=',1pd12.5,' i3=',i3,' ',i3,' ii3=',i3,' ',i3,'  Idomain3=',i3)") &
                           i3, sg%x3c(i3)/PI1, sg%i3m(i3), sg%i3p(i3), sg%ii3m(i3), sg%ii3p(i3), n3
                   end do
                end do
             end if
             i3=sg%i3e(Ndomain3)+1
             write(*,"('i3=',i3,' x3c=',1pd12.5)") i3, sg%x3c(i3)/PI1

             if(sg%level==bottom_level) exit
             sg => sg%crse
          end do
       end if
       call MPI_BARRIER(MPI_COMM_WORLD,ierr)
       stop
    end if
    !-----------------------------------------------------------------------
    !$kgen begin_callsite multigrid
    !START OF KGEN REGION 
    IF (.NOT. ALLOCATED(kgen_isstop)) THEN 
        kgen_osize = 1 
        kgen_unit = -1 
        kgen_stopunit = -1 
        ALLOCATE (kgen_ischecked(0:kgen_osize-1)) 
        ALLOCATE (kgen_islast(0:kgen_osize-1)) 
        ALLOCATE (kgen_issave(0:kgen_osize-1)) 
        ALLOCATE (kgen_invoke(0:kgen_osize-1)) 
        ALLOCATE (kgen_last_invoke(0:kgen_osize-1)) 
        kgen_ischecked(:) = .FALSE. 
        kgen_islast(:) = .FALSE. 
        kgen_issave(:) = .FALSE. 
        kgen_invoke(:) = 0 
        kgen_last_invoke(:) = 0 
        CALL mpi_comm_rank(1140850688, kgen_mymid, kgen_ierr) 
        CALL kgen_error_stop(kgen_ierr, "mpi_comm_rank is failed") 
        CALL mpi_comm_size(1140850688, kgen_msize, kgen_ierr) 
        CALL kgen_error_stop(kgen_ierr, "mpi_comm_size is failed") 
        kgen_mpirank = kgen_mymid 
        ALLOCATE (kgen_filepath(0:kgen_msize-1, 0:kgen_osize-1)) 
        ALLOCATE (kgen_lockpath(0:kgen_msize-1, 0:kgen_osize-1)) 
        ALLOCATE (kgen_isstop(0:kgen_msize-1, 0:kgen_osize-1)) 
        kgen_isstop(:,:) = .TRUE. 
        CALL kgen_init_vars(kgen_mymid, INT(0), INT(15), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, kgen_lockpath, &
        &kgen_last_invoke, kgen_isstop) 
    END IF   
    CALL mpi_barrier(1140850688, kgen_ierr) 
    kgen_issave(0) = .FALSE. 
    kgen_islast(0) = .FALSE. 
    CALL kgen_check_save(INT(0), INT(15), INT(0), INT(0), INT(0), INT(0), kgen_mymid, 0, kgen_osize, kgen_invoke, &
    &kgen_last_invoke, kgen_issave, kgen_islast) 
    IF (kgen_issave(0)) THEN 
        WRITE (kgen_filepath(kgen_mymid, 0), FMT="(A,I0,A,I0,A,I0)") "/p/work1/wohlbier/devel/aster/kgen/kernel/multigrid.", &
        &kgen_mymid, ".", 0, ".", kgen_invoke(0) 
        OPEN (NEWUNIT=kgen_unit, FILE=TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0))), STATUS="REPLACE", ACCESS="STREAM", &
        &FORM="UNFORMATTED", ACTION="WRITE", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr) 
        CALL kgen_error_stop(kgen_ierr, "File open error: " // TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0)))) 
          
        !argument input variables 
          
        !extern input variables 
        CALL kw_externs_in_def_rad(kgen_unit) 
        CALL kw_externs_in_def_vars(kgen_unit) 
        CALL kw_externs_in_def_mpi(kgen_unit) 
          
        !local input variables 
        WRITE (UNIT = kgen_unit) ifg 
        CALL kw_rad_diffusion_integer__ik_ptr(nfg, kgen_unit, "nfg", .FALSE.) 
        CALL kw_rad_diffusion_grid__grid_ptr(sg, kgen_unit, "sg", .FALSE.) 
        CALL kw_rad_diffusion_grid__grid_ptr(top_grid, kgen_unit, "top_grid", .FALSE.) 
    END IF   
    IF (kgen_issave(0)) THEN 
        kgen_openmp_issave = kgen_invoke(0) 
    ELSE 
        kgen_openmp_issave = -1 
    END IF   
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
    kgen_openmp_issave = -1 
    IF (kgen_issave(0)) THEN 
          
        !extern output variables 
        CALL kw_externs_out_def_vars(kgen_unit) 
          
        !local output variables 
        WRITE (UNIT = kgen_unit) ifg 
        CALL kw_rad_diffusion_grid__grid_ptr(sg, kgen_unit, "sg", .FALSE.) 
        CLOSE (UNIT=kgen_unit) 
        WRITE (*, *) "Collected Kernel Input/Ouput state from: ", kgen_mymid, 0, kgen_invoke(0) 
    END IF   
    IF (.NOT. kgen_ischecked(0) .AND. kgen_islast(0)) THEN 
        kgen_ischecked(0) = .TRUE. 
        OPEN (NEWUNIT=kgen_stopunit, FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0))), STATUS="NEW", IOSTAT=kgen_ierr) 
        IF (kgen_ierr == 0) THEN 
            CLOSE (UNIT=kgen_stopunit, STATUS="KEEP") 
        END IF   
        CALL mpi_barrier(1140850688, kgen_ierr) 
        IF (kgen_mymid == 0) THEN 
            CALL kgen_check_stop(INT(0), INT(15), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
        END IF   
        IF (ALL(kgen_isstop) .and. kgen_mymid == 0) THEN 
            OPEN (NEWUNIT=kgen_stopunit, FILE="/p/work1/wohlbier/devel/aster/kgen/kernel/kgen_statefile.lst", STATUS="REPLACE", &
            &FORM="FORMATTED", ACCESS="SEQUENTIAL", ACTION="WRITE", IOSTAT=kgen_ierr) 
            IF (kgen_ierr .EQ. 0) THEN 
                FLUSH (kgen_stopunit) 
                CALL kgen_write_list(kgen_stopunit, INT(0), INT(15), INT(0), INT(0), INT(0), INT(0)) 
                FLUSH (kgen_stopunit) 
                CLOSE (UNIT=kgen_stopunit, STATUS="KEEP") 
                WRITE (*, *) "Stopping application..." 
            END IF   
        END IF   
        IF (kgen_mymid == 0 .and. .not. ALL(kgen_isstop)) THEN 
            CALL mpi_abort(1140850688, 0, kgen_ierr) 
        ELSE 
            CALL mpi_finalize(kgen_ierr) 
            STOP 
        END IF   
    END IF   
    kgen_invoke(0) = kgen_invoke(0) + 1 
    !END OF KGEN REGION 
    !


    !$kgen end_callsite multigrid
    !-----------------------------------------------------------------------
    !  Deallocate subgrids
    !-----------------------------------------------------------------------
    ! Cycle down to the coarsest (bottom) subgrid

    !
    !
    sg => top_grid
    do while(associated(sg%crse))
       sg => sg%crse
    end do
    ! Cosequently deallocate subgrids from bottom to top
    !
    !
    do while(associated(sg%fine))
       sg => sg%fine
       deallocate(sg%crse)
    end do
    nullify(top_grid)
    deallocate(sg)
    !-----------------------------------------------------------------------

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
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
      


    FUNCTION kgen_get_newunit() RESULT ( new_unit ) 
        INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000 
        LOGICAL :: is_opened 
        INTEGER :: nunit, new_unit, counter 
          
        new_unit = -1 
        DO counter=UNIT_MIN, UNIT_MAX 
            INQUIRE (UNIT=counter, OPENED=is_opened) 
            IF (.NOT. is_opened) THEN 
                new_unit = counter 
                EXIT 
            END IF   
        END DO   
    END FUNCTION kgen_get_newunit 
      
    SUBROUTINE kgen_init_vars(kgen_mymid, mpi_s, mpi_e, omp_s, omp_e, invoke_e, msize, osize, lockpath, last_invoke, isstop) 
        INTEGER, INTENT(IN) :: kgen_mymid, mpi_s, mpi_e, omp_s, omp_e, invoke_e, msize, osize 
        INTEGER, INTENT(INOUT), DIMENSION(0:osize-1) :: last_invoke 
        LOGICAL, INTENT(INOUT), DIMENSION(0:msize-1,0:osize-1) :: isstop 
        CHARACTER(LEN=1024), INTENT(INOUT), DIMENSION(0:msize-1,0:osize-1) :: lockpath 
        INTEGER :: mpi_idx, openmp_idx, temp_unit, ierr 
        DO mpi_idx=mpi_s,mpi_e 
            DO openmp_idx=omp_s,omp_e 
                WRITE (lockpath(mpi_idx, openmp_idx), FMT="(A,I0,A,I0)") "/p/work1/wohlbier/devel/aster/kgen/kernel/done.", &
                &mpi_idx, ".", openmp_idx 
                isstop(mpi_idx, openmp_idx) = .FALSE. 
                last_invoke(openmp_idx) = MAX( last_invoke(openmp_idx), invoke_e) 
                IF (kgen_mymid .EQ. 0) THEN 
                    OPEN (NEWUNIT=temp_unit, FILE=lockpath(mpi_idx, openmp_idx), STATUS="OLD", IOSTAT=ierr) 
                    IF (ierr .EQ. 0) THEN 
                        CLOSE (UNIT=temp_unit, STATUS="DELETE") 
                    END IF   
                END IF   
            END DO   
        END DO   
    END SUBROUTINE kgen_init_vars 
      
    SUBROUTINE kgen_check_save(mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e, mymid, myoid, osize, invoke, last_invoke, issave, &
    &islast) 
        INTEGER, INTENT(IN) :: mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e, osize, mymid, myoid 
        INTEGER, INTENT(IN), DIMENSION(0:osize-1) :: invoke, last_invoke 
        LOGICAL, INTENT(OUT), DIMENSION(0:osize-1) :: issave, islast 
        IF ((mymid .GE. mpi_s) .AND. (mymid .LE. mpi_e)) THEN 
            IF ((myoid .GE. omp_s) .AND. (myoid .LE. omp_e)) THEN 
                IF ((invoke(myoid) .GE. invoke_s) .AND. (invoke(myoid) .LE. invoke_e)) THEN 
                    issave(myoid) = .TRUE. 
                END IF   
                IF (invoke(myoid) .GE. last_invoke(myoid)) THEN 
                    islast(myoid) = .TRUE. 
                END IF   
            END IF   
        END IF   
    END SUBROUTINE kgen_check_save 
      
    SUBROUTINE kgen_check_stop(mpi_s, mpi_e, omp_s, omp_e, msize, osize, myoid, lockpath, isstop) 
        INTEGER, INTENT(IN) :: mpi_s, mpi_e, omp_s, omp_e, msize, osize, myoid 
        CHARACTER(LEN=1024), INTENT(IN), DIMENSION(0:msize-1,0:osize-1) :: lockpath 
        LOGICAL, INTENT(OUT), DIMENSION(0:msize-1,0:osize-1) :: isstop 
        INTEGER :: mpi_idx, openmp_idx, ierr, myunit 
        DO mpi_idx=mpi_s,mpi_e 
            DO openmp_idx=omp_s,omp_e 
                IF (.NOT. isstop(mpi_idx, openmp_idx)) THEN 
                    OPEN (NEWUNIT=myunit, FILE=TRIM(ADJUSTL(lockpath(mpi_idx, openmp_idx))), STATUS="OLD", ACTION="READ", &
                    &IOSTAT=ierr) 
                    IF (ierr .EQ. 0) THEN 
                        isstop(mpi_idx, openmp_idx) = .TRUE. 
                        CLOSE (UNIT=myunit) 
                    END IF   
                END IF   
            END DO   
        END DO   
    END SUBROUTINE kgen_check_stop 
      
    SUBROUTINE kgen_write_list(myunit, mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e) 
        INTEGER, INTENT(IN) :: myunit, mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e 
        INTEGER :: mpi_idx, openmp_idx, invoke_idx, temp_unit, ierr 
        CHARACTER(LEN=16) :: mpi_str, openmp_str, invoke_str 
        DO mpi_idx=mpi_s,mpi_e 
            WRITE (mpi_str, "(I16)") mpi_idx 
            DO openmp_idx=omp_s,omp_e 
                WRITE (openmp_str, "(I16)") openmp_idx 
                DO invoke_idx=invoke_s,invoke_e 
                    WRITE (invoke_str, "(I16)") invoke_idx 
                    WRITE (UNIT = myunit, FMT="(A)") "multigrid." // TRIM(ADJUSTL(mpi_str)) // "." // TRIM(ADJUSTL(openmp_str)) &
                    &// "." // TRIM(ADJUSTL(invoke_str)) 
                END DO   
                OPEN (NEWUNIT=temp_unit, FILE="/p/work1/wohlbier/devel/aster/kgen/kernel/done." // TRIM(ADJUSTL(mpi_str)) // "." &
                &// TRIM(ADJUSTL(openmp_str)), STATUS="OLD", IOSTAT=ierr) 
                IF (ierr .EQ. 0) THEN 
                    CLOSE (UNIT=temp_unit, STATUS="DELETE") 
                END IF   
            END DO   
        END DO   
    END SUBROUTINE kgen_write_list 
      
    SUBROUTINE kgen_error_stop(ierr, errmsg) 
        INTEGER, INTENT(IN) :: ierr 
        CHARACTER(LEN=*), INTENT(IN) :: errmsg 
        INTEGER :: kgen_ierr 
        IF (ierr /= 0) THEN 
            WRITE (*, *) errmsg 
            CALL mpi_abort(1140850688, 0, kgen_ierr) 
        END IF   
    END SUBROUTINE kgen_error_stop 
      
    SUBROUTINE kgen_print_counter(counter) 
        INTEGER, INTENT(IN) :: counter 
        WRITE (*, *) "KGEN writes input state variables at count = ", counter 
    END SUBROUTINE kgen_print_counter 
      
    !write state subroutine for kw_rad_diffusion_integer__ik_ptr 
    SUBROUTINE kw_rad_diffusion_integer__ik_ptr(var, kgen_unit, printname, printvar) 
        INTEGER(KIND=ik), INTENT(IN), POINTER :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        CHARACTER(LEN=*), INTENT(IN) :: printname 
        LOGICAL, INTENT(IN), OPTIONAL :: printvar 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        kgen_istrue = .TRUE. 
        IF (.NOT. ASSOCIATED(var)) THEN 
            kgen_istrue = .FALSE. 
        END IF   
        WRITE (UNIT = kgen_unit) kgen_istrue 
        IF (kgen_istrue) THEN 
            WRITE (UNIT = kgen_unit) var 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                WRITE (*, *) "KGEN DEBUG: " // printname // " = ", var 
            END IF   
        END IF   
          
    END SUBROUTINE kw_rad_diffusion_integer__ik_ptr 
      
    !write state subroutine for kw_rad_diffusion_grid__grid_ptr 
    SUBROUTINE kw_rad_diffusion_grid__grid_ptr(var, kgen_unit, printname, printvar) 
        TYPE(grid), INTENT(IN), POINTER :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        CHARACTER(LEN=*), INTENT(IN) :: printname 
        LOGICAL, INTENT(IN), OPTIONAL :: printvar 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        kgen_istrue = .TRUE. 
        IF (.NOT. ASSOCIATED(var)) THEN 
            kgen_istrue = .FALSE. 
        END IF   
        WRITE (UNIT = kgen_unit) kgen_istrue 
        IF (kgen_istrue) THEN 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                CALL kw_def_rad_grid(var, kgen_unit, printname, .TRUE.) 
            ELSE 
                CALL kw_def_rad_grid(var, kgen_unit, printname, .FALSE.) 
            END IF   
        END IF   
          
    END SUBROUTINE kw_rad_diffusion_grid__grid_ptr 
      
  end subroutine rad_diffusion
end module raddiff_mod !jgw!
BLOCK DATA KGEN 
    INTEGER :: kgen_mpirank = 0 
    INTEGER :: kgen_openmp_issave = -1 
    COMMON / state / kgen_mpirank, kgen_openmp_issave 
END BLOCK DATA KGEN 