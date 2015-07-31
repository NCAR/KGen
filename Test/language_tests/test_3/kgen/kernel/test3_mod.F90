
! KGEN-generated Fortran source file
!
! Filename    : test3_mod.F90
! Generated at: 2015-07-31 10:22:34
! KGEN version: 0.4.12



    MODULE test3_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE compute, ONLY: add
        PUBLIC test_3
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE test_3(kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            INTEGER(KIND=4), parameter :: real_kind = 4
            REAL(KIND=real_kind), allocatable :: array_1(:,:,:,:)
            REAL(KIND=real_kind), allocatable :: array_2(:,:,:,:)
            REAL(KIND=real_kind), allocatable :: array_3(:,:,:,:)
            REAL(KIND=real_kind), allocatable :: ref_array_3(:,:,:,:)
            tolerance = 1.E-14
            CALL kgen_init_check(check_status, tolerance)
            CALL kgen_read_real_real_kind_dim4_alloc(array_1, kgen_unit)
            CALL kgen_read_real_real_kind_dim4_alloc(array_2, kgen_unit)
            CALL kgen_read_real_real_kind_dim4_alloc(array_3, kgen_unit)

            CALL kgen_read_real_real_kind_dim4_alloc(ref_array_3, kgen_unit)


            ! call to kernel
                call add(array_1, array_2, array_3)
            ! kernel verification for output variables
            CALL kgen_verify_real_real_kind_dim4_alloc( "array_3", check_status, array_3, ref_array_3)
            CALL kgen_print_check("add", check_status)
            CALL system_clock(start_clock, rate_clock)
            DO kgen_intvar=1,10
                CALL add(array_1, array_2, array_3)
            END DO
            CALL system_clock(stop_clock, rate_clock)
            WRITE(*,*)
            PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
            !                print *, "a3 = ", array_3
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_real_kind_dim4_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=real_kind), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4
                INTEGER, DIMENSION(2,4) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    READ(UNIT = kgen_unit) kgen_bound(1, 4)
                    READ(UNIT = kgen_unit) kgen_bound(2, 4)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_real_kind_dim4_alloc


        ! verify subroutines
            SUBROUTINE kgen_verify_real_real_kind_dim4_alloc( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=real_kind), intent(in), DIMENSION(:,:,:,:), ALLOCATABLE :: var, ref_var
                real(KIND=real_kind) :: nrmsdiff, rmsdiff
                real(KIND=real_kind), allocatable, DIMENSION(:,:,:,:) :: temp, temp2
                integer :: n
                IF ( ALLOCATED(var) ) THEN
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
                END IF
            END SUBROUTINE kgen_verify_real_real_kind_dim4_alloc

        END SUBROUTINE 
    END MODULE 
