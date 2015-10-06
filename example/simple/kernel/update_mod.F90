
! KGEN-generated Fortran source file
!
! Filename    : update_mod.F90
! Generated at: 2015-10-02 17:13:17
! KGEN version: 0.5.2



    MODULE update_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE calc_mod, ONLY: calc
        PUBLIC update
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE update(kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            INTEGER, PARAMETER :: maxiter=1000
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            INTEGER :: i
            INTEGER :: j
            INTEGER, dimension(4, 4) :: output
            INTEGER :: ref_output(4,4)
                    tolerance = 1.E-14
                    CALL kgen_init_check(check_status, tolerance)
                    READ(UNIT=kgen_unit) i
                    READ(UNIT=kgen_unit) j
                    READ(UNIT=kgen_unit) output

                    READ(UNIT=kgen_unit) ref_output


                    ! call to kernel
                CALL calc(i, j, output)
                    ! kernel verification for output variables
                    CALL kgen_verify_integer_4_dim2( "output", check_status, output, ref_output)
                    CALL kgen_print_check("calc", check_status)
                    CALL system_clock(start_clock, rate_clock)
                    DO kgen_intvar=1,maxiter
                        CALL calc(i, j, output)
                    END DO
                    CALL system_clock(stop_clock, rate_clock)
                    WRITE(*,*)
                    PRINT *, "calc : Time per call (usec): ", 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_integer_4_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                integer(KIND=4), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_integer_4_dim2


        ! verify subroutines
            SUBROUTINE kgen_verify_integer_4_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer, intent(in), DIMENSION(:,:) :: var, ref_var
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
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                    end if
                
                    check_status%numFatal = check_status%numFatal+1
                END IF
            END SUBROUTINE kgen_verify_integer_4_dim2

        END SUBROUTINE 
    END MODULE 
