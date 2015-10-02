
! KGEN-generated Fortran source file
!
! Filename    : calling_module.F90
! Generated at: 2015-10-02 10:27:55
! KGEN version: 0.5.0



    MODULE calling_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
    USE kernel, ONLY : kgen_read_mod2 => kgen_read
    USE kernel, ONLY : kgen_verify_mod2 => kgen_verify
        USE kernel, only : add
        USE kernel, only : complex_type
        PUBLIC calling_subroutine
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE calling_subroutine(kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            !integer(kind=4), parameter :: real_kind = 4
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            INTEGER, PARAMETER :: maxiter=10
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            INTEGER(KIND=4), parameter :: niter = 10
            !integer(kind=4), parameter :: np = 4
            TYPE(complex_type) :: test_type1(niter)
            TYPE(complex_type) :: ref_test_type1(niter)
            !initialization
            !perform computation
            !do i=1,niter
            tolerance = 1.E-14
            CALL kgen_init_check(check_status, tolerance)
            CALL kgen_read_complex_type_dim1(test_type1, kgen_unit)

            CALL kgen_read_complex_type_dim1(ref_test_type1, kgen_unit)


            ! call to kernel
                        call add(test_type1)
            ! kernel verification for output variables
            CALL kgen_verify_complex_type_dim1( "test_type1", check_status, test_type1, ref_test_type1)
            CALL kgen_print_check("add", check_status)
            CALL system_clock(start_clock, rate_clock)
            DO kgen_intvar=1,maxiter
                CALL add(test_type1)
            END DO
            CALL system_clock(stop_clock, rate_clock)
            WRITE(*,*)
            PRINT *, "add : Time per call (usec): ", 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
            !end do
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_complex_type_dim1(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                TYPE(complex_type), INTENT(OUT), DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    DO idx1=kgen_bound(1,1), kgen_bound(2, 1)
                    IF ( PRESENT(printvar) ) THEN
                            CALL kgen_read_mod2(var(idx1), kgen_unit, printvar=printvar)
                    ELSE
                            CALL kgen_read_mod2(var(idx1), kgen_unit)
                    END IF
                    END DO
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_complex_type_dim1


        ! verify subroutines
            RECURSIVE SUBROUTINE kgen_verify_complex_type_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                type(check_t) :: dtype_check_status
                TYPE(complex_type), intent(in), DIMENSION(:) :: var, ref_var
                integer :: idx1
                check_status%numTotal = check_status%numTotal + 1
                CALL kgen_init_check(dtype_check_status)
                DO idx1=LBOUND(var,1), UBOUND(var,1)
                    CALL kgen_verify_mod2(varname, dtype_check_status, var(idx1), ref_var(idx1))
                END DO
                IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                    check_status%numFatal = check_status%numFatal + 1
                ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                    check_status%numWarning = check_status%numWarning + 1
                END IF
            END SUBROUTINE kgen_verify_complex_type_dim1

        END SUBROUTINE calling_subroutine
    END MODULE calling_module
