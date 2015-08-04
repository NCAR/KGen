
! KGEN-generated Fortran source file
!
! Filename    : test1_mod.F90
! Generated at: 2015-08-04 10:15:47
! KGEN version: 0.4.13



    MODULE test1_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE compute, ONLY: compute_add
        PUBLIC test_1
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE test_1(kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            INTEGER(KIND=4), parameter :: real_kind = 8
            INTEGER(KIND=4), parameter :: np = 4
            TYPE derived_type1
                !real(kind=real_kind), dimension(np,np) :: a
                REAL(KIND=real_kind) :: a(np,np)
                REAL(KIND=real_kind) :: b(np,np)
                REAL(KIND=real_kind) :: c(np,np)
            END TYPE derived_type1
            TYPE(derived_type1) :: d1
                tolerance = 1.E-14
                CALL kgen_init_check(check_status, tolerance)
                CALL kgen_read_derived_type1(d1, kgen_unit)

                ! No parent output var


                ! call to kernel
                call compute_add(d1%a, d1%b, d1%c)
                ! kernel verification for output variables
                CALL kgen_print_check("compute_add", check_status)
                CALL system_clock(start_clock, rate_clock)
                DO kgen_intvar=1,10
                    CALL compute_add(d1 % a, d1 % b, d1 % c)
                END DO
                CALL system_clock(stop_clock, rate_clock)
                WRITE(*,*)
                PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
        CONTAINS
        SUBROUTINE kgen_read_derived_type1(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(derived_type1), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%a
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%a **", var%a
            END IF
            READ(UNIT=kgen_unit) var%b
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%b **", var%b
            END IF
            READ(UNIT=kgen_unit) var%c
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%c **", var%c
            END IF
        END SUBROUTINE

        ! write subroutines
        SUBROUTINE kgen_verify_derived_type1(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(derived_type1), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_real_real_kind_dim2("a", dtype_check_status, var%a, ref_var%a)
            CALL kgen_verify_real_real_kind_dim2("b", dtype_check_status, var%b, ref_var%b)
            CALL kgen_verify_real_real_kind_dim2("c", dtype_check_status, var%c, ref_var%c)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
            SUBROUTINE kgen_verify_real_real_kind_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=real_kind), intent(in), DIMENSION(:,:) :: var, ref_var
                real(KIND=real_kind) :: nrmsdiff, rmsdiff
                real(KIND=real_kind), allocatable, DIMENSION(:,:) :: temp, temp2
                integer :: n
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2)))
                
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
            END SUBROUTINE kgen_verify_real_real_kind_dim2


        ! verify subroutines
        ! No verification
        END SUBROUTINE test_1
    END MODULE test1_mod
