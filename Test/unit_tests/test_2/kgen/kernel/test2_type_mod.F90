
! KGEN-generated Fortran source file
!
! Filename    : test2_type_mod.F90
! Generated at: 2015-08-04 10:15:53
! KGEN version: 0.4.13



    MODULE test2_type_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        IMPLICIT NONE
        INTEGER(KIND=4), parameter :: real_kind=4
        INTEGER(KIND=4), parameter :: np = 4
        TYPE, public :: level_3
            REAL(KIND=real_kind) :: param1(np,np)
            REAL(KIND=real_kind) :: param2(np,np)
        END TYPE level_3
        TYPE, public :: elem_mimic
            REAL(KIND=real_kind), dimension(np,np) :: a
            REAL(KIND=real_kind), dimension(np,np) :: b
            REAL(KIND=real_kind), dimension(np,np) :: c
        END TYPE elem_mimic
        TYPE, public :: elem_mimic2
            REAL(KIND=real_kind) :: d
            REAL(KIND=real_kind) :: e
            REAL(KIND=real_kind) :: f
            TYPE(level_3) :: level3_1
        END TYPE elem_mimic2
        TYPE, public :: complex_type
            REAL(KIND=real_kind) :: element_1
            TYPE(elem_mimic) :: elem1
            TYPE(elem_mimic2) :: elem2
        END TYPE complex_type

    ! read interface
    PUBLIC kgen_read
    INTERFACE kgen_read
        MODULE PROCEDURE kgen_read_level_3
        MODULE PROCEDURE kgen_read_elem_mimic
        MODULE PROCEDURE kgen_read_elem_mimic2
        MODULE PROCEDURE kgen_read_complex_type
    END INTERFACE kgen_read

    PUBLIC kgen_verify
    INTERFACE kgen_verify
        MODULE PROCEDURE kgen_verify_level_3
        MODULE PROCEDURE kgen_verify_elem_mimic
        MODULE PROCEDURE kgen_verify_elem_mimic2
        MODULE PROCEDURE kgen_verify_complex_type
    END INTERFACE kgen_verify

    CONTAINS

    ! write subroutines
    ! No module extern variables
    SUBROUTINE kgen_read_level_3(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(level_3), INTENT(out) :: var
        READ(UNIT=kgen_unit) var%param1
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%param1 **", var%param1
        END IF
        READ(UNIT=kgen_unit) var%param2
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%param2 **", var%param2
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_read_elem_mimic(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(elem_mimic), INTENT(out) :: var
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
    SUBROUTINE kgen_read_elem_mimic2(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(elem_mimic2), INTENT(out) :: var
        READ(UNIT=kgen_unit) var%d
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%d **", var%d
        END IF
        READ(UNIT=kgen_unit) var%e
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%e **", var%e
        END IF
        READ(UNIT=kgen_unit) var%f
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%f **", var%f
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_level_3(var%level3_1, kgen_unit, printvar=printvar//"%level3_1")
        ELSE
            CALL kgen_read_level_3(var%level3_1, kgen_unit)
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_read_complex_type(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(complex_type), INTENT(out) :: var
        READ(UNIT=kgen_unit) var%element_1
        IF ( PRESENT(printvar) ) THEN
            print *, "** " // printvar // "%element_1 **", var%element_1
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_elem_mimic(var%elem1, kgen_unit, printvar=printvar//"%elem1")
        ELSE
            CALL kgen_read_elem_mimic(var%elem1, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_elem_mimic2(var%elem2, kgen_unit, printvar=printvar//"%elem2")
        ELSE
            CALL kgen_read_elem_mimic2(var%elem2, kgen_unit)
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_verify_level_3(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(level_3), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_real_real_kind_dim2("param1", dtype_check_status, var%param1, ref_var%param1)
        CALL kgen_verify_real_real_kind_dim2("param2", dtype_check_status, var%param2, ref_var%param2)
        IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
            check_status%numIdentical = check_status%numIdentical + 1
        ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
            check_status%numFatal = check_status%numFatal + 1
        ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
            check_status%numWarning = check_status%numWarning + 1
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_verify_elem_mimic(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(elem_mimic), INTENT(IN) :: var, ref_var

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
    SUBROUTINE kgen_verify_elem_mimic2(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(elem_mimic2), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_real_real_kind("d", dtype_check_status, var%d, ref_var%d)
        CALL kgen_verify_real_real_kind("e", dtype_check_status, var%e, ref_var%e)
        CALL kgen_verify_real_real_kind("f", dtype_check_status, var%f, ref_var%f)
        CALL kgen_verify_level_3("level3_1", dtype_check_status, var%level3_1, ref_var%level3_1)
        IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
            check_status%numIdentical = check_status%numIdentical + 1
        ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
            check_status%numFatal = check_status%numFatal + 1
        ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
            check_status%numWarning = check_status%numWarning + 1
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_verify_complex_type(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(complex_type), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_real_real_kind("element_1", dtype_check_status, var%element_1, ref_var%element_1)
        CALL kgen_verify_elem_mimic("elem1", dtype_check_status, var%elem1, ref_var%elem1)
        CALL kgen_verify_elem_mimic2("elem2", dtype_check_status, var%elem2, ref_var%elem2)
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

        SUBROUTINE kgen_verify_real_real_kind( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=real_kind), intent(in) :: var, ref_var
            check_status%numTotal = check_status%numTotal + 1
            IF ( var == ref_var ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                endif
            ELSE
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    if(check_status%verboseLevel > 2) then
                        WRITE(*,*) "KERNEL: ", var
                        WRITE(*,*) "REF.  : ", ref_var
                    end if
                end if
                check_status%numFatal = check_status%numFatal + 1
            END IF
        END SUBROUTINE kgen_verify_real_real_kind

    END MODULE test2_type_mod
