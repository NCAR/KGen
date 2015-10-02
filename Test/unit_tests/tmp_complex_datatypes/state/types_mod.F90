
! KGEN-generated Fortran source file
!
! Filename    : types_mod.F90
! Generated at: 2015-10-02 10:27:55
! KGEN version: 0.5.0



    MODULE types_mod
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

    ! write interface
    PUBLIC kgen_write
    INTERFACE kgen_write
        MODULE PROCEDURE kgen_write_level_3
        MODULE PROCEDURE kgen_write_elem_mimic
        MODULE PROCEDURE kgen_write_elem_mimic2
        MODULE PROCEDURE kgen_write_complex_type
    END INTERFACE kgen_write

    CONTAINS

    ! write subroutines
    SUBROUTINE kgen_write_level_3(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(level_3), INTENT(in) :: var
        WRITE(UNIT=kgen_unit) var%param1
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%param1 **", var%param1
        END IF
        WRITE(UNIT=kgen_unit) var%param2
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%param2 **", var%param2
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_write_elem_mimic(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(elem_mimic), INTENT(in) :: var
        WRITE(UNIT=kgen_unit) var%a
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%a **", var%a
        END IF
        WRITE(UNIT=kgen_unit) var%b
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%b **", var%b
        END IF
        WRITE(UNIT=kgen_unit) var%c
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%c **", var%c
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_write_elem_mimic2(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(elem_mimic2), INTENT(in) :: var
        WRITE(UNIT=kgen_unit) var%d
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%d **", var%d
        END IF
        WRITE(UNIT=kgen_unit) var%e
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%e **", var%e
        END IF
        WRITE(UNIT=kgen_unit) var%f
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%f **", var%f
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_write_level_3(var%level3_1, kgen_unit, printvar=printvar//"%level3_1")
        ELSE
            CALL kgen_write_level_3(var%level3_1, kgen_unit)
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_write_complex_type(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(complex_type), INTENT(in) :: var
        WRITE(UNIT=kgen_unit) var%element_1
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%element_1 **", var%element_1
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_write_elem_mimic(var%elem1, kgen_unit, printvar=printvar//"%elem1")
        ELSE
            CALL kgen_write_elem_mimic(var%elem1, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_write_elem_mimic2(var%elem2, kgen_unit, printvar=printvar//"%elem2")
        ELSE
            CALL kgen_write_elem_mimic2(var%elem2, kgen_unit)
        END IF
    END SUBROUTINE
    ! No module extern variables
    END MODULE types_mod
