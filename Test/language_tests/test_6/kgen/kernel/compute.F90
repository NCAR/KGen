
! KGEN-generated Fortran source file
!
! Filename    : compute.F90
! Generated at: 2015-07-31 09:02:47
! KGEN version: 0.4.12



    MODULE compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        INTEGER(KIND=4), parameter :: arraysize=10
        PUBLIC add
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE add(a, b, c)
            REAL(KIND=4), dimension(arraysize), intent(in) :: a
            REAL(KIND=4), dimension(arraysize), intent(in) :: b
            REAL(KIND=4), dimension(arraysize), intent(out) :: c
            !print *, "a=", a
                c = b + a
            !print *, "c= ", c
        END SUBROUTINE 
    END MODULE 
