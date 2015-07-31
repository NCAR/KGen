
! KGEN-generated Fortran source file
!
! Filename    : compute.F90
! Generated at: 2015-07-31 09:02:38
! KGEN version: 0.4.12



    MODULE compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        PUBLIC add
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE add(a1, a2, a3)
            REAL(KIND=4), dimension(:), intent(in) :: a1
            REAL(KIND=4), dimension(:), intent(in) :: a2
            REAL(KIND=4), dimension(:), intent(out) :: a3
                a3 = a1 - a2
        END SUBROUTINE 
    END MODULE 
