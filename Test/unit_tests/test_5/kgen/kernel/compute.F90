
! KGEN-generated Fortran source file
!
! Filename    : compute.F90
! Generated at: 2015-08-04 10:16:09
! KGEN version: 0.4.13



    MODULE compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        PUBLIC add
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE add()
            COMMON a, b ! UNRESOLVED: a, b
            REAL(KIND=4), dimension(2,2) :: c
            !real(kind=4), dimension(:,:,:,:), intent(in) :: a1
            !real(kind=4), dimension(:,:,:,:), intent(in) :: a2
            !real(kind=4), dimension(:,:,:,:), intent(out) :: a3
                c = b + a ! UNRESOLVED: b, a
                print *, "c= ", c
        END SUBROUTINE 
    END MODULE 
