
! KGEN-generated Fortran source file
!
! Filename    : compute.F90
! Generated at: 2015-07-31 09:02:52
! KGEN version: 0.4.12



    MODULE compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        PUBLIC add
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE add(a, b, r1, r2, r3)
            LOGICAL, intent(in) :: a
            LOGICAL, intent(in) :: b
            REAL(KIND=4), dimension(:,:), intent(in) :: r2
            REAL(KIND=4), dimension(:,:), intent(in) :: r1
            REAL(KIND=4), dimension(:,:), intent(out) :: r3
            !print *, "a=", a
                if(a .neqv. b) then
                        r3 = r1 + r2
                end if
                if(a .eqv. b) then
                        r3 = r1-r2
                end if
                print *, "r3= ", r3
        END SUBROUTINE 
    END MODULE 
