
! KGEN-generated Fortran source file
!
! Filename    : compute.F90
! Generated at: 2015-07-31 09:02:18
! KGEN version: 0.4.12



    MODULE compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        INTEGER(KIND=4), parameter :: real_kind=8
        INTEGER(KIND=4), parameter :: np = 4
        PUBLIC compute_add
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE compute_add(a, b, c)
            REAL(KIND=real_kind), dimension(np,np), intent(in) :: a
            REAL(KIND=real_kind), dimension(np,np), intent(in) :: b
            REAL(KIND=real_kind), dimension(np,np), intent(out) :: c
            INTEGER(KIND=4) :: i
            INTEGER(KIND=4) :: j
                do i= 1,np
                        do j=1,np
                    !        print *, "a(", i, ")(", j, ") = ", a(i,j)
                                c(i,j) = a(i,j) + b(i,j)
                        end do
                end do
        END SUBROUTINE compute_add
    END MODULE compute
