
! KGEN-generated Fortran source file
!
! Filename    : test2_compute.F90
! Generated at: 2015-07-31 09:14:32
! KGEN version: 0.4.12



    MODULE test2_compute
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE test2_type_mod, only : complex_type
        IMPLICIT NONE
        !integer(kind=4), parameter :: np =4
        INTEGER(KIND=4), parameter :: niter =10
        PUBLIC compute
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE compute(t1)
            TYPE(complex_type), intent(inout), target :: t1(:)
            INTEGER :: i
                do i=1,niter
                        t1(i)%elem1%c(:,:) = (t1(i)%elem2%level3_1%param1(:,:) + &
                                             t1(i)%elem2%level3_1%param2(:,:)) * &
                                             (t1(i)%elem1%a(:,:) + &
                                             t1(i)%elem1%b(:,:))
                end do
            !print *, "t1(1)%elem1%c(:,:) = ", t1(1)%elem1%c(:,:)
        END SUBROUTINE compute
    END MODULE 
