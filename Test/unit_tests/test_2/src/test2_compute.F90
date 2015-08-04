module test2_compute

        USE test2_type_mod

        implicit none

        !integer(kind=4), parameter :: np =4
        integer(kind=4), parameter :: niter =10

        public compute

        contains

        subroutine compute(t1)

                type(complex_type), intent(inout), target :: t1(:)
                integer :: i

                do i=1,niter
                        t1(i)%elem1%c(:,:) = (t1(i)%elem2%level3_1%param1(:,:) + &
                                             t1(i)%elem2%level3_1%param2(:,:)) * &
                                             (t1(i)%elem1%a(:,:) + &
                                             t1(i)%elem1%b(:,:))
                end do

                !print *, "t1(1)%elem1%c(:,:) = ", t1(1)%elem1%c(:,:)

        end subroutine compute

end module
