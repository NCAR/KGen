module kernel

        USE types_mod

        implicit none

        !integer(kind=4), parameter :: np =4
        integer(kind=4), parameter :: niter =10

        public add

        contains

        subroutine add(t1)

                type(complex_type), intent(inout), target :: t1(:)
                integer :: i

                do i=1,niter
                        t1(i)%elem1%c(:,:) = (t1(i)%elem2%level3_1%param1(:,:) + &
                                             t1(i)%elem2%level3_1%param2(:,:)) * &
                                             (t1(i)%elem1%a(:,:) + &
                                             t1(i)%elem1%b(:,:))
                        ctype%elem1%c = t1(i)%elem1%c
                end do

                atype(1)%elem2%d = t1(1)%elem2%d

                !print *, "t1(1)%elem1%c(:,:) = ", t1(1)%elem1%c(:,:)

        end subroutine add

end module
