module kernel

        USE types_mod

        implicit none

        !integer(kind=4), parameter :: np =4
        integer(kind=4), parameter :: niter =10

        public add

        contains

        subroutine add(elem1, elem2)

                type(elem_mimic), intent(inout) :: elem1
                type(elem_mimic2), intent(inout) :: elem2

                elem1%c(:,:) = (elem2%level3_1%param1(:,:) + &
                    elem2%level3_1%param2(:,:)) * (elem1%a(:,:) + elem1%b(:,:))

                !print *, "t1(1)%elem1%c(:,:) = ", t1(1)%elem1%c(:,:)

        end subroutine add

end module
