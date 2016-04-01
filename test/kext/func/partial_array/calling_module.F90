module calling_module

        USE kernel, only : add

        integer, parameter :: np_start = 10000, np_end=20000
        public calling_subroutine

        contains

        subroutine calling_subroutine()

                real(kind=4), dimension(:,:), allocatable :: ar1, ar2, ar3

                allocate(ar1(np_start:np_end,np_start:np_end))
                allocate(ar2(np_start:np_end,np_start:np_end))
                allocate(ar3(np_start:np_end,np_start:np_end))

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0

                call add(ar1, ar2, ar3)

                print *, "SUM(ar3) =", SUM(ar3)

        end subroutine

end module
