! comment0
module calling_module

        USE kernel, only : add

        public calling_subroutine

        contains

        ! comment1
        subroutine calling_subroutine()

                real(kind=4), dimension(2,2) :: ar1, ar2, ar3

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0

                ! comment2

                call add(ar1, ar2, ar3)

                ! comment3
                print *, "ar3 =", ar3

        end subroutine

        ! comment4

        subroutine dummy()
            ! comment5
        end subroutine
        ! comment6
end module

! comment7
