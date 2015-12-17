module calling_module

        USE kernel, only : init, add

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                real(kind=4), dimension(2,2) :: ar1, ar2, ar3

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0

                call init(ar1, ar2)
                call add(ar1, ar2, ar3)

                print *, "ar3 =", ar3

        end subroutine

end module
