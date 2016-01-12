module calling_module

        USE kernel
        !USE types

        public :: calling_subroutine

        contains

        subroutine calling_subroutine()

                real, dimension(2,2) :: ar2
                type(matrix), dimension(size(ar2, 1), size(ar2, 2)) :: ar1

                ar2(:,:) = 1.0

                call add(ar1, ar2)

                print *, "ar1 =", ar1

        end subroutine

end module
