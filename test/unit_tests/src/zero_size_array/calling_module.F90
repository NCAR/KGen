module calling_module

        USE kernel, only : add

        integer, parameter :: ARRSIZE = 0
        public calling_subroutine

        contains

        subroutine calling_subroutine()
                real(kind=4), dimension(2,2) :: ar1, ar2, ar3
                real(kind=4), dimension(2,0) :: ar4
                real(kind=4), dimension(2,ARRSIZE) :: ar5

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0
                ar4(:,:) = 1.0
                ar5(:,:) = 1.0

                call add(ar1, ar2, ar3, ar4, ar5)

                print *, "ar3 =", ar3

        end subroutine

end module
