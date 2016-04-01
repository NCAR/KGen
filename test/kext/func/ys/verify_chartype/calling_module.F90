module calling_module

        USE kernel, only : add

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                real(kind=4), dimension(2,2) :: ar1, ar2, ar3
                character(len=5) :: str1
                character(len=10) :: str2

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0

                str1 = '01234'
                str2 = '0123456789'

                call add(ar1, ar2, ar3, str1, str2)

                print *, "ar3 =", ar3
                print *, str1
                print *, str2

        end subroutine

end module
