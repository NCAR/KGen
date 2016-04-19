module calling_module

        USE kernel, only : add

        public calling_subroutine

        contains

        subroutine calling_subroutine()
                logical, parameter :: is_true = .TRUE.
                real(kind=4), dimension(2,2) :: ar1, ar2, ar3, ar4

                ar1(:,:) = 1.0
                ar2(:,:) = 1.0
                ar3(:,:) = 1.0

                if (is_true) ar4 = add(ar1, ar2, ar3)

                print *, "ar4 =", ar4

        end subroutine

end module
