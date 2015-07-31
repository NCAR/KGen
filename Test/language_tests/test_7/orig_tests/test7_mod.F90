module test7_mod

        USE compute, only : add

        public test_7

        contains

        subroutine test_7()


                logical :: a, b
                real(kind=4), dimension(2,2) :: ar1, ar2, ar3

                a = .false.
                b = .true.

                ar1(:,:) = 1.0
                ar2(:,:) = 2.0

                call add(a, b, ar1, ar2, ar3)


        end subroutine

end module

                
