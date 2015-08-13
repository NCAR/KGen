program test_top

        USE calling_module

        real(kind=4), dimension(2,2) :: a, b, c
        common a, b, c

        a(:,:) = 2.00
        b(:,:) = 2.00

        call calling_subroutine

end program
