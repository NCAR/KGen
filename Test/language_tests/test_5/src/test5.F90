program test5

        USE test5_mod

        real(kind=4), dimension(2,2) :: a, b, c
        common a, b, c

        a(:,:) = 2.00
        b(:,:) = 2.00

        call test_5

end program
