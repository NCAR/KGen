MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output, out2, out3)
        INTEGER, INTENT(IN) :: i, j
        real, INTENT(OUT), dimension(:,:) :: out3, output, out2
        output(i,j) = i + j
        out2(i, j) = 2*(i+j)
        out3(i, j) = 3*(i+j)
    END SUBROUTINE
END MODULE
