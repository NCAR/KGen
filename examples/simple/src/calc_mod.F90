MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output, out2, out3)
        INTEGER, INTENT(IN) :: i, j
        real, INTENT(OUT), dimension(:,:) :: out3, output, out2
          ! Also a comment
        IF ( i > j ) THEN
            output(i,j) = i - j
            out2(i, j) = 2*(i-j)
            out3(i, j) = 3*(i-j)
        ELSE
            output(i,j) = j - i
            out2(i, j) = 2*(j-i)
            out3(i, j) = 3*(j-i)
        END IF
    END SUBROUTINE
END MODULE
