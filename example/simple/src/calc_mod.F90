MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output)
        INTEGER, INTENT(IN) :: i, j
        INTEGER, INTENT(OUT), dimension(:,:) :: output
          ! Also a comment
        output(i,j) = i + j
    END SUBROUTINE
END MODULE
