MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output)
        INTEGER, INTENT(IN) :: i, j
        INTEGER, INTENT(OUT), dimension(:,:) :: output
        if ( i .eq. j ) then
            output(i,j) = 0
        else
            output(i,j) = i + j
        end if
    END SUBROUTINE
    SUBROUTINE print_msg(msg)
        CHARACTER(*), INTENT(IN) :: msg
        print *, msg
    END SUBROUTINE
END MODULE
