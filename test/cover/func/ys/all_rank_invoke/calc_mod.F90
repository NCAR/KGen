MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output)
        INTEGER, INTENT(IN) :: i, j
        INTEGER, INTENT(OUT), dimension(:,:) :: output
        CALL print_msg('start') 
        output(i,j) = i + j
        CALL print_msg('finish') 
    END SUBROUTINE
    SUBROUTINE print_msg(msg)
        CHARACTER(*), INTENT(IN) :: msg
        print *, msg
    END SUBROUTINE
END MODULE
