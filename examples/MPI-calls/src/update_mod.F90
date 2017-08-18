MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update(rank, nranks)
        INTEGER, INTENT(IN) :: rank, nranks
        INTEGER :: i, j, error
        INTEGER :: lsum
        INTEGER, dimension(ROW, COL) :: output, out2, out3

        !$kgen begin_callsite calc
        DO i=1, COL
            DO j=1, ROW
                CALL calc(i, j, output, out2, out3, rank, nranks)
            END DO
        END DO

        lsum = SUM(output)
        !$kgen end_callsite calc

        print *, 'local sum at rank', rank, ' = ', lsum

    END SUBROUTINE
END MODULE
