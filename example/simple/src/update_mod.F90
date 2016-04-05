MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        real, dimension(ROW, COL) :: out2, out3, output
        DO i=1, COL
            DO j=1, ROW
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE
