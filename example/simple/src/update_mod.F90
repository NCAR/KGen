MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        INTEGER, dimension(ROW, COLUMN) :: output
        DO i=1, COLUMN
            DO j=1, ROW
                CALL calc(i, j, output)
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE
