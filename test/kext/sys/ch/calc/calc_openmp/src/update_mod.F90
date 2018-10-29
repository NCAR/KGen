MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        INTEGER, dimension(ROW, COL) :: output

        output(:,:) = 0

        !$OMP PARALLEL DO
        DO i=1, COL
            DO j=1, ROW
                CALL calc(i, j, output)
            END DO
        END DO
        !$OMP END PARALLEL DO

        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE
