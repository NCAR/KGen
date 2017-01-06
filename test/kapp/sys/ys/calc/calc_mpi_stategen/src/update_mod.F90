MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update(rank, nranks)
        INCLUDE 'mpif.h'
        INTEGER, INTENT(IN) :: rank, nranks
        INTEGER :: i, j, error
        INTEGER :: lsum, gsum(nranks)
        INTEGER, dimension(ROW, COL) :: output
        gsum = 0

        output(:,:) = 0

        DO i=1, COL
            DO j=1, ROW
                CALL calc(i, j, output)
            END DO
        END DO

        lsum = SUM(output)
        !$kgen exclude
        CALL mpi_gather(lsum, 1, MPI_INTEGER, &
            gsum, 1, MPI_INTEGER, &
            0, MPI_COMM_WORLD, error)
        !$kgen write gsum
        IF (rank == 0) THEN
            print *, 'global sum = ', SUM(gsum)
        END IF
    END SUBROUTINE
END MODULE
