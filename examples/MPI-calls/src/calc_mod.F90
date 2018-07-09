MODULE calc_mod
    PUBLIC calc
CONTAINS
    SUBROUTINE calc(i, j, output, out2, out3, rank, nranks)
        INCLUDE 'mpif.h'
        INTEGER, INTENT(IN) :: rank, nranks
        INTEGER, INTENT(IN) :: i, j
        INTEGER, INTENT(OUT), dimension(:,:) :: out3, output, out2
        INTEGER :: gsum(nranks)

        gsum = 0

          ! Also a comment
!$kgen coverage test1
        IF ( i > j ) THEN
            output(i,j) = i - j
            out2(i, j) = 2*(i-j)
            out3(i, j) = 3*(i-j)
!$kgen coverage test2
        ELSE
            output(i,j) = j - i
            out2(i, j) = 2*(j-i)
            out3(i, j) = 3*(j-i)
        END IF

        !$kgen exclude
        CALL mpi_gather(sum(output), 1, MPI_INTEGER, &
            gsum, 1, MPI_INTEGER, &
            0, MPI_COMM_WORLD, error)
        !$kgen write gsum
    END SUBROUTINE
END MODULE
