PROGRAM demo
    USE bridge_mod, only : bridge
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER t, rank, N, err

    CALL mpi_init(err)
    CALL mpi_comm_size(MPI_COMM_WORLD,N,err)
    CALL mpi_comm_rank(MPI_COMM_WORLD,rank,err)

    DO t=1,10
        CALL bridge(rank, N)
    END DO

    CALL mpi_finalize(err)
END PROGRAM
