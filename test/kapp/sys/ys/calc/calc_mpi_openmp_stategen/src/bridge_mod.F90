MODULE bridge_mod
    USE update_mod, only : update
    IMPLICIT NONE
    PUBLIC bridge
CONTAINS
    SUBROUTINE bridge(rank, nranks)
        INTEGER, INTENT(IN) :: rank, nranks
        INTEGER i

        !$OMP PARALLEL DO &
        !$OMP& shared(rank, nranks)
        DO i=1,10
            !$kgen callsite update
            CALL update(rank, nranks)
        END DO
        !$OMP END PARALLEL DO

    END SUBROUTINE
END MODULE
