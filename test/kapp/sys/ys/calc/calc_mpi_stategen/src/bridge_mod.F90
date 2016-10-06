MODULE bridge_mod
    USE update_mod, only : update
    IMPLICIT NONE
    PUBLIC bridge
CONTAINS
    SUBROUTINE bridge(rank, nranks)
        INTEGER, INTENT(IN) :: rank, nranks

        !$kgen callsite update
        CALL update(rank, nranks)

    END SUBROUTINE
END MODULE
