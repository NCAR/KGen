PROGRAM demo
    USE update_mod, only : update
    IMPLICIT NONE
    INTEGER t

    DO t=1,10
        CALL update()
    END DO
END PROGRAM
