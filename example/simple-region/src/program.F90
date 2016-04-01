PROGRAM demo
    USE update_mod, only : update
    INTEGER t
    DO t=1,10
        CALL update
    END DO
END PROGRAM
