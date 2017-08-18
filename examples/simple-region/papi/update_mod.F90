MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        real, dimension(4, 4) :: out2, out3, output
        CHARACTER(LEN=4096) :: datapath 
        include "/glade/apps/opt/papi/5.3.2/intel/15.0.0/include/f90papi.h" 
        INTEGER :: kgen_papierr 
        INTEGER, DIMENSION(1) :: kgen_papi_events = PAPI_TOT_INS 
        INTEGER(KIND=8), DIMENSION(2) :: kgen_measure 
        INTEGER, SAVE :: kgen_invokes = 0 
        INTEGER :: dataunit 
        CHARACTER(LEN=10) :: rankstr 
        CHARACTER(LEN=6) :: threadstr 
        DO i=1, 4
            DO j=1, 4
                !$kgen begin_callsite calc
                kgen_invokes = kgen_invokes + 1 
                CALL PAPIF_start_counters(kgen_papi_events(1), 1, kgen_papierr) 
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
                CALL PAPIF_read_counters(kgen_measure(1), 1, kgen_papierr) 
                CALL PAPIF_stop_counters(kgen_measure(2), 1, kgen_papierr) 
                !$kgen end_callsite
                WRITE (rankstr, "(I1)") 0 
                WRITE (threadstr, "(I1)") 0 
                WRITE (datapath, *) "/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/2/" // &
                &TRIM(ADJUSTL(rankstr)) // "." // TRIM(ADJUSTL(threadstr)) 
                OPEN (NEWUNIT=dataunit, FILE=TRIM(ADJUSTL(datapath)), ACTION="WRITE", ACCESS="APPEND", IOSTAT=kgen_papierr) 
                IF (kgen_papierr .EQ. 0) THEN 
                    WRITE (UNIT=dataunit, FMT="(I16,1X,I32)") kgen_invokes, kgen_measure(1) 
                    CLOSE (UNIT=dataunit) 
                ELSE 
                    PRINT *, "FILE OPEN ERROR: ", TRIM(ADJUSTL(datapath)), kgen_papierr 
                END IF   
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE