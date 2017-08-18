MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        real, dimension(4, 4) :: out2, out3, output
        CHARACTER(LEN=4096) :: datapath 
        INTEGER :: ierror 
        REAL(KIND=8), DIMENSION(0:2) :: kgen_timer 
        INTEGER, SAVE :: kgen_invokes = 0 
        INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
        INTEGER :: dataunit 
        CHARACTER(LEN=10) :: rankstr 
        CHARACTER(LEN=6) :: threadstr 
        DO i=1, 4
            DO j=1, 4
                !$kgen begin_callsite calc
                kgen_invokes = kgen_invokes + 1 
                CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
                CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
                kgen_timer(0) = DBLE(kgen_start_clock) / DBLE(kgen_rate_clock) 
                kgen_timer(1) = DBLE(kgen_stop_clock) / DBLE(kgen_rate_clock) 
                kgen_timer(2) = 1.0 / DBLE(kgen_rate_clock) 
                !$kgen end_callsite
                WRITE (rankstr, "(I1)") 0 
                WRITE (threadstr, "(I1)") 0 
                WRITE (datapath, *) "/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/1/" // &
                &TRIM(ADJUSTL(rankstr)) // "." // TRIM(ADJUSTL(threadstr)) 
                OPEN (NEWUNIT=dataunit, FILE=TRIM(ADJUSTL(datapath)), ACTION="WRITE", ACCESS="APPEND", IOSTAT=ierror) 
                IF (ierror .EQ. 0) THEN 
                    WRITE (UNIT=dataunit, FMT="(I16,1X,ES34.16,1X,ES34.16,1X,ES34.16)") kgen_invokes, kgen_timer(0), &
                    &kgen_timer(1), kgen_timer(2) 
                    CLOSE (UNIT=dataunit) 
                ELSE 
                    PRINT *, "FILE OPEN ERROR: ", TRIM(ADJUSTL(datapath)), ierror 
                END IF   
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE