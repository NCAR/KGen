MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        INTEGER :: i, j
        real, dimension(4, 4) :: out2, out3, output
        INTEGER :: kgen_invokes 
        COMMON / state / kgen_invokes 
        DO i=1, 4
            DO j=1, 4
                !$kgen begin_callsite calc
                kgen_invokes = kgen_invokes + 1 
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
                !$kgen end_callsite
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
    END SUBROUTINE
END MODULE
  
SUBROUTINE gen_coverage(fileid, lineid) 
      
    CHARACTER(LEN=4096) :: datapath 
    CHARACTER(LEN=6) :: filestr 
    CHARACTER(LEN=6) :: linestr 
    CHARACTER(LEN=10) :: rankstr 
    CHARACTER(LEN=6) :: threadstr 
    CHARACTER(LEN=1) :: dummychar 
    INTEGER :: ierror 
    CHARACTER(LEN=10) :: numranksstr 
    INTEGER :: kgen_invokes 
    CHARACTER(LEN=6) :: numthreadsstr 
    INTEGER :: invokes, visits, intnum 
    INTEGER :: mpiunit, ompunit, dataunit 
    LOGICAL :: istrue 
    INTEGER, INTENT(IN) :: fileid, lineid 
    COMMON / state / kgen_invokes 
      
    WRITE (filestr, "(I6)") fileid 
    WRITE (linestr, "(I6)") lineid 
    WRITE (rankstr, "(I1)") 0 
    WRITE (threadstr, "(I1)") 0 
    INQUIRE (FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/0/mpi", EXIST=istrue) 
    IF (.NOT. istrue) THEN 
        WRITE (numranksstr, "(I10)") 1 
        OPEN (NEWUNIT=mpiunit, FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/0/mpi", &
        &STATUS="NEW", ACTION="WRITE", FORM="FORMATTED", IOSTAT=ierror) 
        IF (ierror .EQ. 0) THEN 
            WRITE (UNIT=mpiunit, FMT="(A)") TRIM(ADJUSTL(numranksstr)) 
            CLOSE (UNIT=mpiunit) 
        END IF   
    END IF   
    INQUIRE (FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/0/openmp", EXIST=istrue) 
    IF (.NOT. istrue) THEN 
        WRITE (numthreadsstr, "(I1)") 1 
        OPEN (NEWUNIT=ompunit, FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/0/openmp", &
        &STATUS="NEW", ACTION="WRITE", FORM="FORMATTED", IOSTAT=ierror) 
        IF (ierror .EQ. 0) THEN 
            WRITE (UNIT=ompunit, FMT="(A)") TRIM(ADJUSTL(numthreadsstr)) 
            CLOSE (UNIT=ompunit) 
        END IF   
    END IF   
    WRITE (datapath, *) "/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/model/__data__/0/" // &
    &TRIM(ADJUSTL(rankstr)) // "/" // TRIM(ADJUSTL(threadstr)) 
    INQUIRE (FILE=TRIM(ADJUSTL(datapath)) // "/" // TRIM(ADJUSTL(filestr)) // "." // TRIM(ADJUSTL(linestr)), EXIST=istrue) 
    IF (istrue) THEN 
        OPEN (NEWUNIT=dataunit, FILE=TRIM(ADJUSTL(datapath)) // "/" // TRIM(ADJUSTL(filestr)) // "." // TRIM(ADJUSTL(linestr)), &
        &STATUS="OLD", ACTION="READWRITE", FORM="FORMATTED", ACCESS="DIRECT", RECL=33, IOSTAT=ierror) 
        IF (ierror .EQ. 0) THEN 
            INQUIRE (UNIT=dataunit, SIZE=intnum) 
            READ (UNIT=dataunit, REC=intnum/33, FMT="(2I16,1A)") invokes, visits, dummychar 
            IF (invokes .EQ. kgen_invokes) THEN 
                visits = visits + 1 
                WRITE (UNIT=dataunit, REC=intnum/33, FMT="(2I16,1A)") invokes, visits, NEW_LINE("A") 
            ELSE 
                WRITE (UNIT=dataunit, REC=(intnum/33+1), FMT="(2I16,1A)") kgen_invokes, 1, NEW_LINE("A") 
            END IF   
            CLOSE (UNIT=dataunit) 
        END IF   
    ELSE 
        OPEN (NEWUNIT=dataunit, FILE=TRIM(ADJUSTL(datapath)) // "/" // TRIM(ADJUSTL(filestr)) // "." // TRIM(ADJUSTL(linestr)), &
        &STATUS="NEW", ACTION="READWRITE", FORM="FORMATTED", ACCESS="DIRECT", RECL=33, IOSTAT=ierror) 
        IF (ierror .NE. 0) THEN 
            CALL SYSTEM("mkdir -p " // TRIM(ADJUSTL(datapath))) 
            OPEN (NEWUNIT=dataunit, FILE=TRIM(ADJUSTL(datapath)) // "/" // TRIM(ADJUSTL(filestr)) // "." // &
            &TRIM(ADJUSTL(linestr)), STATUS="NEW", ACTION="READWRITE", FORM="FORMATTED", ACCESS="DIRECT", RECL=33, IOSTAT=ierror) 
        END IF   
        IF (ierror .EQ. 0) THEN 
            WRITE (UNIT=dataunit, REC=1, FMT="(2I16,1A)") 0, 1, NEW_LINE("A") 
            CLOSE (UNIT=dataunit) 
        END IF   
    END IF   
      
END SUBROUTINE gen_coverage 
  
BLOCK DATA KGEN 
    INTEGER :: kgen_invokes = 0 
    COMMON / state / kgen_invokes 
END BLOCK DATA KGEN 