MODULE update_mod
    USE calc_mod, only : calc
    PUBLIC update
CONTAINS
    SUBROUTINE update()
        !kgen variables 
        INTEGER :: kgen_openmp_issave 
        INTEGER :: kgen_mpirank 
        COMMON / state / kgen_mpirank, kgen_openmp_issave 
        LOGICAL :: kgen_istrue 
        INTEGER :: kgen_count 
        INTEGER, SAVE :: kgen_unit, kgen_stopunit 
        INTEGER, SAVE :: kgen_mymid, kgen_msize, kgen_osize, kgen_ierr 
        REAL(KIND=8), SAVE :: kgen_array_sum, kgen_realnum 
        LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: kgen_isstop 
        CHARACTER(LEN=1024), SAVE, ALLOCATABLE, DIMENSION(:,:) :: kgen_filepath, kgen_lockpath 
        LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: kgen_islast, kgen_issave, kgen_ischecked 
        INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: kgen_invoke, kgen_last_invoke 
        INTEGER :: i, j
        real, dimension(4, 4) :: out2, out3, output
        DO i=1, 4
            DO j=1, 4
                !$kgen begin_callsite calc
                !START OF KGEN REGION 
                IF (.NOT. ALLOCATED(kgen_isstop)) THEN 
                    kgen_osize = 1 
                    kgen_unit = -1 
                    kgen_stopunit = -1 
                    ALLOCATE (kgen_ischecked(0:kgen_osize-1)) 
                    ALLOCATE (kgen_islast(0:kgen_osize-1)) 
                    ALLOCATE (kgen_issave(0:kgen_osize-1)) 
                    ALLOCATE (kgen_invoke(0:kgen_osize-1)) 
                    ALLOCATE (kgen_last_invoke(0:kgen_osize-1)) 
                    kgen_ischecked(:) = .FALSE. 
                    kgen_islast(:) = .FALSE. 
                    kgen_issave(:) = .FALSE. 
                    kgen_invoke(:) = 1 
                    kgen_last_invoke(:) = 1 
                    kgen_mymid = 0 
                    kgen_msize = 1 
                    kgen_mpirank = 0 
                    ALLOCATE (kgen_filepath(0:kgen_msize-1, 0:kgen_osize-1)) 
                    ALLOCATE (kgen_lockpath(0:kgen_msize-1, 0:kgen_osize-1)) 
                    ALLOCATE (kgen_isstop(0:kgen_msize-1, 0:kgen_osize-1)) 
                    kgen_isstop(:,:) = .TRUE. 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(1), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(2), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(3), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(4), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(5), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(6), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(7), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(8), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(9), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(10), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(11), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(13), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(14), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(15), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(16), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(17), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(18), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(12), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(20), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(28), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(5), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(9), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(10), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(13), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(14), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(15), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(21), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(25), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(1), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(2), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(3), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(4), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(6), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(7), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(8), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(11), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(12), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(16), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(17), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(18), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                    CALL kgen_init_vars(INT(0), INT(0), INT(0), INT(0), INT(19), kgen_msize, kgen_osize, kgen_lockpath, &
                    &kgen_last_invoke, kgen_isstop) 
                END IF   
                kgen_issave(0) = .FALSE. 
                kgen_islast(0) = .FALSE. 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(0), INT(0), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(1), INT(1), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(2), INT(2), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(3), INT(3), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(4), INT(4), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(5), INT(5), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(6), INT(6), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(7), INT(7), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(8), INT(8), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(9), INT(9), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(10), INT(10), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(11), INT(11), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(13), INT(13), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(14), INT(14), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(15), INT(15), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(16), INT(16), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(17), INT(17), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(18), INT(18), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(12), INT(12), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(20), INT(20), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(28), INT(28), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(5), INT(5), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(9), INT(9), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(10), INT(10), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(13), INT(13), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(14), INT(14), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(15), INT(15), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(21), INT(21), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(25), INT(25), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(1), INT(1), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(2), INT(2), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(3), INT(3), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(4), INT(4), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(6), INT(6), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(7), INT(7), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(8), INT(8), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(11), INT(11), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(12), INT(12), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(16), INT(16), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(17), INT(17), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(18), INT(18), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                CALL kgen_check_save(INT(0), INT(0), INT(0), INT(0), INT(19), INT(19), kgen_mymid, 0, kgen_osize, kgen_invoke, &
                &kgen_last_invoke, kgen_issave, kgen_islast) 
                IF (kgen_issave(0)) THEN 
                    WRITE (kgen_filepath(kgen_mymid, 0), FMT="(A,I0,A,I0,A,I0)") &
                    &"/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/kernel/calc.", kgen_mymid, ".", 0, ".", &
                    &kgen_invoke(0) 
                    OPEN (NEWUNIT=kgen_unit, FILE=TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0))), STATUS="REPLACE", ACCESS="STREAM", &
                    &FORM="UNFORMATTED", ACTION="WRITE", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr) 
                    CALL kgen_error_stop(kgen_ierr, "File open error: " // TRIM(ADJUSTL(kgen_filepath(kgen_mymid, 0)))) 
                      
                    !argument input variables 
                      
                    !extern input variables 
                      
                    !local input variables 
                    WRITE (UNIT = kgen_unit) i 
                    WRITE (UNIT = kgen_unit) j 
                    IF (SIZE(output)==1) THEN 
                        IF (UBOUND(output, 1)<LBOUND(output, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(output, 1)==0 .AND. LBOUND(output, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(output)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(output, mask=(output .eq. output))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) output 
                    END IF   
                    IF (SIZE(out2)==1) THEN 
                        IF (UBOUND(out2, 1)<LBOUND(out2, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(out2, 1)==0 .AND. LBOUND(out2, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(out2)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(out2, mask=(out2 .eq. out2))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) out2 
                    END IF   
                    IF (SIZE(out3)==1) THEN 
                        IF (UBOUND(out3, 1)<LBOUND(out3, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(out3, 1)==0 .AND. LBOUND(out3, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(out3)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(out3, mask=(out3 .eq. out3))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) out3 
                    END IF   
                END IF   
                IF (kgen_issave(0)) THEN 
                    kgen_openmp_issave = kgen_invoke(0) 
                ELSE 
                    kgen_openmp_issave = -1 
                END IF   
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
                kgen_openmp_issave = -1 
                IF (kgen_issave(0)) THEN 
                      
                    !extern output variables 
                      
                    !local output variables 
                    IF (SIZE(output)==1) THEN 
                        IF (UBOUND(output, 1)<LBOUND(output, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(output, 1)==0 .AND. LBOUND(output, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(output)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(output, mask=(output .eq. output))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) output 
                    END IF   
                    IF (SIZE(out2)==1) THEN 
                        IF (UBOUND(out2, 1)<LBOUND(out2, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(out2, 1)==0 .AND. LBOUND(out2, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(out2)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(out2, mask=(out2 .eq. out2))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) out2 
                    END IF   
                    IF (SIZE(out3)==1) THEN 
                        IF (UBOUND(out3, 1)<LBOUND(out3, 1)) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE IF (UBOUND(out3, 1)==0 .AND. LBOUND(out3, 1)==0) THEN 
                            kgen_istrue = .FALSE. 
                        ELSE 
                            kgen_istrue = .TRUE. 
                        END IF   
                    ELSE IF (SIZE(out3)==0) THEN 
                        kgen_istrue = .FALSE. 
                    ELSE 
                        kgen_istrue = .TRUE. 
                    END IF   
                    WRITE (UNIT = kgen_unit) kgen_istrue 
                    IF (kgen_istrue) THEN 
                        kgen_array_sum = DBLE(SUM(out3, mask=(out3 .eq. out3))) 
                        WRITE (UNIT = kgen_unit) kgen_array_sum 
                        WRITE (UNIT = kgen_unit) out3 
                    END IF   
                    CLOSE (UNIT=kgen_unit) 
                    WRITE (*, *) "Collected Kernel Input/Ouput state from: ", kgen_mymid, 0, kgen_invoke(0) 
                END IF   
                IF (.NOT. kgen_ischecked(0) .AND. kgen_islast(0)) THEN 
                    kgen_ischecked(0) = .TRUE. 
                    OPEN (NEWUNIT=kgen_stopunit, FILE=TRIM(ADJUSTL(kgen_lockpath(kgen_mymid, 0))), STATUS="NEW", &
                    &IOSTAT=kgen_ierr) 
                    IF (kgen_ierr == 0) THEN 
                        CLOSE (UNIT=kgen_stopunit, STATUS="KEEP") 
                    END IF   
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    CALL kgen_check_stop(INT(0), INT(0), INT(0), INT(0), kgen_msize, kgen_osize, 0, kgen_lockpath, kgen_isstop) 
                    IF (ALL(kgen_isstop)) THEN 
                        OPEN (NEWUNIT=kgen_stopunit, &
                        &FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/kernel/kgen_statefile.lst", &
                        &STATUS="REPLACE", FORM="FORMATTED", ACCESS="SEQUENTIAL", ACTION="WRITE", IOSTAT=kgen_ierr) 
                        IF (kgen_ierr .EQ. 0) THEN 
                            FLUSH (kgen_stopunit) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(0), INT(0)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(1), INT(1)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(2), INT(2)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(3), INT(3)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(4), INT(4)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(5), INT(5)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(6), INT(6)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(7), INT(7)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(8), INT(8)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(9), INT(9)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(10), INT(10)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(11), INT(11)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(13), INT(13)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(14), INT(14)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(15), INT(15)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(16), INT(16)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(17), INT(17)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(18), INT(18)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(12), INT(12)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(20), INT(20)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(28), INT(28)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(5), INT(5)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(9), INT(9)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(10), INT(10)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(13), INT(13)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(14), INT(14)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(15), INT(15)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(21), INT(21)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(25), INT(25)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(1), INT(1)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(2), INT(2)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(3), INT(3)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(4), INT(4)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(6), INT(6)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(7), INT(7)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(8), INT(8)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(11), INT(11)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(12), INT(12)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(16), INT(16)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(17), INT(17)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(18), INT(18)) 
                            CALL kgen_write_list(kgen_stopunit, INT(0), INT(0), INT(0), INT(0), INT(19), INT(19)) 
                            FLUSH (kgen_stopunit) 
                            CLOSE (UNIT=kgen_stopunit, STATUS="KEEP") 
                            WRITE (*, *) "Stopping application..." 
                            CALL SLEEP(1) 
                            STOP 0 
                        END IF   
                    END IF   
                END IF   
                kgen_invoke(0) = kgen_invoke(0) + 1 
                !END OF KGEN REGION 
                !$kgen end_callsite
            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
          
        CONTAINS 
          
        FUNCTION kgen_get_newunit() RESULT ( new_unit ) 
            INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000 
            LOGICAL :: is_opened 
            INTEGER :: nunit, new_unit, counter 
              
            new_unit = -1 
            DO counter=UNIT_MIN, UNIT_MAX 
                INQUIRE (UNIT=counter, OPENED=is_opened) 
                IF (.NOT. is_opened) THEN 
                    new_unit = counter 
                    EXIT 
                END IF   
            END DO   
        END FUNCTION kgen_get_newunit 
          
        SUBROUTINE kgen_init_vars(mpi_s, mpi_e, omp_s, omp_e, invoke_e, msize, osize, lockpath, last_invoke, isstop) 
            INTEGER, INTENT(IN) :: mpi_s, mpi_e, omp_s, omp_e, invoke_e, msize, osize 
            INTEGER, INTENT(INOUT), DIMENSION(0:osize-1) :: last_invoke 
            LOGICAL, INTENT(INOUT), DIMENSION(0:msize-1,0:osize-1) :: isstop 
            CHARACTER(LEN=1024), INTENT(INOUT), DIMENSION(0:msize-1,0:osize-1) :: lockpath 
            INTEGER :: mpi_idx, openmp_idx, temp_unit, ierr 
            DO mpi_idx=mpi_s,mpi_e 
                DO openmp_idx=omp_s,omp_e 
                    WRITE (lockpath(mpi_idx, openmp_idx), FMT="(A,I0,A,I0)") &
                    &"/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/kernel/done.", mpi_idx, ".", openmp_idx 
                    isstop(mpi_idx, openmp_idx) = .FALSE. 
                    last_invoke(openmp_idx) = MAX( last_invoke(openmp_idx), invoke_e) 
                    OPEN (NEWUNIT=temp_unit, FILE=lockpath(mpi_idx, openmp_idx), STATUS="OLD", IOSTAT=ierr) 
                    IF (ierr .EQ. 0) THEN 
                        CLOSE (UNIT=temp_unit, STATUS="DELETE") 
                    END IF   
                END DO   
            END DO   
        END SUBROUTINE kgen_init_vars 
          
        SUBROUTINE kgen_check_save(mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e, mymid, myoid, osize, invoke, last_invoke, &
        &issave, islast) 
            INTEGER, INTENT(IN) :: mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e, osize, mymid, myoid 
            INTEGER, INTENT(IN), DIMENSION(0:osize-1) :: invoke, last_invoke 
            LOGICAL, INTENT(OUT), DIMENSION(0:osize-1) :: issave, islast 
            IF ((mymid .GE. mpi_s) .AND. (mymid .LE. mpi_e)) THEN 
                IF ((myoid .GE. omp_s) .AND. (myoid .LE. omp_e)) THEN 
                    IF ((invoke(myoid) .GE. invoke_s) .AND. (invoke(myoid) .LE. invoke_e)) THEN 
                        issave(myoid) = .TRUE. 
                    END IF   
                    IF (invoke(myoid) .GE. last_invoke(myoid)) THEN 
                        islast(myoid) = .TRUE. 
                    END IF   
                END IF   
            END IF   
        END SUBROUTINE kgen_check_save 
          
        SUBROUTINE kgen_check_stop(mpi_s, mpi_e, omp_s, omp_e, msize, osize, myoid, lockpath, isstop) 
            INTEGER, INTENT(IN) :: mpi_s, mpi_e, omp_s, omp_e, msize, osize, myoid 
            CHARACTER(LEN=1024), INTENT(IN), DIMENSION(0:msize-1,0:osize-1) :: lockpath 
            LOGICAL, INTENT(OUT), DIMENSION(0:msize-1,0:osize-1) :: isstop 
            INTEGER :: mpi_idx, openmp_idx, ierr, myunit 
            DO mpi_idx=mpi_s,mpi_e 
                DO openmp_idx=omp_s,omp_e 
                    IF (.NOT. isstop(mpi_idx, openmp_idx)) THEN 
                        OPEN (NEWUNIT=myunit, FILE=TRIM(ADJUSTL(lockpath(mpi_idx, openmp_idx))), STATUS="OLD", ACTION="READ", &
                        &IOSTAT=ierr) 
                        IF (ierr .EQ. 0) THEN 
                            isstop(mpi_idx, openmp_idx) = .TRUE. 
                            CLOSE (UNIT=myunit) 
                        END IF   
                    END IF   
                END DO   
            END DO   
        END SUBROUTINE kgen_check_stop 
          
        SUBROUTINE kgen_write_list(myunit, mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e) 
            INTEGER, INTENT(IN) :: myunit, mpi_s, mpi_e, omp_s, omp_e, invoke_s, invoke_e 
            INTEGER :: mpi_idx, openmp_idx, invoke_idx, temp_unit, ierr 
            CHARACTER(LEN=16) :: mpi_str, openmp_str, invoke_str 
            DO mpi_idx=mpi_s,mpi_e 
                WRITE (mpi_str, "(I16)") mpi_idx 
                DO openmp_idx=omp_s,omp_e 
                    WRITE (openmp_str, "(I16)") openmp_idx 
                    DO invoke_idx=invoke_s,invoke_e 
                        WRITE (invoke_str, "(I16)") invoke_idx 
                        WRITE (UNIT = myunit, FMT="(A)") "calc." // TRIM(ADJUSTL(mpi_str)) // "." // TRIM(ADJUSTL(openmp_str)) // &
                        &"." // TRIM(ADJUSTL(invoke_str)) 
                    END DO   
                    OPEN (NEWUNIT=temp_unit, FILE="/glade/u/home/youngsun/repos/github/KGen/examples/simple-region/kernel/done." &
                    &// TRIM(ADJUSTL(mpi_str)) // "." // TRIM(ADJUSTL(openmp_str)), STATUS="OLD", IOSTAT=ierr) 
                    IF (ierr .EQ. 0) THEN 
                        CLOSE (UNIT=temp_unit, STATUS="DELETE") 
                    END IF   
                END DO   
            END DO   
        END SUBROUTINE kgen_write_list 
          
        SUBROUTINE kgen_error_stop(ierr, errmsg) 
            INTEGER, INTENT(IN) :: ierr 
            CHARACTER(LEN=*), INTENT(IN) :: errmsg 
            IF (ierr /= 0) THEN 
                WRITE (*, *) errmsg 
                STOP 1 
            END IF   
        END SUBROUTINE kgen_error_stop 
          
        SUBROUTINE kgen_print_counter(counter) 
            INTEGER, INTENT(IN) :: counter 
            WRITE (*, *) "KGEN writes input state variables at count = ", counter 
        END SUBROUTINE kgen_print_counter 
          
    END SUBROUTINE
END MODULE
BLOCK DATA KGEN 
    INTEGER :: kgen_mpirank = 0 
    INTEGER :: kgen_openmp_issave = -1 
    COMMON / state / kgen_mpirank, kgen_openmp_issave 
END BLOCK DATA KGEN 