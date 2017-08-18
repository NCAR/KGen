!KGEN-generated Fortran source file 
  
!Generated at : 2017-08-17 15:05:50 
!KGEN version : 0.7.3 
  
MODULE update_mod
    USE calc_mod, ONLY: calc 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    PUBLIC update 
CONTAINS
SUBROUTINE update(kgen_unit, kgen_measure, kgen_isverified) 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    INTEGER :: i, j 
    REAL, dimension(4, 4) :: out2, out3, output 
    INTEGER, INTENT(IN) :: kgen_unit 
#ifdef KGEN_PAPI 
#include <f90papi.h> 
    INTEGER(KIND=8), INTENT(OUT) :: kgen_measure 
#else 
    REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
#endif 
    LOGICAL, INTENT(OUT) :: kgen_isverified 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
    LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      
    TYPE(check_t) :: check_status 
#ifdef KGEN_PAPI 
    INTEGER, DIMENSION(1) :: kgen_papi_events = (/ PAPI_TOT_INS /) 
    INTEGER(KIND=8), DIMENSION(2) :: kgen_papi_values 
    INTEGER :: kgen_retval 
#else 
    INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
#endif 
    REAL, dimension(4,4) :: kgenref_output 
    REAL, dimension(4,4) :: kgenref_out2 
    REAL, dimension(4,4) :: kgenref_out3 
      
    !local input variables 
    READ (UNIT = kgen_unit) i 
    READ (UNIT = kgen_unit) j 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) output 
        CALL kgen_array_sumcheck("output", kgen_array_sum, DBLE(SUM(output, mask=(output .eq. output))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) out2 
        CALL kgen_array_sumcheck("out2", kgen_array_sum, DBLE(SUM(out2, mask=(out2 .eq. out2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) out3 
        CALL kgen_array_sumcheck("out3", kgen_array_sum, DBLE(SUM(out3, mask=(out3 .eq. out3))), .TRUE.) 
    END IF   
      
    !extern output variables 
      
    !local output variables 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_output 
        CALL kgen_array_sumcheck("kgenref_output", kgen_array_sum, DBLE(SUM(kgenref_output, mask=(kgenref_output .eq. &
        &kgenref_output))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_out2 
        CALL kgen_array_sumcheck("kgenref_out2", kgen_array_sum, DBLE(SUM(kgenref_out2, mask=(kgenref_out2 .eq. kgenref_out2))), &
        &.TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_out3 
        CALL kgen_array_sumcheck("kgenref_out3", kgen_array_sum, DBLE(SUM(kgenref_out3, mask=(kgenref_out3 .eq. kgenref_out3))), &
        &.TRUE.) 
    END IF   
    IF (kgen_evalstage) THEN 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_mainstage) THEN 
    END IF   
      
    !Uncomment following call statement to turn on perturbation experiment. 
    !Adjust perturbation value and/or kind parameter if required. 
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
      
      
    !call to kgen kernel 
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
                IF (kgen_mainstage) THEN 
                      
                    !verify init 
                    CALL kgen_init_check(check_status, tolerance=1.0d-14, verboseLevel=1) 
                      
                    !extern verify variables 
                      
                    !local verify variables 
                    CALL kv_update_real___dim2("output", check_status, output, kgenref_output) 
                    CALL kv_update_real___dim2("out2", check_status, out2, kgenref_out2) 
                    CALL kv_update_real___dim2("out3", check_status, out3, kgenref_out3) 
                    WRITE (*, *) "" 
                    IF (check_status%verboseLevel > 0) THEN 
                        WRITE (*, *) "Number of output variables: ", check_status%numTotal 
                        WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
                        WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
                        WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
                        WRITE (*, *) "Tolerance: ", kgen_tolerance 
                    END IF   
                    WRITE (*, *) "" 
                    IF (check_status%numOutTol > 0) THEN 
                        WRITE (*, *) "Verification FAILED" 
                        check_status%Passed = .FALSE. 
                        kgen_isverified = .FALSE. 
                    ELSE 
                        WRITE (*, *) "Verification PASSED" 
                        check_status%Passed = .TRUE. 
                        kgen_isverified = .TRUE. 
                    END IF   
                    WRITE (*, *) "" 
#ifdef KGEN_PAPI 
                    CALL PAPIF_start_counters(kgen_papi_events, 1, kgen_retval) 
#else 
                    CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
#endif 
                CALL calc(i, j, output, out2, out3)                 !Calling kernel
#ifdef KGEN_PAPI 
                CALL PAPIF_read_counters(kgen_papi_values(1), 1, kgen_retval) 
                CALL PAPIF_stop_counters(kgen_papi_values(2), 1, kgen_retval) 
                kgen_measure = kgen_papi_values(1) 
                WRITE (*, *) "calc : PAPI_TOT_INS per call: ", kgen_measure 
#else 
                CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
                kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock) 
                WRITE (*, *) "calc : Time per call (usec): ", kgen_measure 
#endif 
                END IF   
                IF (kgen_warmupstage) THEN 
                END IF   
                IF (kgen_evalstage) THEN 
                END IF   
                  
                CONTAINS 
                  
                !verify state subroutine for kv_update_real___dim2 
                RECURSIVE SUBROUTINE kv_update_real___dim2(varname, check_status, var, kgenref_var) 
                    CHARACTER(LEN=*), INTENT(IN) :: varname 
                    TYPE(check_t), INTENT(INOUT) :: check_status 
                    REAL, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
                    INTEGER :: check_result 
                    LOGICAL :: is_print = .FALSE. 
                      
                    INTEGER :: idx1, idx2 
                    INTEGER :: n 
                    real :: nrmsdiff, rmsdiff 
                    real, ALLOCATABLE :: buf1(:,:), buf2(:,:) 
                      
                    check_status%numTotal = check_status%numTotal + 1 
                      
                    IF (ALL(var == kgenref_var)) THEN 
                        check_status%numIdentical = check_status%numIdentical + 1 
                        IF (check_status%verboseLevel > 1) THEN 
                            WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                        END IF   
                        check_result = CHECK_IDENTICAL 
                    ELSE 
                        ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                        ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                        n = COUNT(var /= kgenref_var) 
                        WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                            buf1 = ((var-kgenref_var)/kgenref_var)**2 
                            buf2 = (var-kgenref_var)**2 
                        ELSEWHERE 
                            buf1 = (var-kgenref_var)**2 
                            buf2 = buf1 
                        END WHERE   
                        nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                        rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                        IF (nrmsdiff > kgen_tolerance) THEN 
                            check_status%numOutTol = check_status%numOutTol + 1 
                            IF (check_status%verboseLevel > 0) THEN 
                                WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                            END IF   
                            check_result = CHECK_OUT_TOL 
                        ELSE 
                            check_status%numInTol = check_status%numInTol + 1 
                            IF (check_status%verboseLevel > 0) THEN 
                                WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                            END IF   
                            check_result = CHECK_IN_TOL 
                        END IF   
                    END IF   
                    IF (check_result == CHECK_IDENTICAL) THEN 
                        IF (check_status%verboseLevel > 2) THEN 
                            WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                            WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                            WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                            WRITE (*, *) "RMS of difference is ", 0 
                            WRITE (*, *) "Normalized RMS of difference is ", 0 
                            WRITE (*, *) "" 
                        END IF   
                    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                        IF (check_status%verboseLevel > 0) THEN 
                            WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                            WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                            WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                            WRITE (*, *) "RMS of difference is ", rmsdiff 
                            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                            WRITE (*, *) "" 
                        END IF   
                    ELSE IF (check_result == CHECK_IN_TOL) THEN 
                        IF (check_status%verboseLevel > 1) THEN 
                            WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                            WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                            WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                            WRITE (*, *) "RMS of difference is ", rmsdiff 
                            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                            WRITE (*, *) "" 
                        END IF   
                    END IF   
                      
                END SUBROUTINE kv_update_real___dim2 
                  
END SUBROUTINE   
END MODULE