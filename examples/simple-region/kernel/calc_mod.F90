!KGEN-generated Fortran source file 
  
!Generated at : 2017-08-17 15:05:50 
!KGEN version : 0.7.3 
  
MODULE calc_mod
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    PUBLIC calc 
CONTAINS
    SUBROUTINE calc(i, j, output, out2, out3)
        INTEGER, INTENT(IN) :: i, j
        real, INTENT(OUT), dimension(:,:) :: out3, output, out2
          ! Also a comment
        IF ( i > j ) THEN
            output(i,j) = i - j
            out2(i, j) = 2*(i-j)
            out3(i, j) = 3*(i-j)
        ELSE
            output(i,j) = j - i
            out2(i, j) = 2*(j-i)
            out3(i, j) = 3*(j-i)
        END IF
    END SUBROUTINE
END MODULE