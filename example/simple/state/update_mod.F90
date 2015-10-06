
! KGEN-generated Fortran source file
!
! Filename    : update_mod.F90
! Generated at: 2015-10-02 17:13:17
! KGEN version: 0.5.2



    MODULE update_mod
        USE calc_mod, ONLY: calc
        PUBLIC update
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        FUNCTION kgen_get_newunit() RESULT(new_unit)
           INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
           LOGICAL :: is_opened
           INTEGER :: nunit, new_unit, counter
        
           new_unit = -1
           DO counter=UNIT_MIN, UNIT_MAX
               inquire(UNIT=counter, OPENED=is_opened)
               IF (.NOT. is_opened) THEN
                   new_unit = counter
                   EXIT
               END IF
           END DO
        END FUNCTION
        
        SUBROUTINE kgen_error_stop( msg )
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: msg
        
            WRITE (*,*) msg
            STOP 1
        END SUBROUTINE 


        SUBROUTINE update()
            INTEGER :: i, j
            INTEGER, dimension(4, 4) :: output
            INTEGER :: kgen_ierr, kgen_unit
            INTEGER, DIMENSION(3,10) :: kgen_indexes
            INTEGER, SAVE :: kgen_counter = 1
            CHARACTER(LEN=16) :: kgen_counter_conv
            INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
            CHARACTER(LEN=1024) :: kgen_filepath
        DO i=1, 4
            DO j=1, 4

                    !$OMP MASTER
                    kgen_unit = -1
                    IF ( ANY(kgen_counter == kgen_counter_at) ) THEN
                        WRITE( kgen_counter_conv, * ) kgen_counter
                        kgen_filepath = "/glade/p/work/amogh/kgen_dev/git_kgen/KGen/example/simple/kernel/calc." // TRIM(ADJUSTL(kgen_counter_conv))
                            kgen_unit = kgen_get_newunit()
                            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="REPLACE", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
                            IF ( kgen_ierr /= 0 ) THEN
                                CALL kgen_error_stop( "FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)) )
                            END IF
                            CALL kgen_print_counter(kgen_counter)

                        ! driver variables
                        ! Not kernel driver input

                        ! input variables for the parent of callsite
                        WRITE(UNIT=kgen_unit) i
                        WRITE(UNIT=kgen_unit) j
                        WRITE(UNIT=kgen_unit) output

                    END IF
                    !$OMP END MASTER
                    ! call to kernel
                CALL calc(i, j, output)
                    !$OMP MASTER
                    IF ( ANY(kgen_counter == kgen_counter_at) ) THEN

                        ! output variables for the parent of callsite
                        WRITE(UNIT=kgen_unit) output

                        ENDFILE kgen_unit
                        CALL sleep(1)
                        CLOSE (UNIT=kgen_unit)
                    END IF
                    PRINT *, "kgen_counter = ", kgen_counter
                    IF ( kgen_counter > maxval(kgen_counter_at) ) THEN
                            CALL sleep(2)
                        PRINT *, "kgen_counter is larger than maximum counter. Exit program..."
                        STOP
                    END IF
                    kgen_counter = kgen_counter + 1
                    !$OMP END MASTER

            END DO
        END DO
        print *, 'SUM(output) = ', SUM(output)
        CONTAINS
            SUBROUTINE kgen_print_counter(counter)
                INTEGER, INTENT(IN) :: counter
                PRINT *, "KGEN writes input state variables at count = ", counter
            END SUBROUTINE
            
            SUBROUTINE kgen_print_mpirank_counter(rank, counter)
                INTEGER, INTENT(IN) :: rank, counter
                PRINT *, "KGEN writes input state variables at count = ", counter, " on mpirank = ", rank
            END SUBROUTINE

        ! write subroutines
            SUBROUTINE kgen_write_integer_4_dim2(var, kgen_unit, indexes, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: indexes
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                integer(KIND=4), INTENT(IN), DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2

                IF ( SIZE(var)==1 ) THEN
                    IF ( UBOUND(var, 1)<LBOUND(var, 1) ) THEN
                        is_true = .FALSE.
                    ELSE IF ( UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0 ) THEN
                        is_true = .FALSE.
                    ELSE
                        is_true = .TRUE.
                    END IF
                ELSE
                    is_true = .TRUE.
                END IF
                WRITE(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    IF ( PRESENT(indexes) ) THEN
                        WRITE(UNIT = kgen_unit) indexes(1,1)
                        WRITE(UNIT = kgen_unit) indexes(2,1)
                        WRITE(UNIT = kgen_unit) indexes(1,2)
                        WRITE(UNIT = kgen_unit) indexes(2,2)
                        WRITE(UNIT = kgen_unit) var(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2))
                        IF ( PRESENT(printvar) ) THEN
                            PRINT *, "** KGEN DEBUG: " // printvar // &
                            &"(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2)) **", &
                            &var(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2))
                        END IF
                    ELSE
                        WRITE(UNIT = kgen_unit) LBOUND(var, 1)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 1)
                        WRITE(UNIT = kgen_unit) LBOUND(var, 2)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 2)
                        WRITE(UNIT = kgen_unit) var
                        IF ( PRESENT(printvar) ) THEN
                            PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                        END IF
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_write_integer_4_dim2

        END SUBROUTINE 
    END MODULE 
