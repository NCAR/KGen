
! KGEN-generated Fortran source file
!
! Filename    : test2_mod.F90
! Generated at: 2015-07-31 10:22:33
! KGEN version: 0.4.12



    MODULE test2_mod
    USE test2_type_mod, ONLY : kgen_write_mod2 => kgen_write
        USE test2_compute
        USE test2_type_mod
        PUBLIC test_2
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


        SUBROUTINE test_2()
            !integer(kind=4), parameter :: real_kind = 4
            INTEGER(KIND=4), parameter :: niter = 10
            !integer(kind=4), parameter :: np = 4
            TYPE(complex_type) :: test_type1(niter)
            !initialization
            INTEGER :: kgen_ierr, kgen_unit
            INTEGER, DIMENSION(3,10) :: kgen_indexes
            INTEGER, SAVE :: kgen_counter = 1
            CHARACTER(LEN=16) :: kgen_counter_conv
            INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
            CHARACTER(LEN=1024) :: kgen_filepath
                do i=1,niter
                        test_type1(i)%element_1 = 1.0
                !do j=1,np
                !        do k=1,np
                                        test_type1(i)%elem1%a(:,:) = 1.0
                                        test_type1(i)%elem1%b(:,:) = 1.0
                                        test_type1(i)%elem2%level3_1%param1(:,:) = 1.0
                                        test_type1(i)%elem2%level3_1%param2(:,:) = 1.0
                !        end do
                !end do
                        test_type1(i)%elem2%d = 1.0
                        test_type1(i)%elem2%e = 1.0
                        test_type1(i)%elem2%f = 1.0
                end do
            !perform computation
            !do i=1,niter

            !$OMP MASTER
            kgen_unit = -1
            IF ( ANY(kgen_counter == kgen_counter_at) ) THEN
                WRITE( kgen_counter_conv, * ) kgen_counter
                kgen_filepath = "/glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_2/kgen/kernel/compute." // TRIM(ADJUSTL(kgen_counter_conv))
                    kgen_unit = kgen_get_newunit()
                    OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="REPLACE", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
                    IF ( kgen_ierr /= 0 ) THEN
                        CALL kgen_error_stop( "FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)) )
                    END IF
                    CALL kgen_print_counter(kgen_counter)

                ! driver variables
                ! Not kernel driver input

                ! input variables for the parent of callsite
                CALL kgen_write_complex_type_dim1(test_type1, kgen_unit)

            END IF
            !$OMP END MASTER
            ! call to kernel
                        call compute(test_type1)
            !$OMP MASTER
            IF ( ANY(kgen_counter == kgen_counter_at) ) THEN

                ! output variables for the parent of callsite
                CALL kgen_write_complex_type_dim1(test_type1, kgen_unit)

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

            !end do
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
            SUBROUTINE kgen_write_complex_type_dim1(var, kgen_unit, indexes, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: indexes
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                TYPE(complex_type), INTENT(IN), DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1

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
                        DO idx1=indexes(1,1), indexes(2,1)
                            IF ( PRESENT(printvar) ) THEN
                                CALL kgen_write_mod2(var(idx1), kgen_unit, printvar=printvar)
                            ELSE
                                CALL kgen_write_mod2(var(idx1), kgen_unit)
                            END IF
                        END DO
                    ELSE
                        WRITE(UNIT = kgen_unit) LBOUND(var, 1)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 1)
                        DO idx1=LBOUND(var,1), UBOUND(var, 1)
                            IF ( PRESENT(printvar) ) THEN
                                CALL kgen_write_mod2(var(idx1), kgen_unit, printvar=printvar)
                            ELSE
                                CALL kgen_write_mod2(var(idx1), kgen_unit)
                            END IF
                        END DO
                    END IF
                END IF
            END SUBROUTINE kgen_write_complex_type_dim1

        END SUBROUTINE test_2
    END MODULE test2_mod
