
! KGEN-generated Fortran source file
!
! Filename    : test3_mod.F90
! Generated at: 2015-08-04 10:16:00
! KGEN version: 0.4.13



    MODULE test3_mod
        USE compute, ONLY: add
        PUBLIC test_3
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


        SUBROUTINE test_3()
            INTEGER(KIND=4), parameter :: real_kind = 4
            REAL(KIND=real_kind), allocatable :: array_1(:,:,:,:), array_2(:,:,:,:), array_3(:,:,:,:)
            INTEGER(KIND=4), parameter :: array_size = 10
            INTEGER :: kgen_ierr, kgen_unit
            INTEGER, DIMENSION(3,10) :: kgen_indexes
            INTEGER, SAVE :: kgen_counter = 1
            CHARACTER(LEN=16) :: kgen_counter_conv
            INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
            CHARACTER(LEN=1024) :: kgen_filepath
                allocate(array_1(array_size,array_size,array_size,array_size))
                allocate(array_2(array_size,array_size,array_size,array_size))
                allocate(array_3(array_size,array_size,array_size,array_size))
                array_1(:,:,:,:) = 1.0
                array_2(:,:,:,:) = 1.0

            !$OMP MASTER
            kgen_unit = -1
            IF ( ANY(kgen_counter == kgen_counter_at) ) THEN
                WRITE( kgen_counter_conv, * ) kgen_counter
                kgen_filepath = "/glade/p/work/amogh/kgen_dev/git_kgen/KGen/Test/unit_tests/test_3/kgen/kernel/add." // TRIM(ADJUSTL(kgen_counter_conv))
                    kgen_unit = kgen_get_newunit()
                    OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="REPLACE", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
                    IF ( kgen_ierr /= 0 ) THEN
                        CALL kgen_error_stop( "FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)) )
                    END IF
                    CALL kgen_print_counter(kgen_counter)

                ! driver variables
                ! Not kernel driver input

                ! input variables for the parent of callsite
                CALL kgen_write_real_real_kind_dim4_alloc(array_1, kgen_unit)
                CALL kgen_write_real_real_kind_dim4_alloc(array_3, kgen_unit)
                CALL kgen_write_real_real_kind_dim4_alloc(array_2, kgen_unit)

            END IF
            !$OMP END MASTER
            ! call to kernel
                call add(array_1, array_2, array_3)
            !$OMP MASTER
            IF ( ANY(kgen_counter == kgen_counter_at) ) THEN

                ! output variables for the parent of callsite
                CALL kgen_write_real_real_kind_dim4_alloc(array_3, kgen_unit)

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

            !                print *, "a3 = ", array_3
                if(allocated(array_1)) deallocate(array_1)
                if(allocated(array_2)) deallocate(array_2)
                if(allocated(array_3)) deallocate(array_3)
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
            SUBROUTINE kgen_write_real_real_kind_dim4_alloc(var, kgen_unit, indexes, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: indexes
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=real_kind), INTENT(IN), ALLOCATABLE, DIMENSION(:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4

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
                        WRITE(UNIT = kgen_unit) indexes(1,3)
                        WRITE(UNIT = kgen_unit) indexes(2,3)
                        WRITE(UNIT = kgen_unit) indexes(1,4)
                        WRITE(UNIT = kgen_unit) indexes(2,4)
                        WRITE(UNIT = kgen_unit) var(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2),indexes(1,3):indexes(2,3),indexes(1,4):indexes(2,4))
                        IF ( PRESENT(printvar) ) THEN
                            PRINT *, "** " // printvar // &
                            &"(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2),indexes(1,3):indexes(2,3),indexes(1,4):indexes(2,4)) **", &
                            &var(indexes(1,1):indexes(2,1),indexes(1,2):indexes(2,2),indexes(1,3):indexes(2,3),indexes(1,4):indexes(2,4))
                        END IF
                    ELSE
                        WRITE(UNIT = kgen_unit) LBOUND(var, 1)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 1)
                        WRITE(UNIT = kgen_unit) LBOUND(var, 2)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 2)
                        WRITE(UNIT = kgen_unit) LBOUND(var, 3)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 3)
                        WRITE(UNIT = kgen_unit) LBOUND(var, 4)
                        WRITE(UNIT = kgen_unit) UBOUND(var, 4)
                        WRITE(UNIT = kgen_unit) var
                        IF ( PRESENT(printvar) ) THEN
                            PRINT *, "** " // printvar // " **", var
                        END IF
                    END IF
                END IF
            END SUBROUTINE kgen_write_real_real_kind_dim4_alloc

        END SUBROUTINE 
    END MODULE 
