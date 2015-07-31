
! KGEN-generated Fortran source file
!
! Filename    : test1_mod.F90
! Generated at: 2015-07-31 10:22:26
! KGEN version: 0.4.12



    MODULE test1_mod
        USE compute, ONLY: compute_add
        PUBLIC test_1
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


        SUBROUTINE test_1()
            INTEGER(KIND=4), parameter :: real_kind = 8
            INTEGER(KIND=4), parameter :: np = 4
            TYPE derived_type1
                !real(kind=real_kind), dimension(np,np) :: a
                REAL(KIND=real_kind) :: a(np,np)
                REAL(KIND=real_kind) :: b(np,np)
                REAL(KIND=real_kind) :: c(np,np)
            END TYPE derived_type1
            INTEGER(KIND=4) :: niter = 10
            INTEGER(KIND=4) :: n
            TYPE(derived_type1) :: d1
            INTEGER :: kgen_ierr, kgen_unit
            INTEGER, DIMENSION(3,10) :: kgen_indexes
            INTEGER, SAVE :: kgen_counter = 1
            CHARACTER(LEN=16) :: kgen_counter_conv
            INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
            CHARACTER(LEN=1024) :: kgen_filepath
        do i=1,np
                do j=1,np
                        d1%a(i,j) = 1.0
                        d1%b(i,j) = 1.0
                    !print *, "a(", i, ")(", j, ") = ", d1.a(i,j)
                end do
        end do
        do n=1,niter

                !$OMP MASTER
                kgen_unit = -1
                IF ( ANY(kgen_counter == kgen_counter_at) ) THEN
                    WRITE( kgen_counter_conv, * ) kgen_counter
                    kgen_filepath = "/glade/p/work/amogh/kgen_dev/KGen/Test/language_tests/test_1/kgen/kernel/compute_add." // TRIM(ADJUSTL(kgen_counter_conv))
                        kgen_unit = kgen_get_newunit()
                        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="REPLACE", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="WRITE", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
                        IF ( kgen_ierr /= 0 ) THEN
                            CALL kgen_error_stop( "FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)) )
                        END IF
                        CALL kgen_print_counter(kgen_counter)

                    ! driver variables
                    ! Not kernel driver input

                    ! input variables for the parent of callsite
                    CALL kgen_write_derived_type1(d1, kgen_unit)

                END IF
                !$OMP END MASTER
                ! call to kernel
                call compute_add(d1%a, d1%b, d1%c)
                !$OMP MASTER
                IF ( ANY(kgen_counter == kgen_counter_at) ) THEN

                    ! output variables for the parent of callsite
                    ! No parent output var

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

                !         do i=1,np
                !                 do j=1,np
                !                         print *, "c(", i, ")(", j, ") = ", d1.c(i,j)
                !                 end do
                !         end do
        end do
        CONTAINS
            SUBROUTINE kgen_print_counter(counter)
                INTEGER, INTENT(IN) :: counter
                PRINT *, "KGEN writes input state variables at count = ", counter
            END SUBROUTINE
            
            SUBROUTINE kgen_print_mpirank_counter(rank, counter)
                INTEGER, INTENT(IN) :: rank, counter
                PRINT *, "KGEN writes input state variables at count = ", counter, " on mpirank = ", rank
            END SUBROUTINE
        SUBROUTINE kgen_write_derived_type1(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(derived_type1), INTENT(in) :: var
            WRITE(UNIT=kgen_unit) var%a
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%a **", var%a
            END IF
            WRITE(UNIT=kgen_unit) var%b
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%b **", var%b
            END IF
            WRITE(UNIT=kgen_unit) var%c
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%c **", var%c
            END IF
        END SUBROUTINE

        ! write subroutines
        END SUBROUTINE test_1
    END MODULE test1_mod
