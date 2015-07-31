
! KGEN-generated Fortran source file
!
! Filename    : test5_mod.F90
! Generated at: 2015-07-31 09:02:43
! KGEN version: 0.4.12



    MODULE test5_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE compute, ONLY: add
        PUBLIC test_5
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE test_5(kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            !real(kind=real_kind), allocatable :: array_1(:,:,:,:), &
            !                                       array_2(:,:,:,:), &
            !                                      array_3(:,:,:,:)
            !integer(kind=4), parameter :: array_size = 10
            !allocate(array_1(array_size,array_size,array_size,array_size))
            !allocate(array_2(array_size,array_size,array_size,array_size))
            !allocate(array_3(array_size,array_size,array_size,array_size))
            !array_1(:,:,:,:) = 1.0
            !array_2(:,:,:,:) = 1.0
            tolerance = 1.E-14
            CALL kgen_init_check(check_status, tolerance)
            ! Not kernel driver input

            ! No parent output var


            ! call to kernel
                call add()
            ! kernel verification for output variables
            CALL kgen_print_check("add", check_status)
            CALL system_clock(start_clock, rate_clock)
            DO kgen_intvar=1,10
                CALL add
            END DO
            CALL system_clock(stop_clock, rate_clock)
            WRITE(*,*)
            PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
            !                print *, "a3 = ", array_3
            !if(allocated(array_1)) deallocate(array_1)
            !if(allocated(array_2)) deallocate(array_2)
            !if(allocated(array_3)) deallocate(array_3)

        ! write subroutines
        ! No subroutines

        ! verify subroutines
        ! No verification
        END SUBROUTINE 
    END MODULE 
