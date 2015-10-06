
! KGEN-generated Fortran source file
!
! Filename    : calc_mod.F90
! Generated at: 2015-10-02 17:13:17
! KGEN version: 0.5.2



    MODULE calc_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        PUBLIC calc
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE calc(i, j, output)
            INTEGER, intent(in) :: i
            INTEGER, intent(in) :: j
            INTEGER, intent(out), dimension(:,:) :: output
        output(i,j) = i + j
        END SUBROUTINE 
    END MODULE 
