
! KGEN-generated Fortran source file
!
! Filename    : calc_mod.F90
! Generated at: 2015-09-24 15:27:23
! KGEN version: 0.5.0



    MODULE calc_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        USE kind_mod, only : ikind
        PUBLIC calc
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables

        SUBROUTINE calc(i, j, output)
            INTEGER(KIND=ikind), intent(in) :: i
            INTEGER(KIND=ikind), intent(in) :: j
            INTEGER(KIND=ikind), intent(out), dimension(:,:) :: output
        output(i,j) = i + j
        END SUBROUTINE 
    END MODULE 
