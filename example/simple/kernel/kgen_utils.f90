module kgen_utils_mod

INTEGER, PARAMETER :: kgen_dp = selected_real_kind(15, 307)

type check_t
    logical :: Passed
    integer :: numFatal
    integer :: numTotal
    integer :: numIdentical
    integer :: numWarning
    integer :: VerboseLevel
    real(kind=kgen_dp) :: tolerance
    real(kind=kgen_dp) :: minvalue
end type check_t

public kgen_dp, check_t, kgen_init_check, kgen_print_check

contains

subroutine kgen_init_check(check, tolerance, minvalue)
  type(check_t), intent(inout) :: check
  real(kind=kgen_dp), intent(in), optional :: tolerance
  real(kind=kgen_dp), intent(in), optional :: minvalue

  check%Passed   = .TRUE.
  check%numFatal = 0
  check%numWarning = 0
  check%numTotal = 0
  check%numIdentical = 0
  check%VerboseLevel = 1
  if(present(tolerance)) then
     check%tolerance = tolerance
  else
      check%tolerance = 1.0D-15
  endif
  if(present(minvalue)) then
     check%minvalue = minvalue
  else
      check%minvalue = 1.0D-15
  endif
end subroutine kgen_init_check

subroutine kgen_print_check(kname, check)
   character(len=*) :: kname
   type(check_t), intent(in) ::  check

   write (*,*)
   write (*,*) TRIM(kname),': Tolerance for normalized RMS: ',check%tolerance
   write (*,*) TRIM(kname),': Number of variables checked: ',check%numTotal
   write (*,*) TRIM(kname),': Number of Identical results: ',check%numIdentical
   write (*,*) TRIM(kname),': Number of warnings detected: ',check%numWarning
   write (*,*) TRIM(kname),': Number of fatal errors detected: ', check%numFatal

   if (check%numFatal> 0) then
        write(*,*) TRIM(kname),': Verification FAILED'
   else
        write(*,*) TRIM(kname),': Verification PASSED'
   endif
end subroutine kgen_print_check

end module
