# kgen_extra.py

kgen_file_header = \
"""
! KGEN-generated Fortran source file
!
! Filename    : %s
! Generated at: %s
! KGEN version: %s

"""

kgen_subprograms = \
"""FUNCTION kgen_get_newunit() RESULT(new_unit)
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
END SUBROUTINE """

kgen_print_counter = \
"""SUBROUTINE kgen_print_counter(counter)
    INTEGER, INTENT(IN) :: counter
    PRINT *, "KGEN writes input state variables at count = ", counter
END SUBROUTINE

SUBROUTINE kgen_print_mpirank_counter(rank, counter)
    INTEGER, INTENT(IN) :: rank, counter
    PRINT *, "KGEN writes input state variables at count = ", counter, " on mpirank = ", rank
END SUBROUTINE"""


kgen_verify_intrinsic_checkpart = \
"""check_status%%numTotal = check_status%%numTotal + 1
IF ( var %s ref_var ) THEN
    check_status%%numIdentical = check_status%%numIdentical + 1
    if(kgen_verboseLevel == 3) then
        WRITE(*,*)
        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
    endif
ELSE
    if(kgen_verboseLevel > 0) then
        WRITE(*,*)
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        if(kgen_verboseLevel == 3) then
            WRITE(*,*) "KERNEL: ", var
            WRITE(*,*) "REF.  : ", ref_var
        end if
    end if
    check_status%%numOutTol = check_status%%numOutTol + 1
END IF"""

kgen_verify_numeric_array = \
"""check_status%%numTotal = check_status%%numTotal + 1
IF ( ALL( var %(eqtest)s ref_var ) ) THEN

    check_status%%numIdentical = check_status%%numIdentical + 1            
    if(kgen_verboseLevel == 3) then
        WRITE(*,*)
        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
        !WRITE(*,*) "KERNEL: ", var
        !WRITE(*,*) "REF.  : ", ref_var
        IF ( ALL( var == 0 ) ) THEN
            if(kgen_verboseLevel == 3) then
                WRITE(*,*) "All values are zero."
            end if
        END IF
    end if
ELSE
    allocate(temp(%(allocshape)s))
    allocate(temp2(%(allocshape)s))

    n = count(var/=ref_var)
    where(abs(ref_var) > kgen_minvalue)
        temp  = ((var-ref_var)/ref_var)**2
        temp2 = (var-ref_var)**2
    elsewhere
        temp  = (var-ref_var)**2
        temp2 = temp
    endwhere
    nrmsdiff = sqrt(sum(temp)/real(n))
    rmsdiff = sqrt(sum(temp2)/real(n))

    if (nrmsdiff > kgen_tolerance) then
        check_status%%numOutTol = check_status%%numOutTol+1
    else
        check_status%%numInTol = check_status%%numInTol+1
    endif

    deallocate(temp,temp2)
END IF"""

kgen_verify_nonreal_array = \
"""check_status%%numTotal = check_status%%numTotal + 1
IF ( ALL( var %(eqtest)s ref_var ) ) THEN

    check_status%%numIdentical = check_status%%numIdentical + 1            
    if(kgen_verboseLevel == 3) then
        WRITE(*,*)
        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
        !WRITE(*,*) "KERNEL: ", var
        !WRITE(*,*) "REF.  : ", ref_var
        IF ( ALL( var == 0 ) ) THEN
                WRITE(*,*) "All values are zero."
        END IF
    end if
ELSE
    if(kgen_verboseLevel > 0) then
        WRITE(*,*)
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
    end if

    check_status%%numOutTol = check_status%%numOutTol+1
END IF"""

kgen_utils_file_head = \
"""
INTEGER, PARAMETER :: kgen_dp = selected_real_kind(15, 307)
INTEGER, PARAMETER :: CHECK_IDENTICAL = 1
INTEGER, PARAMETER :: CHECK_IN_TOL = 2
INTEGER, PARAMETER :: CHECK_OUT_TOL = 3

REAL(kind=kgen_dp) :: kgen_tolerance = 1.0D-15, kgen_minvalue = 1.0D-15
INTEGER :: kgen_verboselevel = 1

interface kgen_tostr
    module procedure kgen_tostr_args1
    module procedure kgen_tostr_args2
    module procedure kgen_tostr_args3
    module procedure kgen_tostr_args4
    module procedure kgen_tostr_args5
    module procedure kgen_tostr_args6
end interface

! PERTURB: add following interface
interface kgen_perturb_real
    module procedure kgen_perturb_real4_dim1
    module procedure kgen_perturb_real4_dim2
    module procedure kgen_perturb_real4_dim3
    module procedure kgen_perturb_real8_dim1
    module procedure kgen_perturb_real8_dim2
    module procedure kgen_perturb_real8_dim3
end interface

type check_t
    logical :: Passed
    integer :: numOutTol
    integer :: numTotal
    integer :: numIdentical
    integer :: numInTol
    integer :: rank
end type check_t

public kgen_dp, check_t, kgen_init_verify, kgen_init_check, kgen_tolerance
public kgen_minvalue, kgen_verboselevel, kgen_print_check, kgen_perturb_real
public CHECK_NOT_CHECKED, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
public kgen_get_newunit, kgen_error_stop
"""

kgen_utils_array_sumcheck = \
"""
subroutine kgen_array_sumcheck(varname, sum1, sum2, finish)
    character(*), intent(in) :: varname
    real(kind=8), intent(in) :: sum1, sum2
    real(kind=8), parameter  :: max_rel_diff = 1.E-10
    real(kind=8)  :: diff, rel_diff
    logical, intent(in), optional :: finish
    logical checkresult

    if ( sum1 == sum2 ) then
        checkresult = .TRUE.
    else
        checkresult = .FALSE.

        diff = ABS(sum2 - sum1)

        if ( .NOT. (sum1 == 0._8) ) then

            rel_diff = ABS(diff / sum1)
            if ( rel_diff > max_rel_diff ) then

                print *, ''
                print *, 'SUM of array, "', varname, '", is different.'
                print *, 'From file : ', sum1
                print *, 'From array: ', sum2
                print *, 'Difference: ', diff
                print *, 'Normalized difference: ', rel_diff

                if ( present(finish) .AND. finish ) then
                    stop
                end if
            end if
        else
            print *, ''
            print *, 'SUM of array, "', varname, '", is different.'
            print *, 'From file : ', sum1
            print *, 'From array: ', sum2
            print *, 'Difference: ', diff

            if ( present(finish) .AND. finish ) then
                stop
            end if
        end if
    end if
end subroutine
"""

kgen_utils_file_tostr = \
"""
function kgen_tostr_args1(idx1) result(tostr)
    integer, intent(in) :: idx1
    character(len=64) :: str_idx1
    character(len=64) :: tostr

    write(str_idx1, *) idx1
    tostr = trim(adjustl(str_idx1))
end function

function kgen_tostr_args2(idx1, idx2) result(tostr)
    integer, intent(in) :: idx1, idx2
    character(len=64) :: str_idx1, str_idx2
    character(len=128) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2))
end function

function kgen_tostr_args3(idx1, idx2, idx3) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3
    character(len=64) :: str_idx1, str_idx2, str_idx3
    character(len=192) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3))
end function

function kgen_tostr_args4(idx1, idx2, idx3, idx4) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4
    character(len=256) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4))
end function

function kgen_tostr_args5(idx1, idx2, idx3, idx4, idx5) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4, idx5
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5
    character(len=320) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    write(str_idx5, *) idx5
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
        // ", " // trim(adjustl(str_idx5))
end function

function kgen_tostr_args6(idx1, idx2, idx3, idx4, idx5, idx6) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4, idx5, idx6
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5, str_idx6
    character(len=384) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    write(str_idx5, *) idx5
    write(str_idx6, *) idx6
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
        // ", " // trim(adjustl(str_idx5)) // ", " // trim(adjustl(str_idx6))
end function
"""

kgen_utils_file_checksubr = \
"""
subroutine kgen_perturb_real4_dim1(var, pertlim)
    real*4, intent(inout), dimension(:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        call random_number(pertval)
        pertval = 2.0_4*pertlim*(0.5_4 - pertval)
        var(idx1) = var(idx1)*(1.0_4 + pertval)
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real4_dim2(var, pertlim)
    real*4, intent(inout), dimension(:,:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1,idx2

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            call random_number(pertval)
            pertval = 2.0_4*pertlim*(0.5_4 - pertval)
            var(idx1,idx2) = var(idx1,idx2)*(1.0_4 + pertval)
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real4_dim3(var, pertlim)
    real*4, intent(inout), dimension(:,:,:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1,idx2,idx3

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            do idx3=1,size(var, dim=3)
                call random_number(pertval)
                pertval = 2.0_4*pertlim*(0.5_4 - pertval)
                var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_4 + pertval)
            end do
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim1(var, pertlim)
    real*8, intent(inout), dimension(:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        call random_number(pertval)
        pertval = 2.0_8*pertlim*(0.5_8 - pertval)
        var(idx1) = var(idx1)*(1.0_8 + pertval)
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim2(var, pertlim)
    real*8, intent(inout), dimension(:,:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1,idx2

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            call random_number(pertval)
            pertval = 2.0_8*pertlim*(0.5_8 - pertval)
            var(idx1,idx2) = var(idx1,idx2)*(1.0_8 + pertval)
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim3(var, pertlim)
    real*8, intent(inout), dimension(:,:,:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1,idx2,idx3

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            do idx3=1,size(var, dim=3)
                call random_number(pertval)
                pertval = 2.0_8*pertlim*(0.5_8 - pertval)
                var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_8 + pertval)
            end do
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_init_verify(verboseLevel, tolerance, minValue)

  integer, intent(in), optional :: verboseLevel
  real(kind=kgen_dp), intent(in), optional :: tolerance
  real(kind=kgen_dp), intent(in), optional :: minValue

  if(present(verboseLevel)) then
     kgen_verboseLevel = verboseLevel
  end if

  if(present(tolerance)) then
      kgen_tolerance = tolerance
  end if

  if(present(minvalue)) then
      kgen_minvalue = minvalue
  end if

end subroutine kgen_init_verify

subroutine kgen_init_check(check, rank)

  type(check_t), intent(inout) :: check
  integer, intent(in), optional :: rank 

  check%Passed   = .TRUE.
  check%numOutTol = 0
  check%numInTol = 0
  check%numTotal = 0
  check%numIdentical = 0

  if(present(rank)) then                                                             
      check%rank = rank                                                              
  else                                                                               
      check%rank = 0                                                                 
  endif  

end subroutine kgen_init_check

subroutine kgen_print_check(kname, check)
   character(len=*) :: kname
   type(check_t), intent(in) ::  check

   write (*,*) TRIM(kname),': Tolerance for normalized RMS: ',kgen_tolerance
   !write (*,*) TRIM(kname),':',check%numFatal,'fatal errors,',check%numWarning,'warnings detected, and',check%numIdentical,'identical out of',check%numTotal,'variables checked'
   write (*,*) TRIM(kname),': Number of variables checked: ',check%numTotal
   write (*,*) TRIM(kname),': Number of Identical results: ',check%numIdentical
   write (*,*) TRIM(kname),': Number of variables within tolerance(not identical): ',check%numInTol
   write (*,*) TRIM(kname),': Number of variables out of tolerance: ', check%numOutTol

   if (check%numOutTol> 0) then
        write(*,*) TRIM(kname),': Verification FAILED'
   else
        write(*,*) TRIM(kname),': Verification PASSED'
   endif
end subroutine kgen_print_check
"""

kgen_get_newunit = \
"""
FUNCTION kgen_get_newunit() RESULT ( new_unit )
    INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
    LOGICAL :: is_opened
    INTEGER :: nunit, new_unit, counter
    REAL :: r

    CALL RANDOM_SEED
    new_unit = -1
    DO counter=1, UNIT_MAX
        CALL RANDOM_NUMBER(r)
        nunit = INT(r*UNIT_MAX+UNIT_MIN)
        INQUIRE (UNIT=nunit, OPENED=is_opened)
        IF (.NOT. is_opened) THEN
            new_unit = nunit
            EXIT
        END IF
    END DO
END FUNCTION kgen_get_newunit
"""

kgen_error_stop = \
"""
SUBROUTINE kgen_error_stop( msg )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: msg

    WRITE (*,*) msg
    STOP 1
END SUBROUTINE
"""

kgen_rankthread = \
"""
SUBROUTINE kgen_rankthreadinvoke( str, rank, thread, invoke )
    CHARACTER(*), INTENT(IN) :: str
    INTEGER, INTENT(OUT) :: rank, thread, invoke
    INTEGER :: pos1, pos2, i, e

    pos1 = 1

    rank = -1
    thread = -1
    invoke = -1

    DO
        pos2 = INDEX(str(pos1:), ".")
        IF (pos2 == 0) THEN
            READ(str(pos1:),*,IOSTAT=e) i
            IF ( e == 0 ) THEN
                rank = thread
                thread = invoke
                READ(str(pos1:), *) invoke
            END IF
            EXIT
        END IF

        READ(str(pos1:pos1+pos2-2),*,IOSTAT=e) i
        IF ( e == 0 ) THEN
            rank = thread
            thread = invoke
            READ(str(pos1:pos1+pos2-2), *) invoke
        END IF 

        pos1 = pos2+pos1
    END DO
END SUBROUTINE
"""

rdtsc = \
"""         .file   "rdtsc.s"
         .text
.globl rdtsc_
         .type   rdtsc_, @function
rdtsc_:
         rdtsc
         movl %eax,%ecx
         movl %edx,%eax
         shlq $32,%rax
         addq %rcx,%rax
         ret
         .size   rdtsc_, .-rdtsc_"""

Intrinsic_Procedures = [ \
# numeric functions \
'abs','aimag','aint','anint','ceiling','cmplx','conjg','dble','dim','dprod','floor','int','max','min','mod','modulo','nint','real','sign', \
# mathematical functions \
'acos','asin','atan','atan2','cos','cosh','exp','log','log10','sin','sinh','sqrt','tan','tanh', \
# character functions \
'achar','adjustl','adjustr','char','iachar','ichar','index','len_trim','lge','lgt','lle','llt','max','min','repeat','scan','trim','verify', \
# kind functions \
'kind','selected_char_kind','selected_int_kind','selected_real_kind', \
# miscellaneous type conversion functions \
'logical', 'transfer', \
# numeric inquiry functions \
'digits','epsilon','huge','maxexponent','minexponent','precision','radix','range','tiny', \
# array inquiry functions \
'lbound','shape','size','ubound', \
# other inquiry functions \
'allocated','associated','bit_size','extends_type_of','len','new_line','present','same_type_as', \
# bit manipulation procedures \
'btest','iand','ibclr','ibits','ibset','ieor','ior','ishft','ishftc','mvbits','not', \
# vector and matrix multiply functions \
'exponent','fraction','nearest','rrspacing','scale','set_exponent','spacing','dot_product','matmul', \
# array reduction functions \
'all' ,'any' ,'count' ,'maxval' ,'minval' ,'product' ,'sum', \
# array construction functions \
'cshift','eoshift','merge','pack','reshape','spread','transpose','unpack', \
# array location functions \
'maxloc','minloc', \
# null function \
'null', \
# allocation transfer procedure \
'move_alloc', \
# random number subroutines \
'random_number','random_seed', \
# system environment procedures \
'command_argument_count','cpu_time','date_and_time','get_command','get_command_argument','get_environment_variable','is_iostat_end','is_iostat_eor','system_clock', \
# specific names \
'alog','alog10','amax0','amax1','amin0','amin1','amod','cabs','ccos','cexp','clog','csin','csqrt','dabs','dacos','dasin','datan','dcos','dcosh','ddim','dexp','dint','dlog','dlog10','dmax1','dmin1','dmod','dnint','dsign','dsin','dsinh','dsqrt','dtan','dtanh','float','iabs','idim','idint','idnint','ifix','isign','max0','max1','min0','min1' \
]

Intrinsic_Modules = { \
    'ISO_FORTRAN_ENV': [ 'error_unit', 'file_storage_size', 'input_unit', 'iostat_end', 'iostat_eor', 'numeric_storage_size', 'output_unit', 'character_storage_size' ], \
    'ISO_C_BINDING': [ 'c_associated', 'c_f_pointer', 'c_f_procpointer', 'c_funloc', 'c_loc', 'c_sizeof', 'c_int', 'c_short', 'c_long', 'c_long_long', 'c_signed_char', 'c_size_t', 'c_int8_t', 'c_int16_t', 'c_int32_t', 'c_int64_t', 'c_int_least8_t', 'c_int_least16_t', 'c_int_least32_t', 'c_int_least64_t', 'c_int_fast8_t', 'c_int_fast16_t', 'c_int_fast32_t', 'c_int_fast64_t', 'c_intmax_t', 'c_intptr_t', 'c_ptrdiff_t', 'c_float', 'c_double', 'c_long_double', 'c_float_complex', 'c_double_complex', 'c_long_double_complex', 'c_bool', 'c_char', 'c_null_char', 'c_alert', 'c_backspace', 'c_form_feed', 'c_new_line', 'c_carriage_return', 'c_horizontal_tab', 'c_vertical_tab', 'c_null_ptr', 'c_ptr', 'c_null_funptr', 'c_funptr ' ], \
    'IEEE_EXCEPTIONS': [ 'ieee_flag_type', 'ieee_status_type', 'ieee_support_flag', 'ieee_support_halting', 'ieee_get_flag', 'ieee_get_halting_mode', 'ieee_get_status', 'ieee_set_flag', 'ieee_set_halting_mode', 'ieee_set_status' ], \
    'IEEE_ARITHMETIC': [ 'ieee_class_type', 'ieee_round_type', 'ieee_support_datatype', 'ieee_support_divide', 'ieee_support_denormal', 'ieee_support_inf', 'ieee_support_io', 'ieee_support_nan', 'ieee_support_rounding', 'ieee_support_standard', 'ieee_support_underflow_control', 'ieee_support_sqrt', 'ieee_class', 'ieee_copy_sign', 'ieee_is_finite', 'ieee_is_nan', 'ieee_is_normal', 'ieee_is_negative', 'ieee_is_logb', 'ieee_next_after', 'ieee_rem', 'ieee_rint', 'ieee_scalb', 'ieee_unordered', 'ieee_value', 'ieee_selected_real_kind', 'ieee_get_rounding_mode', 'ieee_get_underflow_mode', 'ieee_set_rounding_mode', 'ieee_set_underflow_mode' ], \
    'IEEE_FEATURES': [ 'ieee_features_type' ] \
}

