module types_mod
        use types_mod1, only : complex_type, real_kind

        implicit none

        type, public, extends(complex_type) :: extended_type

                real(kind=real_kind) :: element_2 = 0

        end type extended_type

    type(extended_type), save :: exttypevar

    public add2ext, exttypevar

contains

    subroutine add2ext(val)
        real(kind=real_kind), intent(in) :: val

        exttypevar%element_2 = exttypevar%element_2 + val
        exttypevar%elem2%level3_1%param1(1,1) = exttypevar%element_2
    end subroutine
    
end module types_mod
 
