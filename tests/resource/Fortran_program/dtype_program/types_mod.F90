module types_mod

        implicit none

        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 4
        type, public :: level_3

                real(real_kind) :: param1(np,np) = 0
                real(real_kind) :: param2(np,np) = 0

        end type level_3

        type, public :: elem_mimic

                        real(kind=real_kind), dimension(np,np) :: a = 0
                        real(kind=real_kind), dimension(np,np) :: b = 0
                        real(kind=real_kind), dimension(np,np) :: c = 0

        end type elem_mimic

        type, public :: elem_mimic2
 
                        real(kind=real_kind) :: d = 0
                        real(kind=real_kind) :: e = 0
                        real(kind=real_kind) :: f = 0
                        
                        type(level_3) :: level3_1

        end type elem_mimic2


        type, public :: complex_type

                real(kind=real_kind) :: element_1

                type(elem_mimic) :: elem1

                type(elem_mimic2) :: elem2

        end type complex_type

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
 
