module types_mod1
        use types_mod2, only : elem_mimic, elem_mimic2, real_kind

        implicit none

        type, public :: complex_type

                real(kind=real_kind) :: element_1

                type(elem_mimic) :: elem1

                type(elem_mimic2) :: elem2

        end type complex_type
    
end module types_mod1
 
