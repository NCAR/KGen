module types_mod

        implicit none

        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 4
        type, public :: level_3

                real(real_kind) :: param1(np,np)
                real(real_kind) :: param2(np,np)

        end type level_3

        type, public :: elem_mimic

                        real(kind=real_kind), dimension(np,np) :: a
                        real(kind=real_kind), dimension(np,np) :: b
                        real(kind=real_kind), dimension(np,np) :: c

        end type elem_mimic

        type, public :: elem_mimic2
 
                        real(kind=real_kind) :: d
                        real(kind=real_kind) :: e
                        real(kind=real_kind) :: f
                        
                        type(level_3) :: level3_1

        end type elem_mimic2


        type, public :: complex_type

                real(kind=real_kind) :: element_1

                type(elem_mimic) :: elem1

                type(elem_mimic2) :: elem2

        end type complex_type

end module types_mod
 
