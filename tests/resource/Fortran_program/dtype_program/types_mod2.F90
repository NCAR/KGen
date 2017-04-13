module types_mod2

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
end module types_mod2
 
