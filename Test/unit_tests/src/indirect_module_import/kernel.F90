module kernel
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2

        type typeA
            integer(kind=real_kind), dimension(np, np) :: a, b, c
        end type        

        public add, typeA
       
        contains

        subroutine add(atype)
                type(typeA), intent(inout) :: atype

                atype%c = atype%a + atype%b

        end subroutine add

end module kernel
