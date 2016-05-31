! comment a
module kernel
        
        ! comment b

        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        ! comment c
        subroutine add(a, b, c)
                ! comment d
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(out) :: c

                c = a + b

                ! comment e
        end subroutine add

        ! comment f

        subroutine sub(a, b)
            ! comment g
        end subroutine
        ! comment h
end module kernel

! comment i
