module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = NUM 
        public add
       
        contains

        subroutine add(a, b, c, d, e, f, g, h, i)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b, d
                real(kind=real_kind), dimension(np,np), intent(out) :: c, e, f, g, h, i

                c = a + b + d
                e = a + 1.0
                f = a + b + 1.0
                g = a + 2.0
                h = a + b + 2.0
                i = b + 3.0

        end subroutine add

end module kernel
