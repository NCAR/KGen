module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        real(kind=4), parameter :: FOUR = 4.0

        public add
       
        contains

        subroutine add(a, b, c)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(out) :: c

                real(kind=real_kind), dimension(np,np)  :: ar1, ar2
                data ar1 / 1.0, 2.0, 3.0, 4.0 /, ar2 / 1.0, 2.0, 3.0, FOUR /

                c = a + b + ar2

        end subroutine add

end module kernel
