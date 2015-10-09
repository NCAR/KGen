module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        function add(a, b, c) result(d)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(in) :: c
                real(kind=real_kind), dimension(np,np) :: d

                d = c + a + b

        end function add

end module kernel
