module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add

        type dtype_hdr
            real(kind=real_kind), dimension(np,np) :: x
            type(dtype_hdr), pointer :: nexthdr => null()
        end type dtype_hdr

       
        contains

        subroutine add(a, b, c)
            real(kind=real_kind), dimension(np,np), intent(in) :: a
            real(kind=real_kind), dimension(np,np), intent(in) :: b
            real(kind=real_kind), dimension(np,np), intent(out) :: c
            type(dtype_hdr) :: d1
            type(dtype_hdr), target :: d2

            d1%nexthdr => d2 
            d1%x = a
            d1%nexthdr%x = b

            c = d1%x + d2%x 

        end subroutine add

end module kernel
