module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add

        type dtype_hdr_A
            real(kind=real_kind), dimension(np,np) :: x
            type(dtype_hdr_B), pointer :: nexthdr => null()
        end type dtype_hdr_A

        type dtype_hdr_B
            real(kind=real_kind), dimension(np,np) :: x
            type(dtype_hdr_A), pointer :: nexthdr => null()
        end type dtype_hdr_B

        type dtype_hdr_C
            real(kind=real_kind), dimension(np,np) :: x
            type(dtype_hdr_C), pointer :: nexthdr => null()
        end type dtype_hdr_C
      
        contains

        subroutine add(a, b, c)
            real(kind=real_kind), dimension(np,np), intent(in) :: a
            real(kind=real_kind), dimension(np,np), intent(in) :: b
            real(kind=real_kind), dimension(np,np), intent(out) :: c
            type(dtype_hdr_A) :: da
            type(dtype_hdr_B), TARGET :: db
            type(dtype_hdr_C) :: dc

            da%nexthdr => db 
            da%x = a
            da%nexthdr%x = b

            dc%x = da%x + db%x 
            c = dc%x

        end subroutine add

end module kernel
