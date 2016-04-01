module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np_start = 10000, np_end = 20000 
        public add
       
        contains

        subroutine add(a, b, c)
                integer i, j
                real(kind=real_kind), dimension(np_start:np_end,np_start:np_end), intent(in) :: a
                real(kind=real_kind), dimension(np_start:np_end,np_start:np_end), intent(in) :: b
                real(kind=real_kind), dimension(np_start:np_end,np_start:np_end), intent(out) :: c

                do i=np_start,np_end
                    do j=np_start,np_end
                        c(i,j) = a(i,j) + b(i,j)
                    end do
                end do

        end subroutine add

end module kernel
