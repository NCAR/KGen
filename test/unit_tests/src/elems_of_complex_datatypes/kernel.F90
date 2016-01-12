module kernel
        
        integer(kind=4), parameter :: real_kind=8
        integer(kind=4), parameter :: np = 4
        public add
       
        contains

        subroutine add(a, b, c)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(out) :: c

                integer(kind=4) :: i,j
                do i= 1,np
                        do j=1,np
                        !        print *, "a(", i, ")(", j, ") = ", a(i,j)
                                c(i,j) = a(i,j) + b(i,j)
                        end do
                end do
        end subroutine add

end module kernel
