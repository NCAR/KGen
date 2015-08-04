module test1_mod
        
        use compute, only : compute_add
        public test_1

        contains

        subroutine test_1()

        integer(kind=4), parameter :: real_kind = 8
        integer(kind=4), parameter :: np = 4        

        type :: derived_type1
       
                !real(kind=real_kind), dimension(np,np) :: a
                real(kind=real_kind) a(np,np)
                real(kind=real_kind) b(np,np)
                real(kind=real_kind) c(np,np)
        
        end type derived_type1
        
        integer(kind=4) :: niter = 10
        integer(kind=4) :: n
        type(derived_type1) d1

        do i=1,np
                do j=1,np
                        d1%a(i,j) = 1.0
                        d1%b(i,j) = 1.0
                        !print *, "a(", i, ")(", j, ") = ", d1.a(i,j)
                end do
        end do

        do n=1,niter
                call compute_add(d1%a, d1%b, d1%c)
       !         do i=1,np
       !                 do j=1,np
       !                         print *, "c(", i, ")(", j, ") = ", d1.c(i,j)
       !                 end do
       !         end do
        end do

        end subroutine test_1
end module test1_mod

