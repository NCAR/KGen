module calling_module

        USE kernel
        USE types_mod

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                
                !integer(kind=4), parameter :: real_kind = 4
                integer(kind=4), parameter :: niter = 10
                !integer(kind=4), parameter :: np = 4

                type(complex_type), target :: test_type1(niter)
                CLASS(complex_type), pointer, dimension(:) :: ptr

                ptr => test_type1

                !initialization
                do i=1,niter
                        ptr(i)%element_1 = 1.0
                        !do j=1,np
                        !        do k=1,np
                                        ptr(i)%elem1%a(:,:) = 1.0
                                        ptr(i)%elem1%b(:,:) = 1.0
                                        ptr(i)%elem2%level3_1%param1(:,:) = 1.0
                                        ptr(i)%elem2%level3_1%param2(:,:) = 1.0
                        !        end do
                        !end do

                        ptr(i)%elem2%d = 1.0
                        ptr(i)%elem2%e = 1.0
                        ptr(i)%elem2%f = 1.0

                end do

                !perform computation
                !do i=1,niter
                        call add(ptr)
                !end do

                print *, 'test value = ', ptr(0)%elem1%a

        end subroutine calling_subroutine

end module calling_module
                

