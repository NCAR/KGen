module calling_module

        USE kernel
        USE types_mod

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                
                !integer(kind=4), parameter :: real_kind = 4
                integer(kind=4), parameter :: niter = 10
                !integer(kind=4), parameter :: np = 4

                type(complex_type) :: test_type1(niter)

                !initialization
                do i=1,niter
                        test_type1(i)%element_1 = 1.0
                        !do j=1,np
                        !        do k=1,np
                                        test_type1(i)%elem1%a(:,:) = 1.0
                                        test_type1(i)%elem1%b(:,:) = 1.0
                                        test_type1(i)%elem2%level3_1%param1(:,:) = 1.0
                                        test_type1(i)%elem2%level3_1%param2(:,:) = 1.0
                        !        end do
                        !end do

                        test_type1(i)%elem2%d = 1.0
                        test_type1(i)%elem2%e = 1.0
                        test_type1(i)%elem2%f = 1.0

                end do

                !perform computation
                !do i=1,niter
                        call add(test_type1)
                !end do

                print *, 'test value = ', test_type1(0)%elem1%a

        end subroutine calling_subroutine

end module calling_module
                

