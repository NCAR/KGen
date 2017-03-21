program test_top
        USE kernel, only : real_kind
        USE calling_module

        real(kind=real_kind) testvalue
        integer i

        testvalue = 1.0_real_kind

        do i=1,10
            call calling_subroutine(testvalue)
        end do

end program test_top
