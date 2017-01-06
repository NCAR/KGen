program test_top
        USE kernel, only : real_kind
        USE calling_module

        integer(kind=real_kind) i

        do i=1,10
            call calling_subroutine(i)
        end do

end program test_top
