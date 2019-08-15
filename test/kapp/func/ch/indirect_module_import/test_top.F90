program test_top
        USE kernel, only : real_kind
        USE calling_module

        integer(kind=real_kind) testvalue

        testvalue = 1

        call calling_subroutine(testvalue)

end program test_top
