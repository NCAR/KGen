program test_top

        USE calling_module

        real(kind=4), dimension(2,2) :: &
        &arg1, &
        &arg2

        real(kind=4), dimension(2,2) :: &
        &arg3, &
        &arg3_1, &
        &arg3_2, &
        &arg3_3, &
        &arg4, &
        &arg4_1, &
        &arg4_2

        real(kind=4), dimension(2,2) :: &
        &arg5, &
        &arg6

        arg1 = 1.0
        arg2 = 1.0
        arg5 = 1.0
        arg6 = 1.0


        call calling_subroutine(arg1, arg2, arg3, arg3_1, arg3_2, arg3_3, arg4, arg4_1, arg4_2, arg5, arg6)

        print *, 'arg3 = ', arg3
        print *, 'arg3_1 = ', arg3_1
        print *, 'arg3_2 = ', arg3_2
        print *, 'arg3_3 = ', arg3_3
        print *, 'arg4 = ', arg4
        print *, 'arg4_1 = ', arg4_1
        print *, 'arg4_2 = ', arg4_2
        print *, 'arg5 = ', arg5

end program test_top
