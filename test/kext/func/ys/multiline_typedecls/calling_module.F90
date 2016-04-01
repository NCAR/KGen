module calling_module

        USE kernel, only : add

        public calling_subroutine

        contains

        subroutine calling_subroutine(arg1, arg2, arg3, arg3_1, arg3_2, arg3_3, arg4, arg4_1, arg4_2, arg5, arg6)

                real(kind=4), intent(in), dimension(2,2) :: &
                &arg1, &
                &arg2

                real(kind=4), intent(out), dimension(2,2) :: &
                &arg3, &
                &arg3_1, &
                &arg3_2, &
                &arg3_3, &
                &arg4, &
                &arg4_1, &
                &arg4_2

                real(kind=4), intent(inout), dimension(2,2) :: &
                &arg5, &
                &arg6

                real(kind=4), dimension(2,2) :: &
                &ar1, &
                &ar2, &
                &ar3, &
                &ar4, &
                &ar5
        

                ar1(:,:) = 1.0 + arg1
                ar2(:,:) = 1.0 + arg5
                ar4(:,:) = 1.0
                ar5(:,:) = 1.0

                !$kgen begin_callsite add
                call add(ar1, ar2, ar3, arg1, arg3, arg3_2, arg4, arg4_2, arg5)
                !$kgen end_callsite

                arg3_1 = ar3 + ar1
                arg3_3 = ar3 + ar2
                arg4_1 = ar3 + ar2 + 1.0

                print *, "ar3 =", ar3
                print *, "ar4 =", ar4
                print *, "ar5 =", ar5

        end subroutine

end module
