module test4_mod

        USE compute, only : add

        public test_4

        contains

        subroutine test_4()

                integer(kind=4), parameter :: real_kind = 4
                integer(kind=4), parameter :: arraysize = 10

                real(kind=real_kind) :: array_1(arraysize), &
                                                        array_2(arraysize), &
                                                        array_3(arraysize)

                data array_1 /1.0_real_kind, -1.0_real_kind, 1.0_real_kind, -1.0_real_kind, 1.0_real_kind, &
                           -1.0_real_kind, 1.0_real_kind, -1.0_real_kind, 1.0_real_kind, -1.0_real_kind/

                data array_2 /-1.0_real_kind, 1.0_real_kind, -1.0_real_kind, 1.0_real_kind, -1.0_real_kind, &
                           1.0_real_kind, -1.0_real_kind, 1.0_real_kind, -1.0_real_kind, 1.0_real_kind/


                call add(array_1, array_2, array_3)

                !print *, "a3 = ", array_3

        end subroutine

end module

                
