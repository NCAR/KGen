module test6_mod

        USE compute, only : add

        public test_6

        contains

        subroutine test_6()

                integer(kind=4), parameter :: real_kind = 4
                integer(kind=4), parameter :: arraysize=10

                real(kind=4), dimension(arraysize) :: a, b, c

                data (a(i), i=1,10) /1.0, 1.0, 1.0, 1.0, 1.0, &
                                    1.0, 1.0, 1.0, 1.0, 1.0/ 

                data (b(i), i=1,10) /1.0, 1.0, 1.0, 1.0, 1.0, &
                                    1.0, 1.0, 1.0, 1.0, 1.0/

                call add(a, b, c)


        end subroutine

end module

                
