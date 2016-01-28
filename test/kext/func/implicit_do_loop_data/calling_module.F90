module calling_module

        USE kernel, only : add

        public calling_subroutine

        contains

        subroutine calling_subroutine()

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

                
