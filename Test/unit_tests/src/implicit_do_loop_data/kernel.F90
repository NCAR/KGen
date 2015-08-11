module kernel

        integer(kind=4), parameter :: arraysize=10
        public add

        contains

        subroutine add(a, b, c)

                real(kind=4), dimension(arraysize), intent(in) :: a, b
                real(kind=4), dimension(arraysize), intent(out) :: c

                !print *, "a=", a
                c = b + a
                !print *, "c= ", c

        end subroutine

end module
