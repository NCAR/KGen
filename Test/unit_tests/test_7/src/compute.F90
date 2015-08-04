module compute

        public add

        contains

        subroutine add(a, b, r1, r2, r3)

                logical, intent(in) :: a, b
                real(kind=4), dimension(:,:), intent(in) :: r1, r2
                real(kind=4), dimension(:,:), intent(out) :: r3

                !print *, "a=", a
                if(a .neqv. b) then
                        r3 = r1 + r2
                end if

                if(a .eqv. b) then
                        r3 = r1-r2
                end if


                print *, "r3= ", r3

        end subroutine

end module
