module kernel

        public add

        contains

        subroutine add(a1, a2, a3)

                real(kind=4), dimension(:,:,:,:), intent(in) :: a1
                real(kind=4), dimension(:,:,:,:), intent(in) :: a2
                real(kind=4), dimension(:,:,:,:), intent(out) :: a3

                integer(kind=4), parameter :: knew_size=9
                integer(kind=4), parameter :: ktemp_size=knew_size+1, karray_size = ktemp_size

                a3 = a1 + a2 + karray_size

        end subroutine

end module
