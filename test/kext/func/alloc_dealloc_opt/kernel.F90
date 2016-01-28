module kernel

        public add

        contains

        subroutine add(a1, a2, a3)

                real(kind=4), dimension(:,:,:,:), intent(in) :: a1
                real(kind=4), dimension(:,:,:,:), intent(in) :: a2
                real(kind=4), dimension(:,:,:,:), intent(out) :: a3

                a3 = a1 + a2

        end subroutine

end module
