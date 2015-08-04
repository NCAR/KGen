module compute

        public add

        contains

        subroutine add()

                common a, b
                real(kind=4), dimension(2,2) :: c

                !real(kind=4), dimension(:,:,:,:), intent(in) :: a1
                !real(kind=4), dimension(:,:,:,:), intent(in) :: a2
                !real(kind=4), dimension(:,:,:,:), intent(out) :: a3

                c = b + a
                print *, "c= ", c

        end subroutine

end module
