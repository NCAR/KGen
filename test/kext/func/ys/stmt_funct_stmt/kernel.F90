module kernel

        public add

        contains

        subroutine add(a1, a2, a3)

                !real(kind=4) F, G, s
                real(kind=4) s
                real(kind=4), dimension(:,:,:,:), intent(in) :: a1
                real(kind=4), dimension(:,:,:,:), intent(in) :: a2
                F(a,b) = a + b

                real(kind=4), dimension(:,:,:,:), intent(out) :: a3

                G(a,b) = a * b

                a3 = a1 + a2

                s = F(a1(1,1,1,1), a2(1,1,1,1)) + G(a1(1,1,1,1), a2(1,1,1,1))
                a3(1,1,1,1) = s

        end subroutine

end module
