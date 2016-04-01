module kernel
        
        !USE types
        type matrix
                real elem
        end type

        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public assignment ( = )

        interface assignment ( = )
                module procedure add
        end interface

        contains

        subroutine add(X, Y)
                !real(kind=real_kind), dimension(np,np), intent(in) :: a
                !real(kind=real_kind), dimension(np,np), intent(out) :: b

                real, intent(in), dimension(:,:) :: Y
                type(matrix), intent(out), dimension(size(Y,1),size(Y,2)) :: X

                X(:,:)%elem = Y(:,:)

                !print *, "x = ", X(:,:)%elem

                !b = a

        end subroutine

end module kernel
