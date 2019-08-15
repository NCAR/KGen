module kernel
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        real(kind=real_kind), dimension(np, np) :: save_array
        real(kind=real_kind) :: save_value, temp_value
        real(real_kind) :: kvalue

        type typeA
            ! test comment 1
            real(kind=real_kind), pointer, dimension(:, :) :: a, b ! test comment 2
            real(kind=real_kind), dimension(np, np) :: c ! test comment 2
        end type        

        public add, typeA, save_array, save_value
       
        contains

        subroutine add(atype)
                type(typeA), intent(inout) :: atype
                integer i, j

                do i=1,np
                    do j=1,np
                        atype%c(i,j) = atype%a(i,j) + atype%b(i,j)
                    end do
                end do
                !$kgen write atype%c
                save_value = atype%c(1,1)
        end subroutine add

end module kernel
