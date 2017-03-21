module kernel
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        real(kind=real_kind), dimension(np, np) :: save_array
        real(kind=real_kind) :: save_value, temp_value
        real(real_kind) :: kvalue
        integer, save :: block0 = 0, block1 = 0

        type typeA
            ! test comment 1
            real(kind=real_kind), pointer, dimension(:, :) :: a, b, c ! test comment 2
        end type        

        public add, typeA, save_array, save_value
       
        contains

        subroutine add(atype)
                type(typeA), intent(inout) :: atype
                integer i, j, temp

                save_value = 0.0_real_kind

                do i=1,np
                        do j=1,np
                            atype%c(i,j) = atype%a(i,j) + atype%b(i,j) + i + j
                            if ( atype%c(i,j) .gt. 4) then
                                save_value = save_value + 1.0_real_kind
                                block0 = block0 + 1
                            else
                                save_value = save_value + 0.5_real_kind
                                block1 = block1 + 1
                            end if
                        end do
                end do

        end subroutine add

end module kernel
