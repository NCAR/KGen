module calling_module

        use module1

        public calling_subroutine

        contains

        subroutine calling_subroutine(testvalue)

                !real(kind=real_kind), dimension(np,np) :: ar1, ar2, ar3
                integer(kind=real_kind), intent(in) :: testvalue
                type(typeA), allocatable :: atype(:)     ! test comment
                real(real_kind) :: value = TEST_VALUE     ! test comment
                integer i, j, k

                allocate(atype(2))
                do i=1,2
                    allocate(atype(i)%a(np,np))
                    allocate(atype(i)%b(np,np))
                    !allocate(atype(i)%c(np,np))
                    do j=1,np
                        do k=1,np
                            atype(i)%a(j,k) = 1.0
                            atype(i)%b(j,k) = 1.0
                        end do
                    end do
                end do

                call add(atype(1), value + testvalue, kvalue)

                call add(atype(2), value + testvalue, kvalue)

                print *, "C =", atype(1)%c


                deallocate(atype(1)%a)
                deallocate(atype(1)%b)
                !deallocate(atype(1)%c)
                deallocate(atype(2)%a)
                deallocate(atype(2)%b)
                !deallocate(atype(2)%c)

                deallocate(atype)

        end subroutine

        subroutine add(atype, value, kvalue)
            type(typeA), intent(inout) :: atype
            real(real_kind), intent(in) :: value
            real(real_kind), intent(out) :: kvalue

            call add_kernel(atype)
            atype%c = atype%c + value
            save_array(1,1) = atype%a(1,1)
            save_value = save_array(1,1) + value
            kvalue = save_value
        end subroutine

end module
