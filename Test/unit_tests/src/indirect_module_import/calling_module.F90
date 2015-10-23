module calling_module

        use module1

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                !real(kind=real_kind), dimension(np,np) :: ar1, ar2, ar3
                type(typeA) :: atype     ! test comment

                atype%a = 1.0
                atype%b = 1.0

                call add(atype)

                print *, "C =", atype%c

        end subroutine

        subroutine add(atype)
            type(typeA), intent(inout) :: atype

            call add_kernel(atype)
        end subroutine

end module
