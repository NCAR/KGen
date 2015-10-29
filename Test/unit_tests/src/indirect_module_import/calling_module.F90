module calling_module

        use module1

        public calling_subroutine

        contains

        subroutine calling_subroutine()

                !real(kind=real_kind), dimension(np,np) :: ar1, ar2, ar3
                type(typeA) :: atype     ! test comment
                real(real_kind) :: value = TEST_VALUE     ! test comment

                atype%a = 1.0
                atype%b = 1.0

                call add(atype, value)

                print *, "C =", atype%c

        end subroutine

        subroutine add(atype, value)
            type(typeA), intent(inout) :: atype
            real(real_kind), intent(in) :: value

            call add_kernel(atype)
        end subroutine

end module
