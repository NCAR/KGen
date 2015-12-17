module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        subroutine add(a, b, c)
        !Comment One
                real(kind=real_kind), dimension(np,np), intent(in) :: a
            !Comment Two
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                !Comment Three
                real(kind=real_kind), dimension(np,np), intent(out) :: c
             
                c = a &         !Another comment
                        + b       !Comment Four

        end subroutine add

end module kernel
