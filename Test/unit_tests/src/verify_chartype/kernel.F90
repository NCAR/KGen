module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        subroutine add(a, b, c, str1, str2)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(out) :: c
                character(*), intent(inout) :: str1
                character(*), intent(inout) :: str2

                c = a + b

                str1 = 'abcde'
                str2 = 'abcdefghij'

        end subroutine add

end module kernel
