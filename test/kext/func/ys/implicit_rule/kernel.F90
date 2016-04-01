module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        subroutine add(a, b, c, kount1)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(inout) :: c
                !real, intent(in) :: var_temp
                another_real = 54.234
                int_another = 15
                print *,  kount1

                c = a + b + c + kount1
      end subroutine add

end module kernel
