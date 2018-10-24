module kernel
        
        integer(kind=4), parameter :: real_kind=4
        integer(kind=4), parameter :: np = 2
        public add
       
        contains

        recursive subroutine add(a, b, c, depth)
                real(kind=real_kind), dimension(np,np), intent(in) :: a
                real(kind=real_kind), dimension(np,np), intent(in) :: b
                real(kind=real_kind), dimension(np,np), intent(inout) :: c
                integer, intent(in), optional :: depth 
                real(kind=real_kind), dimension(np,np) :: d
                d = 0.0

                if (present(depth) .and. depth>0) then
                    call add(a, b, d, depth-1)
                end if

                c = a + b + d

        end subroutine add

end module kernel
