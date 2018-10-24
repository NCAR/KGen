module kernel
 
    integer(kind=4), parameter :: real_kind=4
    integer(kind=4), parameter :: np = 2
       
    type sampletype
        real(kind=real_kind), dimension(np,np) :: ta
        real(kind=real_kind), dimension(np,np) :: tb
    end type sampletype
    type(sampletype) :: tvar
    
    public init, add
       
    contains

    subroutine init(a, b)
        real(kind=real_kind), dimension(np,np), intent(in) :: a
        real(kind=real_kind), dimension(np,np), intent(in) :: b
        tvar%ta = a
        tvar%tb = b
    end subroutine

    subroutine add(a, b, c)
        real(kind=real_kind), dimension(np,np), intent(in) :: a
        real(kind=real_kind), dimension(np,np), intent(in) :: b
        real(kind=real_kind), dimension(np,np), intent(out) :: c

        c = a + b + tvar%tb
        tvar%tb = b

    end subroutine add

end module kernel
