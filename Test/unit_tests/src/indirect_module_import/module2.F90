module module2
    USE kernel, only : add_kernel => add, typeA

    private

    public add_kernel, typeA

contains

    subroutine dummy2()
        integer i
        i = 1
    end subroutine

end module
