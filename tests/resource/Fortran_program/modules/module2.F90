module module2
    USE kernel, only : add_kernel => add, typeA, save_array, save_value, kvalue

    private

    public add_kernel, typeA, save_array, save_value, kvalue

contains

    subroutine dummy2()
        integer i
        i = 1
    end subroutine

end module
