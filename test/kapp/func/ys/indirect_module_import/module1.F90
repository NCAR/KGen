module module1
use module2
use kernel, only : real_kind, np

real(real_kind), parameter :: TEST_VALUE = 1.0_real_kind

public TEST_VALUE, real_kind

contains

subroutine dummy()
    integer i
    i = 1
end subroutine

end module
