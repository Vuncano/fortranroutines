program test
  implicit none
  
  external f1

  real*8 :: f1, derf, x0
  integer :: n

  x0 = 2.0d0
  n = 1

  call nderivative(f1, x0, n, derf)

  print*, derf

end program test

real*8 function f1(x)
  implicit none

  real*8 :: x

  f1 = x**2 + x
end function f1

include "der.f90"
