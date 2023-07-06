program test
  implicit none

  external f

  real*8 :: f, x1, tol, root, froot, a, b
  integer :: n

  tol = 10e-9
  x1 = 1.0d0

  call newton_raphson(f, x1, tol, n, root, froot)

  print*, "======= NEWTON-RAPHSON ======="
  print *, "number of tries= ", n
  print *, "root= ", root
  print *, "f(root)= ", froot

  a = 0
  b = 2

  n = 0
  root = 0
  froot = 0
  call bs(f, a, b, tol, n, root, froot)

  print*, "======= BISSEÇÃO ======="
  print *, "number of tries= ", n
  print *, "root= ", root
  print *, "f(root)= ", froot

end program test

real*8 function f(x)
  real*8 x

  f = exp(x) - x - 2
end function

include "newton-raphson.f90"

include "abs.f90"
