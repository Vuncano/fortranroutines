program teste
  implicit none

  external f

  real*8 :: result, xmin, xmax, f, result2
  integer :: n
  
  xmin = 0.0d0
  xmax = 2.0d0
  n = 100000

  call integral(f, xmin, xmax, n, result)

  write(*,*) "integral metodo retangulo = ", result

  call integralTrap(f, xmin, xmax, n, result2)

  write(*,*) "integral metodo do trapesio = ", result2

end program teste

real*8 function f(x)
  real*8 :: x

  f = x**2
end function

include "integral.f90"
include "trapesio.f90"
