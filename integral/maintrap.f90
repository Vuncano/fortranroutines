program IntegralTrap
  implicit none

  external f

  real*8 :: x, xmin, xmax, f, deltax, tempt, areatot
  integer :: i, n

  xmin = 0.d0
  xmax = 10.d0
  n = 1000000

  deltax = (xmax - xmin) / dfloat(n-1)

  tempt = 0.d0
  x = 0.d0
  do i = 2, (n-1)
    x = xmin + dfloat(i-2)*deltax  
    tempt = tempt + f(x)
  end do

  areatot = (deltax / 2) * (f(1.d0) + 2*tempt + f(xmax))
  write(*,*) "area trap = ", areatot

end program IntegralTrap

real*8 function f(x)
  implicit none

  real*8 x

  f = x**3 * sin(x)
end function
