program integral
  implicit none

  external f

  real*8 :: xmin, xmax, x, fx, delx, tempr, f
  integer :: n, i

  xmin = 1.0
  xmax = 2.0
  n = 10000

  delx = (xmax - xmin) / dfloat(n-1)

  tempr = 0.0
  do i = 1, n
    x = xmin + dfloat(i-1)*delx
    tempr = tempr + delx * f(x)
  enddo

  print *, tempr

end program integral

real*8 function f(x)
  implicit none

  real*8 :: x

  f = x**2 - 1.0
end function
