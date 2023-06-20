program integral
  implicit none

  external f

  real*8 :: xmin, xmax, x, fx, deltax, tempr, f
  integer :: n, i

  xmin = 0.0
  xmax = 10.0
  n = 1000000

  deltax = (xmax - xmin) / dfloat(n-1)

  tempr = 0.0
  do i = 1, n
    x = xmin + dfloat(i-1)*deltax
    tempr = tempr + deltax * f(x)
  enddo

  print *, "area ret = ", tempr

  call system('gfortran maintrap.f90 && ./a.out')

end program integral

real*8 function f(x)
  implicit none

  real*8 :: x

  f = x**3 * sin(x)
end function
