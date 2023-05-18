program plots
  implicit none

  real(kind = 8) :: x, y, f1, min, max, delx
  integer :: nx, i

  open(unit = 1, file = 'exp.dat')

  min = 1.1d0
  max = 2.0d0
  nx = 500
  delx = (max-min)/dfloat(nx)

  do i = 1, nx
    x = min + (i-1) * delx

    y = f1(x)

    print *, x, y

    write (1, *)x, y

  end do

  close(1)

call system ('python plots.py')

end program plots

real*8 function f1(x)
  implicit none
  real*8 x

  f1 = exp(x) - x - 2

end function f1
