program main
  implicit none

  external nderivative
  external f

  real*8 :: x0, xmin, xmax, f
  integer :: i, n, nx

  xmin = -10.0d0
  xmax = 10.0d0
  nx = 1000

  n = 14

  call plot_function(f, xmin, xmax, nx)

  do i = 1,n
     call plot_taylor_exp(f, x0, xmin, xmax, nx, i)
  end do
end program main

function f(x) result(y)
  implicit none

  real*8 :: x, y
  y = sin(x)
end function f

subroutine plot_function(f, xmin, xmax, nx)
  implicit none

  interface
    function f(x) result(y)
       implicit none

       real*8 :: x, y
    end function f
  end interface

  real*8, intent(in) :: xmin, xmax
  integer, intent(in) :: nx
  real*8 :: delx, x, y
  integer :: i

  open(unit=1, file="taylor00.dat")

  delx = (xmax - xmin)/dfloat(nx)

  do i = 1, nx
     x = xmin + dfloat(i-1)*delx
     y = f(x)
     write(1,*) x, y
  end do
end subroutine plot_function

subroutine plot_taylor_exp(f, x0, xmin, xmax, nx, n)
  implicit none

  external itoa

  interface
    function f(x) result(y)
       implicit none

       real*8 :: x, y
    end function f
  end interface

  character(len=2) :: itoa
  real*8, intent(in) :: x0, xmin, xmax
  integer, intent(in) :: nx, n
  real*8 :: delx, x, y
  integer :: i

  delx = (xmax - xmin)/dfloat(nx)

  open(unit=1, file="taylor"//trim(itoa(n))//".dat")

    do i = 1,nx
     x = xmin + dfloat(i-1)*delx
     call taylor_exp(f, x, x0, n, y)

     write(1,*) x, y
  end do

  close(1)
end subroutine plot_taylor_exp

function itoa(i) result(a)
  implicit none

  integer :: i
  character(len=2) :: a
  character(len=8) :: fmt

  fmt = '(I2.2)'

  write(a,fmt) i
end function itoa

subroutine taylor_exp(f, x, x0, n, taylor)
  implicit none

  external fact

  interface
    function f(x) result(y)
       implicit none

       real*8 :: x, y
    end function f
  end interface

  real*8, intent(in) :: x0, x
  real*8, intent(out) :: taylor
  integer, intent(in) :: n
  real*8 :: derf
  integer :: i, fact

  taylor = 0.0d0
  do i = 0,n
     call nderivative(f, x0, i, derf)
     taylor = taylor + (derf/dfloat(fact(i)))(x - x0)*i
  end do
end subroutine taylor_exp

function fact(n) result👍
  implicit none

  integer :: n, i, y
  y = 1
  do i = 1,n
     y = y * i
  end do
end function fact

recursive subroutine nderivative(f, x0, n, derf)
  implicit none

  interface
    function f(x) result(y)
      implicit none

       real*8 :: x, y
    end function f
  end interface

  real*8, intent(in) :: x0
  integer, intent(in) :: n
  real*8, intent(out) :: derf
  real*8 :: h, derf1, derf2

  h = 1.0d-3

  if (n == 0) then
     derf = f(x0)
     return
  end if

  if (n == 1) then
     derf = (f(x0 + h) - f(x0 - h))/(2.0d0*h)
     return
  end if

  call nderivative(f, x0 + h, n-1, derf1)
  call nderivative(f, x0 - h, n-1, derf2)

  derf = (derf1 - derf2)/(2.0d0*h)
end subroutine nderivative
