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

  ! h = 1.0d-3
  h = 2 * sqrt(epsilon(1.0d0))


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

function f(x) result(y)
  implicit none

  real*8 :: x, y

  y = x**2 + 2*x
end function f
