subroutine bs(f, a, b, eps, root, nmax)
  implicit none

  interface
    function f(x) result(y)
      real*8 :: x, y

    end function
  end interface 

  real*8, intent(in) :: a, b, eps
  integer, intent(in) :: nmax
  real*8, intent(out) :: root
  real*8 :: an, bn, xm
  integer :: i 

  xm = (b - a)/2.0d0

  if (abs(f(a)) <= eps) then 
    root = a 
    return 
  else if (abs(f(b)) <= eps) then
    root = b
    return
  end if 

  i = 0
  an = a 
  bn = b

  if (f(a)*f(b) < 0) then
    do while (abs(f(xm)) >= eps .and. i < nmax)
      xm = (bn - an)/2.0d0

      if (f(an)*f(xm) < 0) then
        bn = xm
      else if (f(xm)*f(bn) < 0) then
        an = xm 
      end if 

      i = i + 1
    end do
  end if

  print *, i

  root = xm
end subroutine
