subroutine newton_raphson(f, x1, tol, n, root, froot)
  implicit none

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y

    end function f
  end interface

  real*8, intent(in) :: x1, tol
  real*8, intent(out) :: root, froot
  real*8 :: h, x
  integer, intent(out) :: n

  x = x1
  
  h = 2 * sqrt(epsilon(1.0d0))
  
  do while (abs(f(x)) >= tol)
    x = x - (f(x) / ((f(x+h) - f(x)) / h))

    n = n+1
  end do

  root = x
  froot = f(x)
end subroutine newton_raphson
