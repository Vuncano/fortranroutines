subroutine integral(f, xmin, xmax, n, result)
  implicit none

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y

    end function f
  end interface

  real*8, intent(in) :: xmin, xmax
  real*8, intent(out) :: result
  real*8 :: deltax, x 
  integer, intent(in) :: n
  integer :: i

  deltax = (xmax - xmin) / dfloat(n-1)

  do i = 1, n
    x = xmin + dfloat(i-1) * deltax
    result = result + deltax * f(x)
  end do
end subroutine integral
