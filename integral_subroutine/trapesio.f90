subroutine integralTrap(f, xmin, xmax, n ,resul)
  implicit none

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y

    end function f
  end interface

  real*8, intent(in) :: xmin, xmax
  real*8, intent(out) :: resul
  integer, intent(in) :: n
  integer :: i
  real*8 :: deltax, x, tempt

  deltax = (xmax - xmin) / dfloat(n-1)

  do i = 2, (n-1)
    x = xmin + dfloat(i-2)*deltax
    tempt = tempt + f(x)
  enddo

  resul = (deltax / 2) * (f(1.0d0) + 2*tempt + f(xmax))
end subroutine
