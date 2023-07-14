subroutine integral(xmin, xmax, n, m, result)
  implicit none

  external f

  ! interface
  !   function f(x,m) result(y)
  !     implicit none
  !
  !     real*8 :: x, y, m
  !
  !   end function f
  ! end interface

  real*8, intent(in) :: xmin, xmax, m
  real*8, intent(out) :: result
  real*8 :: deltax, x, f 
  integer, intent(in) :: n
  integer :: i

  deltax = (xmax - xmin) / dfloat(n-1)

  do i = 1, n
    x = xmin + dfloat(i-1) * deltax
    result = result + deltax * f(x, m)
  end do
end subroutine integral

