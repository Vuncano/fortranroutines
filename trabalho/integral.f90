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

real*8 function f(x,m)
  real*8 :: x, m

  f = (x**2) / sqrt(x**2 + m**2)

end function

function integral(xmin, xmax, n, m) result(sum)
  implicit none

  real*8 xmin, temp2,  xmax, sum, delx, temp, m, x
  integer n, i


  delx = (xmax - xmin) / n
  sum = 0.0

  do i = 1, n
    x = xmin + ((i-1) * delx)
    temp = x**2 / sqrt(m**2 +x**2)

    sum = sum + temp
  end do

  temp = xmin**2 / (sqrt(m**2 + xmin**2)) 
  temp2 = xmax**2 / (sqrt(m**2 + xmax**2))
  sum =  (delx / 2.0)*(temp + 2.0*sum + temp2)
end function integral
