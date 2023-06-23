program taylor
  implicit none
  
  external freal
  external fact

  real*8 :: x, x0, freal, derf, taylor_term, min, max, deltax, fact
  integer :: i, i2, n, npoints

  n = 7
  x0 = 0.0d0
  
  min = -10
  max = 10
  npoints = 1000
  deltax = (max - min) / dfloat(npoints)

  do i = 0, n
    call nderivative(freal, x0, i, derf)

    do i2 = 1, npoints
      x = min + ((i2 - 1) * deltax)

      taylor_term = (derf / fact(i)) * (x - x0)**i
    end do

  enddo


end program taylor

real*8 function freal(x0)
  implicit none

  real :: x0

  freal = sin(x0)
end function

real*8 function fact(i)
  integer, INTENT(IN) :: i

  integer :: n
  
  fact = 1.0

  if (i == 0) then
    fact = 1
    return
  end if

  do n = 2, i
    fact = fact * n
  end do
end function fact

include "der.f90" 
