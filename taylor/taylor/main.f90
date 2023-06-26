program taylor
  implicit none
  
  external freal
  external fact

  real*8 :: x, x0, freal, derf, taylor_term, min, max, deltax, fact, y, taylor_plot
  integer :: i, i2, n, npoints, derf_ordem

  ! open(unit = 10, file = '0taylor.dat') 
  ! open(unit = 11, file = '1taylor.dat') 
  ! open(unit = 12, file = '2taylor.dat') 
  ! open(unit = 13, file = '3taylor.dat') 
  ! open(unit = 14, file = '4taylor.dat') 
  ! open(unit = 15, file = '5taylor.dat') 
  ! open(unit = 16, file = '6taylor.dat') 
  ! open(unit = 17, file = '7taylor.dat') 
  ! open(unit = 18, file = '8taylor.dat') 
  ! open(unit = 19, file = '9taylor.dat') 
  open(unit = 20, file = 'sin.dat') 

  n = 5
  x0 = 0.0d0
  
  min = -10.0d0
  max = 10.0d0
  npoints = 1000
  deltax = (max - min) / dfloat(npoints)

  ! do i = 0, n
  !   derf_ordem = i
  !   call nderivative(freal, x0, derf_ordem, derf)
  !
  !   taylor_term = taylor_term + (derf / fact(i)) * (x - x0)**i
  !
  !   if (derf .ne. 0)then
  !     do i2 = 1, npoints
  !       x = min + ((dfloat(i2) - 1) * deltax)
  !
  !       taylor_plot = taylor_term
  !
  !       write(10+i, *) x, taylor_plot
  !     end do
  !   end if
  !
  ! enddo


  do i = 1, n
    call plot_taylor(freal, x0, min, max, npoints, i)
  end do

  x = 0.0d0
  y = 0.0d0
  do i = 1, npoints
    x = min + ((dfloat(i)-1) * deltax)

    y = freal(x)

    write(20, *) x, y
  enddo

  ! close(10)
  close(11)
  ! close(12)
  ! close(13)
  ! close(14)
  ! close(15)
  ! close(16)
  ! close(17)
  ! close(18)
  ! close(19)
  close(20)

  call system("python plot.py")

end program taylor

real*8 function freal(x0)
  implicit none

  real*8 :: x0

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

subroutine taylor_serie(f, x, x0, n, taylor)
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
  real*8 :: derf, fact
  integer :: i

  taylor = 0.0d0
  do i = 0, n
    call nderivative(f, x0, i, derf)

    taylor = taylor + (derf / fact(i)) * (x - x0)**i
  end do
end subroutine taylor_serie

subroutine plot_taylor(f, x0, min, max, nx, n)
  implicit none

  external integer_to_string

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y
    end function f
  end interface

  character(len=2) :: integer_to_string
  real*8, intent(in) :: x0, min, max
  integer, intent(in) :: nx, n
  real*8 :: delx, x, y
  integer :: i

  delx = (max - min)/dfloat(nx)

  open(unit = 1, file="taylor"//trim(integer_to_string(n))//".dat")

  do i = 1, nx
    x = min + dfloat(i-1)*delx
    call taylor_serie(f, x, x0, n, y)

    write(1,*) x, y
  end do

  close(1)
end subroutine plot_taylor

function integer_to_string(i) result(a)
  implicit none

  integer :: i
  character(len=2) :: a
  character(len=8) :: fmt

  fmt = '(I2.2)'

  write(a,fmt) i
end function integer_to_string

include "der.f90" 
