program plots
  implicit none

  external f1

  real(kind = 8) :: x, y, min, max, delx, f1 
  integer :: nx, i

  open(unit = 1, file = '1-x.dat')
  open(unit = 2, file = '1-x2.dat')

  x = 0.0d0
  y = 0.0d0
  min = -10.d0
  max = 10.d0
  nx = 500 
  delx = (max - min)/dfloat(nx)

  do i = 1, nx 
    x = min + ((i-1) * delx) 

    if (x == 1.0d0) then
      exit
    end if

    y = f1(x)

  !  print *, x, y 
    write (1, *)x, y 

  end do

!  do i = i + 1, nx
 !   x = min + ((i-1) * delx)
!
 !   y = f1(x)
!
 !   print *, x, y
  !  write (2,*) x, y
 ! end do

  close(1)
  close(2)

  call system ('python plot.py')

end program plots

real(kind = 8) function f1(x)
  implicit none
  real(kind = 8) x

  f1 = 1/(1-x)

end function f1
