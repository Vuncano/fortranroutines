program main

  implicit none

  external func

  real*8 :: x, y, min, max, delx, func
  integer :: i, nx

  open(unit = 1, file = "1-x.dat")
  open(unit = 2, file = "1-x2.dat")

  x = 0.0d0
  y = 0.0d0
  min = -10.d0
  max = 10.d0
  nx = 500 
  delx = (max - min)/dfloat(nx)

  do i = 1,nx
    x = min + ((i-1)*delx)
   
    if (x == 1.0d0) then
      exit
    end if

    y = func(x)
    print *, x, y
    write (1, *) x, y
  end do 

  do i = i+1, nx
    x = min + ((i-1)*delx)

    y = func(x)
    print *, x, y
    write (2, *) x, y
  end do

  close(1)

end program main

    real*8 function func(x)

    implicit none

    real*8 :: x, y

    func = 1/(1-x)
  end function func
