program tayloranalitico
  implicit none

  external f 

  real*8 :: x, x0, xmin, xmax, delx, taylor, f
  integer :: i, n
  
  open(unit = 10, file = "taylor01.dat")
  open(unit = 30, file = "taylor03.dat")
  open(unit = 50, file = "taylor05.dat")
  open(unit = 70, file = "taylor07.dat")
  open(unit = 90, file = "sin.dat")

  
  x0 = 0.0d0
  
  xmin = -10.0d0
  xmax = 10.0d0
  n = 1000
  delx = (xmax - xmin) / dfloat(n)

  do i = 1, n
    x = xmin + ((i-1) * delx) 

    taylor = x

    write(10, *) x, taylor
  end do

  i=0
  x=0
  taylor=0
  do i = 1, n
    x = xmin + ((i-1) * delx) 

    taylor = x - (x**3) / (3*2)

    write(30, *) x, taylor
  end do

  i=0
  x=0
  taylor=0
  do i = 1, n
    x = xmin + ((i-1) * delx) 

    taylor = x - ((x**3) / (3*2)) + ((x**5) / (5*4*3*2))

    write(50, *) x, taylor
  end do

  i=0
  x=0
  taylor=0
  do i = 1, n
    x = xmin + ((i-1) * delx) 

    taylor = x - ((x**3) / (3*2)) + ((x**5) / (5*4*3*2)) + ((x**7) / (7*6*5*4*3*2))

    write(70, *) x, taylor
  end do
  
  i=0
  x=0
  taylor=0
  do i = 1, n
    x = xmin + ((i-1) * delx) 

    taylor = f(x)

    write(90, *) x, taylor
  end do

  close(10)
  close(30)
  close(50)
  close(70)
  close(90)

  call system("python plot.py")


end program tayloranalitico

real*8 function f(x)
  real*8 x

  f = sin(x)

end function
