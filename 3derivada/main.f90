program df
  implicit none

  external f1

  real*8 :: f1, h, x, df1, df2, df3, min, max, delx
  integer :: n, i

  open(unit = 1, file = 'df1.dat')
  open(unit = 2, file = 'df2.dat')
  open(unit = 3, file = 'df3.dat')

  min = -5.0d0
  max = 5.0d0
  n = 100
  delx = (max - min) / dfloat(n)
  h = 2 * sqrt(epsilon(1.0d0))

  do i = 1, n
    x = min + (i-1) * delx

    df1 = (f1(x+h) - f1(x)) / h

    write (1,*) x, df1

  end do

  do i = 1, n
    x = min + (i-1) * delx

    df2 = (f1(x) + f1(x-h)) / h 

    write (2,*) x, df2

  end do

  do i = 1, n
    x = min + (i-1) * delx

    df3 = (f1(x+h) - f1(x-h)) / 2*h

    write(3,*) x, df3

  end do

  close(1)
  close(2)
  close(3)
  
  call system('gnuplot -p plot.gnu')

end program

real*8 function f1(x)
  implicit none

  real*8 :: x

  f1 = exp(x) - x - 2

end function
