program taylor
  implicit none

  real :: x, min, max, delx, h, y, de
  integer :: i, n

  open(unit = 1, file = 'numerical_taylor.dat')
  open(unit = 2, file = 'e^x.dat')
  open(unit = 3, file = 'analitical_taylor.dat')

  min = -0.5
  max = 0.5
  n = 500
  delx = (max - min) / n
  h = 2.0 * sqrt(epsilon(1.0))

  de = (exp(1.0+h) - exp(1.0)) / h 

  do i = 1, n
    x = min + (i-1) * delx

    y = exp(0.0) + de**0 * x

    write (1,*) x, y
  end do

  do i = 1, n
    x = min +(i-1) * delx

    y = exp(x)

    write (2,*) x, y
  end do

  do i = 1, n
    x = min + (i-1) * delx

    y = exp(0.0) + exp(0.0) * x

    write (3,*) x, y
  end do

  close(1)
  close(2)
  close(3)

  call system ('gnuplot -p plot.gnu')
 
end program
