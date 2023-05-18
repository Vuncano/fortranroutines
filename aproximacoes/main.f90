program sinxx
  implicit none

  external f, fs

  real :: f, fs, x, max, min, delx, y
  integer :: i, n

  open(unit = 1, file = 'x.dat')
  open(unit = 2, file = 'sinx.dat')

  min = -0.5
  max = 0.5
  n = 500
  delx = (max - min) / n

  do i = 1, n
    x = min + ((i-1) * delx)

    y = f(x)

    write(1,*) x, y

  end do

  do i = 1, n
    x = min + ((i-1) * delx)
  
    y = fs(x)

    write (2,*) x, y
  end do

  close(1)
  close(2)

  call system('python plot.py')


end program

real function fs(x)
  real :: x

  fs = exp(x)

end

real function f(x)
  real :: x

  f = 1+x

end
