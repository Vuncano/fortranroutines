program DataFile
  implicit none

  real*8 :: x, func, pi, min, max, delxn, func2, func3, nx, ix, delx

  pi = 4.d0 * datan(1.d0)  !defines pi as 4 times arctg(1)

  !! create data files
  open(unit=25, file = 'function.dat')
  open(unit=26, file = 'function3.dat')
  open(unit=13, file = 'function2.dat')

  min = 0
  max = 6.d0 
  nx = 50
  delxn = (max-min)/(nx-1)

  do 1 ix = 1, nx
  x = min + (ix-1) * delx
  func = sin(x)
  func2 = dsqrt(x)
  func3 = func * func2

  write (*,*) ix, x, delx, func2

  !! to write to a file, write (unit, *)

  write (26, *)x, func3 
  write (25, *)x, func 
  write (13, *)x, func2 

  1 enddo

  close(26)
  close(25)
  close(13)

  stop

end program DataFile
