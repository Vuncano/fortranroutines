program DataFile
  implicit none

  real*8 :: x, func, pi, min, max, delx, func2, func3
  integer :: nx, lx

  pi = 4.d0 * datan(1.d0)

  open(unit = 25, file = 'function.dat')
  open(unit = 26, file = 'function3.dat')
  open(unit = 13, file = 'function2.dat')

  min = 0
  max = 6.d0
  nx = 50
  delx = (max-min)/dfloat(nx-1)

  do 1 lx = 1,nx
    x = min + dfloat(lx-1)*delx
    func = sin(x)
    func2 = dsqrt(x)
    func3 = func*func2

    print *, lx, x, delx, func2

    write (26,*)x, func3
    write (25,*)x, func
    write (13,*)x, func2

  1 enddo

  close(26)
  close(25)
  close(13)

  stop

end
