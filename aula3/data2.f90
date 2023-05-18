program DataFile
  implicit none

  real(kind = 8) :: x, func, pi, min, max, delxn, func2, func3
  integer :: nx, lx

  pi = 4.d0 * datan(1.d0)

  open(unit = 1, file = 'func.dat')
  open(unit = 2, file = 'func2.dat')
  open(unit = 3, file = 'func3.dat')

  min = 0
  max = 6.d0
  nx = 50
  delxn = (max-min)/dfloat(nx-1)
  lx = 1

  do while (lx <= nx)

    x = min + dfloat(lx-1) * delxn
    func = sin(x)
    func2 = dsqrt(x)
    func3 = func * func2

    lx = lx + 1

!    print *, lx, x, delxn, func2

    write (1, *)x, func
    write (2, *)x, func2
    write (3, *)x, func3

  end do

  close(1)
  close(2)
  close(3)

end program DataFile
