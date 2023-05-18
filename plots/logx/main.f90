program logx
  implicit none
 
  real(kind = 8) :: x, func, min, max, lx
  integer :: nx

  open(unit = 1, file = 'logx.dat')

  min = 0.d0
  max = 20.d0
  nx = 21
  lx = 1

  do while (lx <= nx)

    x = min + (lx-1)
    func = log(x)

    lx = lx + 0.25

!    print *, x, func

    write (1, *) x, func

  end do

  close(1)

  call system ('python plot.py')

end program logx
