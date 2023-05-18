program plot
  implicit none

  external f1

  real*8 :: x, y, min, max, delx, f1
  integer :: nx, i

  open(unit = 1, file = 'func.dat')
  open(unit = 2, file = 'func2.dat')

  x = 0.0d0
  y = 0.0d0
  min = -10.0d0
  max = 10.0d0
  nx = 500
  delx = (max - min) / dfloat(nx)

  do i = 1, nx
    x = min +  ((i-1) * delx) 
    
    if (x == 1) then
      exit
    endif

    y = f1(x)
    
    write(1,*) x, y

  end do

  do i = i + 1, nx
     x = min +  ((i-1) * delx)

     y = f1(x)

     write(2,*) x, y

  end do
   
  close(1)
  close(2)

end program plot

real*8 function f1(x)
  real*8 :: x  
  
  f1 = 1 / (1-x)

end
