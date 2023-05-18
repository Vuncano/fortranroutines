program derivada
  implicit none

  external f1, df1

  real*8 :: x, f1, h, func, fa, funca, min, max, delx, min2, max2, delx2, df1, fnu, min3, max3, delx3
  integer :: n, i

  open(unit = 1, file = 'func.dat')
  open(unit = 2, file = 'funca.dat')
  open(unit = 3, file = 'funcnu.dat')

  min = -1.d0
  max = 3.d0
  min2 = 1.75d0
  max2 = 2.5d0
  min3 = 1.5d0
  max3 = 2.75d0
  n = 500
  delx = (max - min) / dfloat(n)
  delx2 =  (max2 - min2) / dfloat(n)
  delx3 = (max3 - min3) / dfloat(n)

  h = 2 * sqrt(epsilon(1.0d0))
 ! fnu = (f1(2.0d0 + h) - f1(2.0d0)) / h
 fnu = (f1(x + h) - f1(x)) / h

  fa = df1(2.d0)

! **função**
 do i = 1, n
  x = min + (i - 1) * delx

  func = f1(x)


  write (1,*) x, func
  end do

! **derivada analitica**
  do i = 1, n
    x = min + (i - 1) * delx
    !funca = fa * (x-2) + f1(2.0d0)
    funca = df1(x)
    write (2,*) x, funca
  end do
  
! **derivada numerica**
  do i = 1, n
    x = min + (i - 1) * delx
    !funca = fnu * (x-2) + f1(2.0d0)
    funca = (f1(x + h) - f1(x)) / h
    write (3,*) x, funca
    print *, x, funca
  end do

 close(1)
 close(2)

  call system ('gnuplot -p func.gnu')

end program

real*8 function f1(x)
  implicit none
  real*8 x

  f1 = x * sin(x**2) + 1

end

real*8 function df1(x)
  implicit none
  real*8 x

  df1 = sin(x**2) + 2 * x**2 * cos(x**2)

end
