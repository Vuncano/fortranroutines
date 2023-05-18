program newton_raphson
  implicit none

  external f1
  external df1

  real*8 :: x1, f1, df1, h, x
  integer :: n, n1

  x1 = -8.0d0
  x = -8.0d0
  n = 0
  n1 = 0
  h = 2 * sqrt(epsilon(1.0d0))

  do while(abs(f1(x1)) >= 10e-10)
    
    x1 = x1 - (f1(x1) / df1(x1))

    n = n+1

  enddo

  print *, '========== ANALITICA ============'
  print *, 'root = ', x1, 'f(root) = ', f1(x1), 'atempts = ', n

  do while(abs(f1(x)) >= 10e-10)

    x = x - (f1(x) / ((f1(x+h) - f1(x))/h))

    n1 = n1+1

  end do

  print *, '========== NUMERICA =============='
  print *, 'root = ', x, 'f(root) = ', f1(x), 'atempts = ', n1

end program newton_raphson

real*8 function f1(x)
  implicit none

  real*8 :: x

  !f1 = exp(x) - x - 2

  f1 = x**2 - 4

end function

real*8 function df1(x)
  implicit none

  real*8 :: x

  !df1 = exp(x) - 1

  df1 = 2*x

end function
