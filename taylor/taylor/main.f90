program taylor
  implicit none
  
  external f
  external fact

  real :: f, h, x, min, max, delx, x0, t, fact, dfs, dfsi
  integer :: n, i

  min = -10.0
  max = 10.0
  n = 8
  delx = (max - min) / float(n)
  h = 2 * sqrt(EPSILON(1.0))

  x0 = 0
  dfs = (f(x0+h) - f(x0)) / h
  
  do i = 0, n

    dfs = (dfs+h - dfs) / h

    t = t + (dfs / fact(i)) * (x - x0)**i

    
    print *, i, dfs
  enddo


end program taylor

real function f(x0)
  implicit none

  real :: x0

  f = sin(x0)
end function

real function fact(i)
  integer, INTENT(IN) :: i

  integer :: n
  
  fact = 1.0

  do n = 2, i
    fact = fact * n
  end do
end function fact
