program test
  real*8 :: x, n, sr

  n = epsilon(x)
  sr = 2 * sqrt(epsilon(x))

  print *, "n", n
  print *, "2 * sqrt", sr

end program
