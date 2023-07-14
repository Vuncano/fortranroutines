program trabalho
  implicit none

  external funcao

  real*8 :: nc, nf, m0,lambda, g, k, min 
  real*8 :: max, delta, m, pi, resultintegral, f, root, froot, funcao, y
  integer :: n, i

  open(unit=10, file="graph.dat")

  nc = 3
  nf = 2
  m0 = 0.0056
  lambda = 0.5079
  g = 2.44/(lambda**2)
  n = 10000

  pi = 4.0d0 * datan(1.0d0)

  min = 0.0d0
  max = 10.0d0
  delta = (max - min) / dfloat(n)

  do i = 1, n
    m = min + ((i-1) * delta)

    call integral(0.0d0, lambda, n, m, resultintegral)

    y = m - m0 - (((4*g*nc*nf*m) / (2 * pi**2)) * resultintegral)

    ! y = funcao(m)

    write(10, *) m, y

  end do

  close(10)

end program trabalho

real*8 function funcao(m)

  real*8, intent(in) :: m
  real*8 :: resultintegral, Nc, Nf, M0, lambda, g, pi

  Nc = 3
  Nf = 2
  M0 = 0.0056
  lambda = 0.5079
  g = 2.44/(lambda**2)

  pi = 4.d0 * datan(1.d0)  !defines pi as 4 times arctg(1)

  call integral(0.0d0, lambda, 100000, m, resultintegral)

  funcao = m - m0 - (((4*g*nc*nf*m) / (2 * pi**2)) * resultintegral)

end function

real*8 function f(x,m)
  real*8 :: x, m

  f = (x**2) / sqrt(x**2 + m**2)

end function

include "integral.f90"
