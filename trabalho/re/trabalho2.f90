program trabalho
  implicit none

  ! external find_roots

  real*8 :: min, max, delta, rootf, m, root, find_roots, dumb
  integer :: i, n

  open(unit=10, file="graph.dat")

  min = 1.0d-8
  max = 2.0d0
  n = 1000
  delta = (max - min)/dfloat(n)

  do i = 1, n
     m = min + (i-1) * delta
     write(10, *) m, rootf(m)
  end do

  close(10)

  ! root = find_roots(0.25d0, 0.5d0)
  ! print *, root

  call newton_raphson(rootf, 0.5d0, 1.0d-9, i, root, dumb)

  write(*,*) "root = ", root
  write(*,*) "f(root) = ", rootf(root)
end program

function rootf(M) result(f)
  implicit none

  external integral

  real*8 :: M, f, int, Nc, Nf, M0, lambda, G, pi, integral

  Nc = 3
  Nf = 2
  M0 = 0.0056
  lambda = 0.5079
  g = 2.44/(lambda**2)

  pi = 4.0 * datan(1.0d0)

  f = M - M0 - ((4*G*Nc*Nf*M)/(2*pi**2))*integral(0.0d0, lambda, 10000, M)
end function

function integral(xmin, xmax, n, M) result(sum)
  implicit none

  external intf

  real*8 :: xmin, xmax, sum, delx, m, x, intf
  integer :: n, i

  delx = (xmax - xmin) / n
  sum = 0.0d0

    do i = 1, n
     x = xmin + (i-1)*delx
     sum = sum + intf(x, M)
  end do

  sum = (delx / 2.0d0)*(intf(xmin, M) + 2.0d0*sum + intf(xmax, M))
end function

function intf(x, M) result(y)
  implicit none

  real*8 :: x, m, y

  y = x**2 / sqrt(M**2 + x**2)
end function

include "newton-raphson.f90"
