program trabalho
  implicit none

  external find_roots

  real*8 :: min, max, delta, rootf, m, root, find_roots, dumb
  integer :: i, n

  open(unit=10, file="graph.dat")

  min = 1.0d-8
  max = 1.0d0
  n = 1000
  delta = (max - min)/dfloat(n)

  do i = 1, n
     m = min + (i-1) * delta
     write(10, *) m, rootf(m)
  end do

  close(10)

  call newton_raphson(rootf, 0.5d0, 1.0d-8, i, root, dumb)
  print *, root
  print *, rootf(root)

end program

subroutine newton_raphson(f, x1, tol, n, root, froot)
  implicit none

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y

    end function f
  end interface

  real*8, intent(in) :: x1, tol
  real*8, intent(out) :: root, froot
  real*8 :: h, x
  integer, intent(out) :: n

  x = x1

  h = 2 * sqrt(epsilon(1.0d0))

  do while (abs(f(x)) >= tol)
    x = x - (f(x) / ((f(x+h) - f(x)) / h))

    n = n+1
  end do

  root = x
  froot = f(x)
end subroutine newton_raphson

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
