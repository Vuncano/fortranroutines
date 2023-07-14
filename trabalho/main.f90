program trabalho
  implicit none

  external f
  ! external funcao
  external integral


  real*8 :: Nc, Nf, M0, lambda, g, k,min, max, delta, y, m, pi, resultintegral, f, root, froot, funcao, integral
  integer :: n, i 

  open(unit=10, file="graph.dat")
  
  Nc = 3
  Nf = 2
  M0 = 0.0056
  lambda = 0.5079
  g = 2.44/(lambda**2)

  pi = 4.d0 * datan(1.d0)  !defines pi as 4 times arctg(1)

  min = 1.0d-8
  max = 10.0d0
  delta = (max - min)/dfloat(n)
  n = 1000


  do i = 1, n
    m = min + ((i-1) * delta) 
    resultintegral = integral(0.0d0, lambda, n, m) 
    print *, resultintegral

    
      y = m - m0 - (((4*g*nc*nf*m) / (2 * pi**2)) * resultintegral)
      ! y = funcao(m)

    write(10, *) m, y

  enddo

  close(10)

  ! call bs(funcao, 0.25d0, 0.5d0, 1.0d-9, i, root, froot)

  ! write(*,*) i, root, froot

  ! write(*,*) funcao(root)


end program trabalho

! real*8 function f(x,m)
!   real*8 :: x, m
!
!   f = (x**2) / sqrt(x**2 + m**2)
!
! end function

real*8 function funcao(m)

  real*8, intent(in) :: m
  real*8 :: resultintegral, Nc, Nf, M0, lambda, g, pi

  Nc = 3
  Nf = 2
  M0 = 0.0056
  lambda = 0.5079
  g = 2.44/(lambda**2)

  pi = 4.d0 * datan(1.d0)  !defines pi as 4 times arctg(1)

  ! call integral(0.0d0, lambda, 100000, m, resultintegral)

  funcao = m - m0 - (((4*g*nc*nf*m) / (2 * pi**2)) * resultintegral)

end function


function integral(xmin, xmax, n, m) result(sum)
  implicit none

  real*8 xmin, temp2,  xmax, sum, delx, temp, m, x
  integer n, i


  delx = (xmax - xmin) / n
  sum = 0.0

  do i = 1, n
    x = xmin + ((i-1) * delx)
    temp = x**2 / sqrt(m**2 +x**2)

    sum = sum + temp
  end do

  temp = xmin**2 / (sqrt(m**2 + xmin**2)) 
  temp2 = xmax**2 / (sqrt(m**2 + xmax**2))
  sum =  (delx / 2.0)*(temp + 2.0*sum + temp2)
end function integral

include "abs.f90"

! include "integral.f90"

