program grandemin
      implicit none
      
      external y

      real*8 pi, lambda, Nc, Nf, Z, G, a, b, c, y, M, fy, root
      real*8 fun, xmax, xmin, tempR, delx, x
      integer n, i

      open(unit=5, file="grendemin.dat")
      

      pi= 4*datan(1.d0) 
      lambda= 0.5079d0
      Nc= 3.0d0
      Nf= 2.0d0
      Z= 0.056d0
      G= 2.44/(lambda**2)
      a= 2
      b= 0
      xmin= 0
      xmax= 1
      n= 1000
      delx= (xmax-xmin)/dfloat(n)
       
      do i=1,n
         M = xmin + ((i-1)* delx)
         tempR= 0

        fy= y(M) 
        write(5,*) M, fy
      end do
      close(5)
  
      call bs(y, 0.25d0, 0.50d0, 1.0d-8, root, 10000)
      print *, root, y(root)
       
end program 

function y(M) result(fy)
  implicit none
  external integral
  
  real *8 :: M, fy, G, lambda, Nc, Nf, tempR, pi, Z

  pi= 4*datan(1.d0) 
      lambda= 0.5079d0
      Nc= 3.0d0
      Nf= 2.0d0
      Z= 0.056d0
      G= 2.44/(lambda**2)

  tempr = 0.0 
  ! callc integral(0.0d0, lambda, 1000, M, tempR)
  tempR = integral(0.0d0, lambda, 1000, m)
  fy= M-Z-(((4*G*Nc*Nf*M)/(2*pi**2))*tempR)

end function
    
    function integral(xmin, xmax, n, m) result(sum)
        implicit none

        real*8 xmin, xmax, sum, delx, temp, m
        integer n, i


        delx = (xmax - xmin) / n
        sum = 0.0

        do i = 1, n
        temp = x**2 / sqrt(m**2 +x**2)
            sum = sum + f(xmin + (i - 1) * delx)
        end do
        sum =  (delx / 2.0)*(f(xmin) + 2.0*sum + f(xmax))
    end function calculate_integral_trapezoid

real*8 function F(x, M)
    implicit none
    real*8 x, M
    F= x**2/sqrt(x**2 + M**2)
  
end function

include "abs.f90"
