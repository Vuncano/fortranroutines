      program funcminanalica
      implicit real (a-h,o-z)
      external func
      common /constantes/xpi,xlambda,xmc,G
      xpi=4*atan(1.0)
      xlambda=587.9
      xmc=5.6
      G=2.44/(xlambda**2)
c
c     Secante
      r=390
      u=410
      call secante(r,u,raizs)
      write(*,*)'Raiz, secante', raizs
c     Newton-Raphason
      f=390
      call newtonraphason(f,raizn)
      write(*,*) 'Raiz, Newton',raizn
c     Bissecao
      z=390
      g=410
      call bissecao(z,g,d)
      write(*,*) 'Raiz, bissecao',d
c
      stop
      end

c     Raiz
      subroutine bissecao(a,b,c)
      implicit real (a-h,o-z)
      c=(a+b)/2
      do while ((func(a)*func(b)).LT.-1e-8)
      if (func(a)*func(c).LT.0) then
      b=c
      else
      a=c
      endif
      enddo
c
      return
      end
c
      subroutine secante(x0,x1,rzs)
      implicit real (a-h,o-z)
      do while (abs(func(x1)).GT.1e-2)
      rzs=x1-(func(x1)/(func(x1)-func(x0))/(x1-x0))
      x1=rzs
      enddo
c
      return
      end
c
      subroutine newtonraphason(x,rzn)
      implicit real (a-h,o-z)
      do while (abs(func(x)).GT.1e-2)
      rzn=x-(func(x)/dr(x))
      x=rzn
      enddo
c
      return
      end
c
c     Funcao integral
      subroutine ad(y,fint)
      implicit real (a-h,o-z)
      common /constantes/xpi,xlambda,xmc,G
      xmin=0
      xmax=xlambda
      n=900
      altura=(xmax-xmin)/n
      fint=0
c
      do i=1,n
      x=xmin+altura*(i-1)
c
      xintg=x**2/sqrt(x**2+y**2)
c
      fint=altura*xintg+fint
      enddo
c
      return
      end
c
c     Funcao derivada
      real function dr(x)
      implicit real(a-h,o-z)
      h=1
      dr=(func(x+h)-func(x-h))/h
      return
      end
c
c     Funcao principal
      real function func(x)
      implicit real (a-h,o-z)
      common /constantes/xpi,xlambda,xmc,G
      call ad(x,t)
      func=x-xmc-(12/xpi**2)*G*x*t
      return
      end
