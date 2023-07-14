subroutine bs(f, a, b, tol, n, root, froot)
  implicit none

  interface
    function f(x) result(y)
      implicit none

      real*8 :: x, y

    end function f
  end interface

  real*8, intent(in) :: a, b, tol
  real*8, intent(out) :: root, froot
  integer, intent(out) :: n
  integer :: i
  real*8 :: c, ai, bi

  ai = a
  bi = b

  do while (abs(f(c)) >= tol)
   c = (ai+bi) / 2 

   if (f(a) * f(c) < 0) then
     bi = c
     
   else 
     ai = c
    end if

    i = i+1
  end do

  root = c
  froot = f(c)
  n = i

end subroutine
