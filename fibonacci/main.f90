program fibonacci
  implicit none

  ! external f
  
  integer :: x, f, f1, f2, fib
  integer :: i, n

  f = 0
  f1 = 0
  f2 = 1

  n = 20

  write(*,*)  f1
  write(*,*)  f2
  do i = 1, n
    f = f1 + f2

    write(*,*) f1, "+", f2, "= ", f

    f1 = f2
    f2 = f

  end do

end program fibonacci

! real function f(x)
!   real :: x, f1, f2
!   integer :: n, i
!
!   n = 5
!
!   f1 = 0
!   f2 = 1
!
!   do i = 1, n
!     f = f1 + f2
!
!     f1 = f2
!     f2 = f
!   end do
! end function
