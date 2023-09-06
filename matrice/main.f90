program matrice
  implicit none

  integer :: i, j, k, n
  real*8 :: x(n), b(3), c, a(3, 3)

  parameter (n = 3)

  a(1, 1) = 3.0d0; a(1, 2) = 2.0d0; a(1, 3) = 4.0d0
  a(2, 1) = 2.0d0; a(2, 2) = 3.0d0; a(3, 3) = 2.0d0
  a(3, 1) = 1.0d0; a(3, 2) = 1.0d0; a(3, 3) = 2.0d0

  b(1) = 4.0d0; b(2) = 2.0d0; b(3) = 3.0d0

  ! the follolwing do chain makes the "foward manipulation"
  do k = 1, n-1
    do i = k+1, n

    c = a(i, k) / a(k, k)
    a(i, k) = 0.0d0

    b(i) = b(i) 
  
  
  

end program matrice
