program pi
  implicit none

  real*8 :: p

  p = 4.d0 * datan(1.d0)  !defines pi as 4 times arctg(1)

  write(*,*) p

end program pi
