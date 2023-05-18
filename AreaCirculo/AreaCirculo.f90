program circle
  implicit none

  real :: pi
  parameter (pi = 3.14159)
  real :: area
  real :: r

  print *, "give the circle radius"
  read (*,*) r

  area = pi*r*r

  print *, "area = ", area

end program circle
