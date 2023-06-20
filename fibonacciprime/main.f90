program fibonacci
  implicit none

  integer :: x, f, f1, f2, fib
  integer :: i, n, i2
  character :: prime*9
  
  f = 0
  f1 = 0
  f2 = 1

  n = 20

  write(*,*)  f1
  write(*,*)  f2
  do i = 1, n
    f = f1 + f2

    prime = "prime"

    do i2 = 1, n
      if (f.eq.(f/2)*2) then
        prime = "not prime"
      end if
    end do

    write(*,*) f, prime

    f1 = f2
    f2 = f

  end do

end program fibonacci
