program fibonacci
  implicit none

  integer :: x, f, f1, f2, fib, divisor_teste
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

    do i2 = 2, sqrt(float(f))
      divisor_teste = i2

      if (f == (f/divisor_teste)*divisor_teste) then
        prime = "not prime"
      end if
      
    end do

    if(f == 1) then
      prime = "not prime"
    end if

    write(*,*) f, prime

    f1 = f2
    f2 = f

  end do

end program fibonacci
