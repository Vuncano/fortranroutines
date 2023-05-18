program media
  implicit none

  real :: soma, mt 
  integer :: ntot, ns7, ni7, i, nal

  soma = 0.0
  ns7 = 0
  ni7 = 0

  print *, 'de o numero total de alunos'
  read (*,*) ntot

  do i = 1, ntot
    print *, 'de a media do aluno', i
    read (*,*) nal

      if (nal >= 7.0) then
        ns7 = ns7 +1
      else
        ni7 = ni7 + 1
      end if

    soma = soma + nal
  end do

  mt = soma / float(ntot)

  print *, 'Numero total de alunos: ', ntot
  print *, 'Alunos aprovados: ', ns7
  print *, 'Alunos reprovados: ', ni7
  print *, 'Media total da turma: ', mt

end program media
