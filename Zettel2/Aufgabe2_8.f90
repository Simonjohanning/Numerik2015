program Aufgabe2_8
  implicit none
  !variable declaration
  integer :: x
  write(*,*) '------------------'
  write(*,*) 'Verhalten mittels unserer Lösung:'
  write(*,*) '------------------'
  do x=1,19,2
	  call improved(dble(x))
	  call improved(dble(-x))
  end do
end program Aufgabe2_8

subroutine improved(x)
  implicit none
  double precision :: partialsum, partialsum_previous, summand_previous, x
  integer :: k
  logical :: result_change
  !setup variables
  result_change = .TRUE.
  partialsum_previous = 0.
 	!set partial sum to the first element (so it's different)
  partialsum = 1.
  	!start with the second element (since first is impicitely computed already)
  k = 1
  summand_previous = 1.
  do while(result_change)
	partialsum_previous = partialsum
	partialsum = partialsum_previous + (summand_previous * (dble(x) / dble(k)))
!	write(*,*) 'Partialsumme ',partialsum_previous,' + ',(summand_previous * (dble(x) / dble(k)))
!	write(*,*) ' ergibt neue Partialsumme ',partialsum
!	write(*,*) 'partialsum = ',partialsum,' partialsum_previous = ',partialsum_previous
!	write(*,*) 'partialsum == partialsum_previous = ',partialsum == partialsum_previous
	result_change = .NOT. (partialsum == partialsum_previous)	
	summand_previous = summand_previous * (dble(x) / dble(k))
	k = k+1
  end do
  write(*,*) 'Programm hat nach ',k,' Iterationsschritten mit dem Ergebnis ',partialsum,' für x = ',x,'terminiert.'
!  write(*,*) 'Eingebaute e-Funktion liefert ',exp(dble(x))
  write(*,'(A)') 'Eingebaute e-Funktion liefert '
	write(*,'(E12.11)') exp(dble(x))
	write(*,*) 'Der relative Fehler beträgt:',((partialsum - exp(dble(x)))/exp(dble(x)))
	write(*,*)
!10 format (2(A100, E16.15))
end subroutine improved
