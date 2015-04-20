program Aufgabe2_8
  implicit none

  !variable declaration
  integer :: x

  do x=1,19,2
	 call naive_exp(dble(x))
	 call naive_exp(dble(-x))
  end do



end program Aufgabe2_8


subroutine naive_exp(x)
  implicit none
  double precision :: partialsum, partialsum_previous, x_pow, x
  integer :: k, k_factorial
  logical :: result_change
  !setup variables
  x_pow = 1.
  result_change = .TRUE.
  partialsum_previous = 0.
	!set partial sum to the first element (so it's different)
  partialsum = 1.
  	!start with the second element (since first is impicitely computed already)
  k = 1
  k_factorial = 1
  !Calculate exp naively
  do while(result_change)
   	partialsum_previous = partialsum
	x_pow = dble(x) * x_pow
	k_factorial = k*k_factorial
	partialsum = partialsum + (x_pow / dble(k_factorial))
	write(*,*) 'Partialsumme ',partialsum_previous,' + ',(x_pow / dble(k_factorial))
	write(*,*) ' ergibt neue Partialsumme ',partialsum
!	write(*,*) 'partialsum = ',partialsum,' partialsum_previous = ',partialsum_previous
!	write(*,*) 'partialsum == partialsum_previous = ',partialsum == partialsum_previous
	result_change = .NOT. (partialsum == partialsum_previous)	
	if(ISNAN(partialsum)) result_change = .FALSE.
	k = k+1
	!if(k > 10) result_change = .FALSE.
	!write(*,*) 'x_pow ist ',x_pow
  end do	
  write(*,*) 'Programm hat nach ',k,' Iterationsschritten mit dem Ergebnis ',partialsum,' für x = ',x,'terminiert.'
  write(*,*) 'Eingebaute e-Funktion liefert ',exp(dble(x))
 ! return
end subroutine naive_exp




