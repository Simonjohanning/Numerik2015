program Aufgabe2_8
  implicit none

  !variable declaration
  double precision :: partialsum, partialsum_previous, x_pow, summand_previous
  integer :: k, k_factorial,x
  logical :: result_change

  do x=-19,19,2
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
			result_change = .NOT.(partialsum == partialsum_previous)	
			k = k+1
		end do	
		write(*,*) 'Programm hat nach ',k,' Iterationsschritten mit dem Ergebnis ',partialsum,' für x = ',x,'terminiert.'
		write(*,*) 'Eingebaute e-Funktion liefert ',exp(dble(x))
  end do

  write(*,*) '------------------'
  write(*,*) 'Verhalten mittels unserer Lösung:'
  write(*,*) '------------------'

  do x=-19,19,2
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
		write(*,*) 'Partialsumme ',partialsum_previous,' + ',(summand_previous * (dble(x) / dble(k)))
		write(*,*) ' ergibt neue Partialsumme ',partialsum
	!	write(*,*) 'partialsum = ',partialsum,' partialsum_previous = ',partialsum_previous
	!	write(*,*) 'partialsum == partialsum_previous = ',partialsum == partialsum_previous
		result_change = .NOT.(partialsum == partialsum_previous)	
		summand_previous = summand_previous * (dble(x) / dble(k))
		k = k+1
	  end do
	  write(*,*) 'Programm hat nach ',k,' Iterationsschritten mit dem Ergebnis ',partialsum,' für x = ',x,'terminiert.'
	  write(*,*) 'Eingebaute e-Funktion liefert ',exp(dble(x))
  end do

end program Aufgabe2_8
