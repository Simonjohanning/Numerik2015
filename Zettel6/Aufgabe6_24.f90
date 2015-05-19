program Aufgabe6_24
implicit none
	!double in fortran: real (kind=8)
	!long double in fortan: real (kind=16)

	real (kind=8) :: koeffMatrix(16,16), solution(16)

	!i,j running variables, maxColumnEntry is the row with the highest entry in the current column
	integer :: i,j

	
	!fill matrix
	do i=1,16
		do j=1,16
			koeffMatrix(i,j) =  real(1/real(i+j-1))
			write(*,*) i,j, " is ", koeffMatrix(i,j)
		end do
	end do

	!for every column
	do i=1,16
		call pivotColumn(koeffMatrix, i, solution)
		call eliminate(koeffMatrix, i, solution)
	end do

	do j=1,16
		write(*,*) j,"-te Spalte ist: ",koeffMatrix(j,:)
	end do

	do j=1,16
		write(*,*) "Die",j,"-te Komponente der Lösung des GLS ist ",solution(j)
	end do

end program Aufgabe6_24


subroutine pivotColumn(koeffMatrix, i, solution)	
implicit none	
	real (kind=8) :: koeffMatrix(16,16), tempVector(16), solution(16), tempSolutionEntry

	!i,j running variables, maxColumnEntry is the row with the highest entry in the current column
	integer :: i,j,maxColumnEntry

  !find element with highest absolute value in this column
	maxColumnEntry = i
		do j=i,16
			if(abs(koeffMatrix(j,i)) > abs(koeffMatrix(maxColumnEntry,i))) then
				maxColumnEntry = j
			end if
		end do
		!switch current row with row with maximal element
		tempVector(:) = koeffMatrix(i,:)
		tempSolutionEntry = solution(i)
		koeffMatrix(i,:) = koeffMatrix(maxColumnEntry, :)
		solution(i) = solution(maxColumnEntry)
		koeffMatrix(maxColumnEntry, :) = tempVector(:)
		solution(maxColumnEntry) = tempSolutionEntry
end subroutine pivotColumn
		


subroutine eliminate(koeffMatrix, i, solution)
implicit none
	real (kind=8) :: koeffMatrix(16,16), lki, solution(16), valueToSubtract

	integer :: i, k, j

	!iterate through rows
	do k=i+1,16
!	write(*,*) i,k
		!determine elimination element
	!	write(*,*) "fooo", koeffMatrix(i, i), koeffMatrix(k, i)
		!write(*,*) "lki:",koeffMatrix(k, i)/koeffMatrix(i,i)," = ",koeffMatrix(k, i),"/",koeffMatrix(i, i)
		lki = koeffMatrix(k, i)/koeffMatrix(i, i)
		
		!recalculate koefficients
		!iterate through columns
		do j=i,16
			koeffMatrix(k, j) = koeffMatrix(k, j) - (lki * koeffMatrix(i,j))
			solution(k) = solution(k) - lki*solution(i)
		end do
	end do
end subroutine eliminate



