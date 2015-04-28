program Aufgabe12
implicit none
	integer :: ziffern(24), i, k, b
	real :: u, uApprox
	
	b = 2
	u = 0.1
	uApprox = 0.
	!Ueber Ziffern iterieren
	do i = 0,23
		!Ueber Werte der Ziffern iterieren
		do k = 1,b
!			write(*,*) 'k=',k
!			write(*,*) 'real((b-k)/real(b**i))',real((b-k)/real(b**i))
			!...bis maximale Ziffer gefunden
			if((uApprox+real((b-k)/real(b**i))) <= u) then
				uApprox = uApprox+real((b-k)/real(b**i))
				ziffern(i+1) = b-k
				!Dann inneren loop abbrechen
				exit
			end if
		end do
		write(*,*) i+1,'te Ziffer von ũ ist',ziffern(i+1)
	end do
		
end program Aufgabe12
