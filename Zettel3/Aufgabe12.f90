program Aufgabe12
implicit none
	integer :: ziffern(24), i, k, b
	real :: u, uApprox
	
	b = 2
	u = 0.1
	uApprox = 0.
	do i = 0,23
		do k = 1,b
!			write(*,*) 'k=',k
!			write(*,*) 'real((b-k)/real(b**i))',real((b-k)/real(b**i))
			if((uApprox+real((b-k)/real(b**i))) <= u) then
				uApprox = uApprox+real((b-k)/real(b**i))
				ziffern(i+1) = b-k
				exit
			end if
		end do
		write(*,*) i+1,'te Ziffer von ũ ist',ziffern(i+1)
	end do
		
end program Aufgabe12
