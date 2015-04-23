program Aufgabe2_6
	double precision :: eps
	eps = 1.
	do while((1.+eps) > 1.)
		eps = 0.5*(eps)
	end do
	eps=2.*eps
	write(*,*) 'Berechnetes epsilon (auf Dell studio 17/1749 mit ubuntu 14.04 LTS, 32-bit):',eps
end program Aufgabe2_6
