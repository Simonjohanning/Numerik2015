program Aufgabe1_2
  implicit none
  double precision :: sinhDarstellung, prodDarstellung, x, teilProd, pi
  integer :: k
  write(*,*) 'Bitte Zahl eingeben und mit [ENTER] bestätigen'
  x = 0.04
  pi = 4.*atan(1.)
  teilProd = dble(1+(x**2/(pi)**2))
  do k=2,20,1
	teilProd = teilProd * dble(1+(x**2/(k*pi)**2))
    sinhDarstellung = sinh(x)
    write(*,*) 'Darstellung mittels sinh ergibt ',sinhDarstellung
    write(*,*) 'Darstellung mittels Produkt bis zum ',k,'-ten Glied ergibt ',dble(x*teilProd)
  end do
end program Aufgabe1_2
