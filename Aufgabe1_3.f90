program Aufgabe1_3
  implicit none
  double precision :: rekursionsGlied, e
  integer :: k
  e = exp(1.0)
  rekursionsGlied = (e-1.)
  do k=0,31,1
    rekursionsGlied = (e - (k+1)*rekursionsGlied)
    write(*,*) 'Vorwärsrekursion für',k,'-tes Glied ergibt ',rekursionsGlied
  end do
  rekursionsGlied = 0.
  do k=0,31,1
    rekursionsGlied = dble((e - rekursionsGlied) / (62-k))
    write(*,*) 'Rückwärsrekursion für',k,'-tes Glied ergibt ',rekursionsGlied
  end do
end program Aufgabe1_3
