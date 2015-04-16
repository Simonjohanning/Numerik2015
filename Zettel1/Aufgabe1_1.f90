program Aufgabe1_1
  implicit none
  double precision :: sinhDarstellung, expDarstellung, e, x
  integer :: i
  e = exp(1.0)
  do i=1,20,1
    x = dble(10.0**(-i))
    sinhDarstellung = sinh(x)
    expDarstellung = 0.5*(exp(x) - exp(-x))
    write(*,*) 'Darstellung mittels sinh für            ',i,' ergibt ',sinhDarstellung
    write(*,*) 'Darstellung mittels exp-Darstellung für ',i,' ergibt ',expDarstellung
    write(*,*)
  end do
end program Aufgabe1_1
