program test
  implicit none

  !variable declaration
  

  do x=1,19,2
	 call naive_exp(dble(x))
  end do

end program test

  subroutine naive_exp(x)
  	double precision, intent(in) :: x
  end subroutine naive_exp


