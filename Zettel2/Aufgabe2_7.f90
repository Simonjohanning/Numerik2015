program Aufgabe2_8
  implicit none

  !variable declaration
  double precision :: first_result, second_result, third_result, evaluation_point

  evaluation_point = (1. + sqrt(2.))

  write(*,*) 'Evaluation für f1:'
  first_result = evaluation_point - 2.
  second_result = first_result**6.
  third_result = second_result * (-1.)
  write(*,*) 'f1(1+sqrt(2))=',third_result

  write(*,*) '------------------------'
  write(*,*) 'Evaluation für f2:'
  first_result = (2.*evaluation_point) - 5.
  second_result = first_result**3.
  third_result = second_result * (1.)
  write(*,*) 'f2(1+sqrt(2))=',third_result

  write(*,*) '------------------------'
  write(*,*) 'Evaluation für f3:'
  first_result = (70.*evaluation_point) + 29.
  second_result = first_result**(-1.)
  third_result = second_result * (-1.)
  write(*,*) 'f3(1+sqrt(2))=',third_result
  
end program Aufgabe2_8
