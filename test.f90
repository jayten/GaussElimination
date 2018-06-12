program GETest
  use GE, only: GEsolve
  implicit none

  real, dimension(3,3) :: A
  real, dimension(3)   :: B
  real, dimension(3)   :: X
  integer              :: n=3

  A(1,1) = 3 ; A(1,2) = 2 ; A(1,3) = -4;
  A(2,1) = 2 ; A(2,2) = 3 ; A(2,3) =  3;
  A(3,1) = 5 ; A(3,2) =-3 ; A(3,3) =  1;

  B(1) =  3
  B(2) = 15
  B(3) = 14

  call  GEsolve(A,x,B,n)
  print*, x
  x = (3, 1, 2)
  
end program GETest
