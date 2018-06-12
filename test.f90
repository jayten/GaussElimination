program GETest
  use GE, only: GEsolve
  implicit none

  call test3orderMatrix()
  call test4orderMatrix()

  contains

    subroutine test3orderMatrix()
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
      x=ceiling(x)
      !x = (3,1,2)
      if(int(x(1)) == 3 .and. int(x(2)) == 1 .and. int(x(3)) == 2) then
        print*, "First test successful"
      else
        print*, "Failed First test"
      end if
    end subroutine test3orderMatrix

    subroutine test4orderMatrix()
      implicit none

      real, dimension(4,4) :: A
      real, dimension(4)   :: B
      real, dimension(4)   :: X
      integer              :: n=4

      A(1,1) =.02; A(1,2) =.01; A(1,3) =  0; A(1,4) = 0
      A(2,1) = 1 ; A(2,2) = 2 ; A(2,3) =  1; A(2,4) = 0
      A(3,1) = 0 ; A(3,2) = 1 ; A(3,3) =  2; A(3,4) = 1
      A(4,1) = 0 ; A(4,2) = 0 ; A(4,3) =100; A(3,4) = 200

      B(1) = 0.02
      B(2) = 1
      B(3) = 4
      B(3) = 800

      call  GEsolve(A,x,B,n)
      !x = (1,0,0,4)
      if(x(1) == 1 .and. x(2) ==0 .and. x(3) == 0 .and. x(4) == 4) then
        print*, "Second test successful"
      else
        print*, "Failed Second test"
      end if
    end subroutine test4orderMatrix

  
end program GETest
