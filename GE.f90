module GE
  implicit none


  private
  public :: GEsolve

  contains
    subroutine GEsolve(CoeffMatrix,Solution,RHS, N)
      implicit none
      real, dimension(:,:), intent(in)  :: CoeffMatrix
      real, dimension(:  ), intent(out) :: Solution
      real, dimension(:  ), intent(in)  :: RHS
      integer             , intent(inout):: N

      real, dimension(N) :: X
      real, dimension(N) :: B
      real, dimension(N, N) :: A

      A = CoeffMatrix
      B = RHS
      call ForwardElimWithPivoting(A,B,N)
      call BackSubsituation(A,X,B,N)
      Solution = X
    end subroutine GEsolve


    subroutine ForwardElimWithPivoting(A,B,n)
      implicit none
      real, dimension(:,:), intent(inout) :: A
      real, dimension(:  ), intent(inout) :: B
      integer             , intent(inout) :: n

      integer :: j,k
      real    :: ratio
      integer :: Current_loc
      integer :: Max_loc

      do k = 1,n-1
        !partial pivoting
        Current_loc = k
        Max_loc = k
        do j =  k+1,n
          if(abs(A(Max_loc,k))< A(j,k)) then
            Max_loc = j
          end if
        end do

        if(Current_loc /= Max_loc) then
          call SwapRow(A, B, n, Current_loc, Max_loc)
        end if

        ! foward elimination
        if(A(k,k)==0.0) stop "Zero value at pivot"
        do j = k+1,n
          if(a(j,k) /= 0.0) then
            ratio = A(j,k)/A(k,k)
            A(j,k) = 0.0
            B(j) = B(j) - ratio * B(k)
            A(j,k+1:) = A(j,K+1:) - ratio * A(k,k+1:)
          end if
        end do
      end do
    end subroutine ForwardElimWithPivoting


    subroutine BackSubsituation(A,X,B,n)
      implicit none
      real, dimension(:,:), intent(in) :: A
      real, dimension(:  ), intent(in) :: B
      real, dimension(:  ), intent(inout):: X
      integer             , intent(inout):: n

      integer :: j,k

      X(n) = B(n)/A(n,n)

      do j = n-1,1,-1
        X(j) = B(j)
        do k = j+1,n
          X(j) = X(j) - A(j,k)*X(k)
        end do
        X(j) = X(j) / A(j,j)
      end do
    end subroutine BackSubsituation


    subroutine SwapRow(A, B, n, Row1, Row2)
      implicit none
      real, dimension(:,:), intent(inout) :: A
      real, dimension(:  ), intent(inout) :: B
      integer             , intent(inout):: n
      integer, intent(in) :: Row1
      integer, intent(in) :: Row2

      ! temp variables required for swaping
      real, dimension(n) :: U
      real :: R

      !swap elements of B
      R = B(Row1)
      B(Row1) = B(Row2)
      B(Row2) = R

      !swap rows of A
      U = A(Row1,:)
      A(Row1,:) = A(Row2,:)
      A(Row2,:) = U
    end subroutine SwapRow

end module GE
