Module mMath 
  implicit none
  contains

  function SMatdot_real(iNoRows,ix,iy)
    !Tue October, 2014
    !Real inner product
    !IO
    !iNoRows -> number of rows in input vectors
    !iX      -> input vector
    !iY      -> input vector, which gets conjugated
    use mParallel
    implicit none
    real*8,intent(in),dimension(:) :: ix, iy
    integer,intent(in)             :: iNoRows
    real*8                         :: SMatdot_real
    integer                        :: i
    integer                        :: OldThreads
      SMatdot_real=0
      !$OMP PARALLEL IF(StartOpenMP(2,1)) PRIVATE(I) DEFAULT(SHARED)  REDUCTION(+:SMatdot_real)   
      !$OMP DO SCHEDULE(STATIC)
        do i=1,iNoRows
          SMatdot_real=SMatdot_real+ix(i)*iy(i)
        end do
      !$OMP END DO      
      !$OMP END PARALLEL    
      call ReturnOldThreadNum()
  end function SMatdot_real
End module mMath