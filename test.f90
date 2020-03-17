program test
  character*100 filename
  integer, parameter :: msize=6, nsize=5
  INTEGER          LDA, LDU, LDVT
  PARAMETER        ( LDA = msize, LDU = msize, LDVT = nsize )
  INTEGER          LWMAX
  PARAMETER        ( LWMAX = 1000)
  INTEGER  INFO, LWORK
  DOUBLE PRECISION A( LDA, nsize ), U( LDU, msize ), VT( LDVT, nsize), S( nsize ),WORK( LWMAX ),C(msize,nsize),D(LDA, nsize)

  integer::i,j
  integer::sigK

  EXTERNAL DGESVD
  EXTERNAL PRINT_MATRIX
  INTRINSIC INT, MIN

 WRITE(*,*)'DGESVD Example Program Results'

DATA   A/&
       8.79, 6.11,-9.15, 9.57,-3.49, 9.84,&
       9.93, 6.91,-7.93, 1.64, 4.02, 0.15,&
       9.83, 5.04, 4.86, 8.83, 9.80,-8.99,&
       5.45,-0.27, 4.85, 0.74,10.00,-6.02,&
       3.16, 7.98, 3.01, 5.80, 4.27,-5.31&
       /

LWORK = -1
CALL DGESVD( 'All', 'All', msize, nsize, A, LDA, S, U, LDU, VT, LDVT,WORK, LWORK, INFO )
LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
CALL DGESVD( 'All', 'All', msize, nsize, A, LDA, S, U, LDU, VT, LDVT,WORK, LWORK, INFO )

IF( INFO.GT.0 ) THEN
    WRITE(*,*)'The algorithm computing SVD failed to converge.'
    STOP
 END IF
!  C=0.
!  do i=1,nsize
!     C(i,i)=S(i)
!   end do
!  D=matmul(matmul(U,C),VT) 
CALL PRINT_MATRIX( 'Original matrix', msize, nsize, A, msize)
CALL PRINT_MATRIX( 'Singular values', 1, nsize, S, 1 )
CALL PRINT_MATRIX( 'Left singular vectors (stored columnwise)',msize, nsize, U, LDU )
CALL PRINT_MATRIX( 'Right singular vectors (stored rowwise)', nsize, nsize, VT, LDVT )
!CALL PRINT_MATRIX( 'New matrix', msize, nsize, D, msize)
end program test

SUBROUTINE PRINT_MATRIX( DESC, M, N, A, LDA )
    CHARACTER*(*)    DESC
    INTEGER          M, N, LDA
    DOUBLE PRECISION A( LDA, * )
    INTEGER          I, J
    WRITE(*,*)
    WRITE(*,*) DESC
    DO I = 1, M
       WRITE(*,9998) ( A( I, J ), J = 1, N )
    END DO
9998 FORMAT( 11(:,1X,F6.2) )
    RETURN
  end subroutine PRINT_MATRIX