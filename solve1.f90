!!This is routine for SVD
!!to change compression rate, pleasr modify sigK( the number of sigular value)
!!to construct image from dat file, plese use matlab file: image.m

program solve1
  character*100 filename
  integer, parameter :: msize=1279, nsize=1920
  INTEGER          LDA, LDU, LDVT
  PARAMETER        ( LDA = msize, LDU = msize, LDVT = nsize )
  INTEGER          LWMAX
  PARAMETER        ( LWMAX =msize+4*nsize+64*(msize+nsize))
  INTEGER  INFO, LWORK
  DOUBLE PRECISION A( LDA, nsize ), U( LDU, msize ), VT( LDVT, nsize), S( nsize ),WORK( LWMAX ),C(msize,nsize),D(msize, nsize)

  integer::i,j
  integer::sigK

  EXTERNAL DGESVD
  EXTERNAL write
  EXTERNAL PRINT_MATRIX
  INTRINSIC INT, MIN
  filename='dog_bw_data.dat'
  open(10,file=filename)
  do i=1,msize
    read(10,*) ( A(i,j), j=1,nsize )
  enddo
 close(10)

 WRITE(*,*)'Please input sigular value k, where k=10,20,40,80,160,320...'
 read(*,*) sigK

 LWORK = -1
 CALL DGESVD( 'All', 'All', msize, nsize, A, LDA, S, U, LDU, VT, LDVT,WORK, LWORK, INFO )
 LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
 CALL DGESVD( 'All', 'All', msize, nsize, A, LDA, S, U, LDU, VT, LDVT,WORK, LWORK, INFO )
 
 IF( INFO.GT.0 ) THEN
     WRITE(*,*)'The algorithm computing SVD failed to converge.'
     STOP
  END IF
  !sigK=20 ! change this value for different compression rate.
  C=0.0
  do i=1,sigK
    C(i,i)=S(i)
  end do
D=matmul(matmul(U,C),VT)
call write(msize, nsize, D, msize)  
CALL PRINT_MATRIX( 'The first 10 singular values', 1, 10, S, 1 ) !!report the first 10 sigular value
end program solve1

subroutine write(M, N, A, LDA)
  implicit none
  INTEGER ::M, N, LDA
  character*100 ::ofile
  DOUBLE PRECISION ::A(LDA,*)
  integer::i,j

  ! file name for ascii output
  ofile = 'io.dat'
  open(unit=20,file=ofile,status='replace')
  DO I = 1, M
    WRITE(20,'(*(F14.7))') ( A( I, J ), J = 1, N )
  END DO
  close(20)
end subroutine write

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
9998 FORMAT( *(F14.7) )
  RETURN
end subroutine PRINT_MATRIX