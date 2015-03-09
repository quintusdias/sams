MODULE MvnormModule
    INTEGER seed
END MODULE

subroutine drnmvn(nr, k, rsig, ldrsig, r, ldr)
!*****************************************************************************80
!
! DRNMVN Generates double precision pseudorandom numbers from a multivariate
! normal distribution.
!
!  Discussion:
!
!    Compatibility wrapper for IMSL DRNMVN routine.
!
!  Modified:
!
!    November 3, 2014
!
!  Author:
!
!    John Evans
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NR, number of random multivariate normal
!       vectors to generate.
!
!    Input, integer ( kind = 4 ) K, length of multivariate normal vectors.
!
!    Input, real ( kind = 8 ) RSIG, upper triangular matrix, K by K, containing
!       the Cholesky factor of the variance-covariance matrix.  The variance-
!       covariance matrix is equal to the product of the transpose of RSIG and
!       RSIG.  RSIG can be obtained from the variance-covariance matrix using
!       routine CHFAC.
!
!    Input, integer ( kind = 4 ) LDRSIG, leading dimension of RSIG exactly as
!       specified in the dimension statement in the calling program.
!
!    Output, real ( kind = 8 ) R, NR by K matrix containing the random 
!       multivariate normal vectors in its rows.
!    
!    Input, integer ( kind = 4 ) LDR, leading dimension of R exactly as
!       specified in the dimension statement of the calling program.
!
  use MvnormModule

  implicit none
  Integer, Intent(In) :: nr
  Integer, Intent(In) :: k
  Real*8, Dimension(1:k, 1:k), Intent(In) :: rsig
  Integer, Intent(In) :: ldrsig
  Real*8, Dimension(1:nr, 1:k), Intent(InOut) :: r
  Integer, Intent(In) :: ldr

  integer ( kind = 4 ) j

!
!  Get an MxN matrix of samples of the 1D normal distribution with mean 0
!  and variance 1.  
!
! call r8vec_normal_01 ( m*n, seed, x(1:m,1:n) )
  call r8vec_normal_01 ( k*nr, seed, r(1:k,1:nr) )
!
!  Compute R' * X.
!  We actually carry out this computation in the equivalent form X' * R.
!
  do j = 1, nr
    !x(1:m,j) = mu(1:m) + matmul ( x(1:m,j), r(1:m,1:m) )
    ! mu (means) is 0
    r(1:k,j) = matmul ( r(1:k,j), rsig(1:k,1:k) )
  end do

  return
end

subroutine dlftds(n, a, lda, fact, ldfact)
!*****************************************************************************80
!
! DLFTDS Computes the R^T * R Cholesky factorization of a real symmetric 
! positive definite matrix.
!
!  Discussion:
!
!    Compatibility wrapper for IMSL DLFTDS routine.
!
!  Modified:
!
!    November 3, 2014
!
!  Author:
!
!    John Evans
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, order of the matrix.
!
!    Input, real ( kind = 8 ) A, N by N symmetric positive definite matrix to be
!       factored.  Only the upper triangular of A is referenced.
!
!    Input, integer ( kind = 4 ) LDA, leading dimension of A exactly as
!       specified in the dimension statement of the calling program.
!
!    Output, real ( kind = 8 ) FACT, N by N matrix containing the upper
!       triangular matrix R of the factorization of A in the upper triangle, and
!       the lower triangular matrix R^T in the lower triangle.  If A is not
!       needed, A and FACT can share the same storage location.
!
!    Input, integer ( kind = 4 ) LDFACT, leading dimension of FACT exactly as
!       specified in the dimension statement of the calling program.
!
  implicit none
  Integer, Intent(In) :: n
  Real*8, Dimension(1:n,1:n), Intent(In) :: a
  Integer, Intent(In) :: lda
  Real*8,Dimension(1:n,1:n), Intent(InOut) :: fact
  Integer, Intent(In) :: ldfact

! Success or failure of cholesky factorization.
  integer ( kind = 4 ) info

  call r8po_fa ( n, a, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DLFTDS - Fatal error!'
    write ( *, '(a)' ) &
      '  The variance-covariance matrix is not positive definite symmetric.'
    stop
  end if

  fact(1:n, 1:n) = a(1:n, 1:n)

  return
end

subroutine rnopt ( iopt )

!*****************************************************************************80
!
!! RNOPT Selects the uniform (0, 1) multiplicative congruential pseudorandom
!    number generator.
!
!  Discussion:
!
!    Compatibility wrapper for IMSL RNOPT.  This subroutine is actually a no-op.
!
!  Modified:
!
!    31 October 2014
!
!  Author:
!
!    John Evans
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IOPT, indicator of the generator.
!    
!       The random number generator is either a multiplicative congruential
!       generator with modulus 231 − 1 or a GFSR generator. IOPT is used to
!       choose the multiplier and whether or not shuffling is done, or is used
!       to choose the GFSR method, or is used to choose the Mersenne Twister
!       generator.
!
!       IOPT Generator
!           1  The multiplier 16807 is used.
!           2  The multiplier 16807 is used with shuffling.
!           3  The multiplier 397204094 is used.
!           4  The multiplier 397204094 is used with shuffling.
!           5  The multiplier 950706376 is used.
!           6  The multiplier 950706376 is used with shuffling.
!           7  GFSR, with the recursion X(t) = X(t−1563) ⊕ X(t−96) is used.
!           8  A 32-bit Mersenne Twister generator is used. The real and
!                  double random numbers are generated from 32-bit integers.
!           9  A 64-bit Mersenne Twister generator is used. The real and double
!                  random numbers are generated from 64-bit integers. This
!                  ensures that all bits of both float and double are random.
!
  implicit none
  Integer, Intent(In) :: iopt

  return
end

subroutine rnset ( seed_in )

!*****************************************************************************80
!
!! RNOPT sets the random number generator seed.
!
!  Discussion:
!
!    Compatibility wrapper for IMSL routine RNSET.  
!
!  Modified:
!
!    31 October 2014
!
!  Author:
!
!    John Evans
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SEED, indicator of the generator.
!    
!
  USE MvnormModule
  implicit none
  Integer, Intent(In) :: seed_in

  seed = seed_in

  return
end

