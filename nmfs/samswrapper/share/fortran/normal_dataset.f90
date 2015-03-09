program main

!*****************************************************************************80
!
!! MAIN is the main program for NORMAL_DATASET.
!
!  Discussion:
!
!    NORMAL_DATASET generates a dataset of multivariate normal random values,
!    and writes it to a file.
!
!  Usage:
!
!    normal_dataset m n seed mu a
!
!    where
!
!    * M, the spatial dimension,
!    * N, the number of points to generate,
!    * SEED, the seed, a positive integer.
!    * MU, the mean vector.
!    * A, the MxM variance-covariance matrix.
!
!    creates an M by N multivariate normal random dataset and writes it 
!    to the file "normal_M_N.txt".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 December 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  integer ( kind = 4 ) arg_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) last
  integer ( kind = 4 ) m
  real ( kind = 8 ), allocatable, dimension ( : ) :: mu
  integer ( kind = 4 ) n
  character ( len = 255 ) output_filename
  integer ( kind = 4 ) seed
  character ( len = 255 ) string
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: x

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_DATASET'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Generate a multivariate normal random dataset.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The program requests input values from the user:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  * M, the spatial dimension,'
  write ( *, '(a)' ) '  * N, the number of points to generate,'
  write ( *, '(a)' ) '  * SEED, a positive integer.'
  write ( *, '(a)' ) '  * MU, the mean vector.'
  write ( *, '(a)' ) '  * A, the variance-covariance matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The program generates the data and'
  write ( *, '(a)' ) '  writes it to the file'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    normal_M_N.txt'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  where "M" and "N" are the numeric values.'
!
!  Get the number of command line arguments.
!
  arg_num = iargc ( )
!
!  Get the spatial dimension M.
!
  if ( 1 <= arg_num ) then
    iarg = 1
    call getarg ( iarg, string )
    call s_to_i4 ( string, m, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the spatial dimension M:'
    read ( *, * ) m
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension M = ', m
!
!  Get the number of points N.
!
  if ( 2 <= arg_num ) then
    iarg = 2
    call getarg ( iarg, string )
    call s_to_i4 ( string, n, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the number of points N:'
    read ( *, * ) n
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of points N = ', n
!
!  Get the seed, SEED
!
  if ( 3 <= arg_num ) then
    iarg = 3
    call getarg ( iarg, string )
    call s_to_i4 ( string, seed, ierror, last )
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the seed SEED:'
    read ( *, * ) seed
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Seed SEED = ', seed
!
!  Get the mean vector MU.
!
  allocate ( mu(1:m) )

  if ( 4 <= arg_num ) then
    iarg = 4
    call getarg ( iarg, string )
    read(string, '(f5.2)') mu(1)
    iarg = 5
    call getarg ( iarg, string )
    read(string, '(f5.2)') mu(2)
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the mean vector MU'
    read ( *, * ) mu(1:m)
  end if

  call r8vec_print ( m, mu, '  Mean vector MU:' )
!
!  Get the variance-covariance matrix A.
!
  allocate ( a(1:m,1:m) )

  if ( 6 <= arg_num ) then
    iarg = 6
    call getarg ( iarg, string )
    read(string, '(f5.2)') a(1,1)
    iarg = 7
    call getarg ( iarg, string )
    read(string, '(f5.2)') a(1,2)
    iarg = 8
    call getarg ( iarg, string )
    read(string, '(f5.2)') a(2,1)
    iarg = 9
    call getarg ( iarg, string )
    read(string, '(f5.2)') a(2,2)
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Enter the variance/covariance matrix A'
    read ( *, * ) a(1:m,1:m)
  end if

  call r8mat_print ( m, m, a, '  Variance/covariance matrix:' )
!
!  Compute the data.
!
  allocate ( x(1:m,1:n) )

  call multinormal_sample ( m, n, a, mu, seed, x )
!
!  Write the data to a file.
!
  write ( output_filename, '(a,i2.2,a,i5.5,a)' ) 'normal_', m, '_', n, '.txt'

  call r8mat_write ( output_filename, m, n, x )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The data was written to the file "' &
     // trim ( output_filename ) // '".'
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( mu )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_DATASET'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
