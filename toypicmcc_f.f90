program test_desprng

  use iso_c_binding, only: c_ptr, c_int
  use desprng_f
  use openacc

  implicit none

  integer :: ntime = 4, itime
  integer :: ncoll = 2, icoll

  type(c_ptr) :: thread_data
  type(c_ptr) :: process_data
  type(c_ptr) :: nident
  type(c_ptr) :: iprn

  integer(c_int) :: npart = 4000000
  integer(c_int) :: ipart, icount

  real :: xprn, zeta, czeta, zaverage = 0.0, zvariance = 0.0, dt = 1.0E-2, xt
  real, dimension(:), allocatable :: xi
  real, parameter :: xi0 = 1. / SQRT(2.)
  integer :: ierr

  nident = alloca_ident(8 * (npart + 1))
  thread_data = desprng_alloca_individual(npart + 1)
  process_data = desprng_alloca_common()

  ierr = initialize_common(process_data)
  if (ierr .ne. 0) then
    print *, "Error initialize_common"
  end if

  allocate(xi(npart))
  czeta = SQRT(12.0) 
  xt = ntime * dt

  !$acc data copyout(xi) create(nident)
  do itime = 0, ntime - 1
    !$acc serial loop reduction(+: zaverage, zvariance) private(iprn)
    do ipart = 1, npart
      if (itime .eq. 0) then
        ierr = create_identifier_f(nident, ipart)
        if (ierr .ne. 0) then
          print *, "Error create_identifier_f"
        end if
        ierr = initialize_individual_f(process_data, thread_data, nident, ipart)
        if (ierr .ne. 0) then
          print *, "Error initialize_individual_f"
        end if
        xi(ipart) = xi0
      end if
      do icoll = 0, ncoll - 1
        ! FIXME: is it 16 or 8 bits?
        icount = ISHFT(itime, 16) + icoll
        xprn = get_uniform_prn_f(process_data, thread_data, icount, ipart)
        zeta = czeta * (xprn - 0.5)
        zaverage = zaverage + zeta
        zvariance = zvariance + zeta * zeta
        xi(ipart) = xi(ipart) + (-2.0 * xi(ipart) * dt / ncoll + zeta *  & 
          sqrt(2.0 * (1.0 - xi(ipart) * xi(ipart)) * dt / ncoll))
      end do
    end do
    !$acc end serial loop 
  end do
  !$acc end data

  zaverage = zaverage / (ntime * npart * ncoll)
  zvariance = zvariance / (ntime * npart * ncoll)
  print *, 'average = ', zaverage, 'variance = ', zvariance
  print *, 'npart = ', npart
  print *, 'xi0 = ', xi0
  print *, 'xt = ', xt

  open(99,FILE='./xi.dat',ACCESS='STREAM')
  write(99)npart
  write(99)xi0
  write(99)xt
  write(99)xi
  close(99)

  ! Deallocate
  deallocate(xi)

end program test_desprng
