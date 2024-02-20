program test_desprng

  use desprng_fortran

  implicit none

  integer :: npart = 400
  integer :: ntime = 4, ipart, itime, icount, iprn
  integer, dimension(:), allocatable :: nident
  integer :: ncoll = 2, icoll

  type(desprng_common_t) :: process_data
  type(desprng_individual_t) :: thread_data

  real :: xprn, zeta, czeta, zaverage = 0.0, zvariance = 0.0, dt = 1.0E-2, xt
  real, dimension(:), allocatable :: xi
  real, parameter :: xi0 = 0.6
  integer :: ierr, err

  czeta = SQRT(12.0) 
  xt = ntime * dt

  allocate(nident(8 * npart))
  allocate(xi(8 * npart))

  ierr = desprng_alloca_individual(thread_data, npart)
  ierr = desprng_alloca_common(process_data)

  ierr = initialize_common(process_data)
  if (ierr .ne. 0) then
    print *, "Error initialize_common"
  end if

  do itime = 0, ntime
    do ipart = 0, npart
      if (itime .eq. 1) then
        nident(ipart) = ipart
        ierr = create_identifier(nident(ipart))
        ierr = initialize_individual_new(process_data, thread_data, nident(1), ipart)
        xi(ipart) = xi0
      end if
      do icoll = 0, Ncoll
        icount = itime + icoll
        xprn = get_uniform_prn_new(process_data, thread_data, icount, iprn, ipart)
        zeta = czeta * (xprn - 0.5)
        zaverage = zaverage + zeta
        zvariance = zvariance + zeta * zeta
        xi(ipart) = xi(ipart) -2.0 * xi(ipart) * dt / ncoll + zeta *  & 
                   sqrt(2.0 * (1.0 - xi(ipart) * xi(ipart)) * dt / ncoll)
      end do
    end do
  end do

    zaverage = zaverage / (ntime * npart * ncoll)
  zvariance = zvariance / (ntime * npart * ncoll)
  print *, zaverage, zvariance

  ! Deallocate
  deallocate(nident)
  deallocate(xi)

end program test_desprng
