program test_desprng

  use iso_c_binding, only: c_ptr, c_int
  use desprng_fortran

  implicit none

  ! integer :: npart = 10
  integer :: ntime = 4, itime
  integer :: ncoll = 2, icoll

  type(c_ptr) :: thread_data
  type(c_ptr) :: process_data
  type(c_ptr) :: nident
  type(c_ptr) :: iprn

  integer(c_int) :: npart = 400
  integer(c_int) :: ipart, icount

  real :: xprn, zeta, czeta, zaverage = 0.0, zvariance = 0.0, dt = 1.0E-2, xt
  real, dimension(:), allocatable :: xi
  real, parameter :: xi0 = 0.6
  integer :: ierr

  ! print *, "Before alloca, nident", nident
  nident = alloca_ident(8 * (npart + 1))
  ! print *, "After alloca, nident", nident
  ! print *, "Before desprng_alloca_individual"
  thread_data = desprng_alloca_individual(npart + 1)
  ! print *, "Before desprng_alloca_common, process_data", process_data
  process_data = desprng_alloca_common()
  ! print *, "After alloca, process_data", process_data

  ! print *, "Before initialize_common"
  ierr = initialize_common(process_data)
  ! print *, "After initialize_common, process_data", process_data
  ! print *, ierr
  if (ierr .ne. 0) then
    print *, "Error initialize_common"
  end if

  ! allocate(xi(8 * npart))
  allocate(xi(npart))
  czeta = SQRT(12.0) 
  xt = ntime * dt

  do itime = 1, ntime
    do ipart = 1, npart
      if (itime .eq. 1) then
        ! print *, "ipart = ", ipart
        ! print *, "Before create identifier"
        ! print *, "process_data", process_data
        ! print *, "nident", nident
        ierr = create_identifier_f(nident, ipart)
        if (ierr .ne. 0) then
          print *, "Error create_identifier_f"
        end if
        ! print *, "After create_identifier"
        ! print *, "ipart = ", ipart
        ierr = initialize_individual_f(process_data, thread_data, nident, ipart)
        if (ierr .ne. 0) then
          print *, "Error initialize_individual_f"
        end if
        ! print *, ierr, xi0
        ! print *, "After initialize individual identifier"
        ! print *, "process_data", process_data
        xi(ipart) = xi0
        ! print *, "End if"
      end if
      do icoll = 0, Ncoll - 1
        ! print *, "icoll = ", icoll
        icount = itime + icoll;
        ! print *, "Before get_uniform_prn. ipart = ", ipart
        ! print *, "process_data, icount, ipart", process_data, icount, ipart
        xprn = get_uniform_prn_f(process_data, thread_data, icount, ipart)
        ! print *, "After get_uniform_prn, xprn =", xprn
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
  ! print *, xi

  ! Deallocate
  deallocate(xi)

end program test_desprng
