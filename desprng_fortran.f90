module desprng_fortran

  use iso_c_binding
  private

  public :: alloca_ident, desprng_alloca_individual, desprng_alloca_common, create_identifier_f, &
    initialize_common, initialize_individual_f, get_uniform_prn_f

  interface 

    function alloca_ident(size) bind(c)
      import :: c_int, c_ptr
      type(c_ptr) :: alloca_ident
      integer(c_int), value :: size
    end function

    function desprng_alloca_individual(size) bind(c)
      import :: c_int, c_ptr
      type(c_ptr):: desprng_alloca_individual
      integer(c_int), value :: size
    end function

    function desprng_alloca_common() bind(c)
      import :: c_ptr
      type(c_ptr) :: desprng_alloca_common
    end function desprng_alloca_common

    integer(c_int) function initialize_common(process_data) bind(c)
      import ::  c_int, c_ptr
      type(c_ptr), value :: process_data
    end function initialize_common

    integer(c_int) function create_identifier_f(nident, ipart) bind(c)
      import :: c_ptr, c_int
      type(c_ptr) :: nident
      integer(c_int) :: ipart
    end function

    integer (c_int) function initialize_individual_f   &
            (process_data, thread_data, nident, ipart) bind(c)
      import :: c_int, c_ptr
      implicit none
      type(c_ptr) :: process_data
      type(c_ptr) :: thread_data
      type(c_ptr) :: nident
      integer(c_int) :: ipart
    end function

    real(c_double) function get_uniform_prn_f(process_data, thread_data, &
              icount, iprn, ipart) bind(c)
      import :: c_double, c_ptr, c_int
      implicit none
      type(c_ptr) :: process_data
      type(c_ptr) :: thread_data
      type(c_ptr) :: iprn
      integer(c_int) :: icount
      integer(c_int) :: ipart
    end function
    

  end interface

end module desprng_fortran
