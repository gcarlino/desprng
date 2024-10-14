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

    function initialize_common(process_data) bind(c)
      import ::  c_int, c_ptr
      integer(c_int) :: initialize_common
      type(c_ptr), value :: process_data
    end function initialize_common

    function create_identifier_f(nident, ipart) bind(c)
      import :: c_ptr, c_int
      integer(c_int) :: create_identifier_f
      type(c_ptr), value :: nident
      integer(c_int), value :: ipart
    end function

    function initialize_individual_f(process_data, thread_data, nident, ipart) bind(c)
      import :: c_int, c_ptr
      integer(c_int) :: initialize_individual_f
      type(c_ptr), value :: process_data
      type(c_ptr), value :: thread_data
      type(c_ptr), value :: nident
      integer(c_int), value :: ipart
    end function

    function get_uniform_prn_f(process_data, thread_data, icount, ipart) bind(c)
      import :: c_double, c_ptr, c_int
      implicit none
      real(c_double) :: get_uniform_prn_f
      type(c_ptr), value :: process_data
      type(c_ptr), value :: thread_data
      integer(c_int), value :: icount
      integer(c_int), value :: ipart
    end function
    
  end interface

end module desprng_fortran
