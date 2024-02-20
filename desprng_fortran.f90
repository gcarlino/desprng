module desprng_fortran

  use iso_c_binding
  private

  public :: desprng_individual_t, desprng_common_t
  public :: desprng_alloca_individual, desprng_alloca_common, create_identifier, &
    initialize_common, initialize_individual_new, get_uniform_prn_new

  type, bind(c) :: desprng_individual_t
    integer(c_long) :: nident
    integer(c_long) :: KnL(32)
    integer(c_long) :: KnR(32)
    integer(c_long) :: Kn3(32)
  end type desprng_individual_t

  type, bind(c) :: desprng_common_t
    integer(c_signed_char) :: pc1(56)
    integer(c_signed_char) :: pc2(48)
    integer(c_signed_char) :: totrot(16)
    integer(c_short) :: bytebit(8)
    integer(c_long) :: bigbyte(24)
    integer(c_long) :: SP(8,64)
  end type desprng_common_t

  interface 

    integer(c_int) function desprng_alloca_individual(thread_data, size) bind(c)
      import :: c_int, desprng_individual_t
      implicit none
      type(desprng_individual_t), intent(inout) :: thread_data
      integer(c_int), value, intent(in) :: size
    end function

    integer(c_int) function desprng_alloca_common(process_data) bind(c)
      import :: c_int, desprng_common_t
      type(desprng_common_t), intent(inout) :: process_data
    end function

    integer (c_int) function initialize_common(process_data) bind(c)
      use iso_c_binding
      import :: desprng_common_t
      type(desprng_common_t) :: process_data
    end function initialize_common

    integer(c_int) function create_identifier(nident) bind(c)
      use iso_c_binding
      integer(c_int) :: nident
    end function

    integer (c_int) function initialize_individual_new   &
            (process_data, thread_data, nident, ipart) bind(c)
      use iso_c_binding
      import :: desprng_common_t, desprng_individual_t
      implicit none
      type(desprng_common_t) :: process_data
      type(desprng_individual_t) :: thread_data
      integer(c_int) :: nident
      integer(c_int) :: ipart
    end function

    real(c_double) function get_uniform_prn_new(process_data, thread_data, &
              icount, iprn, ipart) bind(c)
      use iso_c_binding
      import :: desprng_common_t, desprng_individual_t
      implicit none
      type(desprng_common_t) :: process_data
      type(desprng_individual_t) :: thread_data
      integer(c_int) :: icount
      integer(c_int) :: iprn
      integer(c_int) :: ipart
    end function
    

  end interface

end module desprng_fortran
