module fpq_misc

  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  public :: libversion

  interface

    ! int PQlibVersion(void);
    function pqlibversion() bind(c, name='PQlibVersion') result(r)
      import :: c_int
      implicit none
      integer(kind=c_int) :: r
    end function pqlibversion

  end interface

  contains

    function libversion() result(r)
      !! Return the version of libpq that is being used.
      integer :: r
      r = int(pqlibversion())
    end function libversion

end module fpq_misc

