module fpq_misc

  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  public ::

  interface

    ! int PQlibVersion(void);
    function pqlibversion() bind(c, name='PQlibVersion') result(r)
      !! Return the version of libpq that is being used.
      import :: c_int
      implicit none
      integer(kind=c_int) :: r
    end function pqlibversion

  end interface

end module fpq_misc

