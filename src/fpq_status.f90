module fpq_status
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private
  public :: db, user

  interface

    ! char *PQdb(const PGconn *conn);
    function pqdb(conn) bind(c, name='PQdb') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqdb

    ! char *PQuser(const PGconn *conn);
    function pquser(conn) bind(c, name='PQuser') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pquser

  end interface

  contains

    !! # Connection Status Functions
    !! These functions can be used to interrogate the status of an existing database connection object.

    function db(conn) result(r)
      !! Returns the database name of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pqdb(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function db

    function user(conn) result(r)
      !! Returns the user name of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pquser(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function user

end module fpq_status

