module fpq_status
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private
  public :: db, user, pass, host, hostaddr, port, status, serverversion, errormessage

  interface

    ! char *PQdb(const PGconn *conn);
    function pqdb(pgconn) bind(c, name='PQdb') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqdb

    ! char *PQuser(const PGconn *conn);
    function pquser(pgconn) bind(c, name='PQuser') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn

      type(c_ptr) :: r
    end function pquser

    ! char *PQpass(const PGconn *conn);
    function pqpass(pgconn) bind(c, name='PQpass') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqpass

    ! char *PQhost(const PGconn *conn);
    function pqhost(pgconn) bind(c, name='PQhost') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqhost

    ! char *PQhostaddr(const PGconn *conn);
    function pqhostaddr(pgconn) bind(c, name='PQhostaddr') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqhostaddr

    ! char *PQport(const PGconn *conn);
    function pqport(pgconn) bind(c, name='PQport') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqport

    ! char *PQoptions(const PGconn *conn);
    ! STILL TO DO

    ! ConnStatusType PQstatus(const PGconn *conn);
    function pqstatus(pgconn) bind(c, name='PQstatus') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqstatus

    ! PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
    ! STILL TO DO

    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! STILL TO DO

    ! int PQprotocolVersion(const PGconn *conn);
    ! STILL TO DO

    ! int PQserverVersion(const PGconn *conn);
    function pqserverversion(pgconn) bind(c, name='PQserverVersion') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqserverversion

    ! char *PQerrorMessage(const PGconn *conn);
    function pqerrormessage(pgconn) bind(c, name='PQerrorMessage') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqerrormessage

    ! int PQsocket(const PGconn *conn);
    ! STILL TO DO

    ! int PQbackendPID(const PGconn *conn);
    ! STILL TO DO

    ! int PQconnectionNeedsPassword(const PGconn *conn);
    ! STILL TO DO

    ! int PQconnectionUsedPassword(const PGconn *conn);
    ! STILL TO DO

    ! int PQsslInUse(const PGconn *conn);
    ! STILL TO DO

    ! const char *PQsslAttribute(const PGconn *conn, const char *attribute_name);
    ! STILL TO DOy

    ! const char * const * PQsslAttributeNames(const PGconn *conn);
    ! STILL TO DO

    ! void *PQsslStruct(const PGconn *conn, const char *struct_name);
    ! STILL TO DO

    ! void *PQgetssl(const PGconn *conn);
    ! STILL TO DO

  end interface

  contains

    !! # Connection Status Functions
    !! [Documentation.](https://www.postgresql.org/docs/current/libpq-status.html)

    function db(pgconn) result(r)
      !! Returns the database name of the connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pqdb(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function db

    function user(pgconn) result(r)
      !! Returns the user name of the connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pquser(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function user

    function pass(pgconn) result(r)
      !! Returns the password of the connection.
      type(c_ptr), intent(in) :: pgconn

        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Password.
      type(c_ptr) :: ptr
      ptr = pqpass(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function pass

    function host(pgconn) result(r)
      !! Returns the server host name of the active connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Hostname.
      type(c_ptr) :: ptr
      ptr = pqhost(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function host

    function hostaddr(pgconn) result(r)
      !! Returns the server IP address of the active connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Hostaddr.
      type(c_ptr) :: ptr
      ptr = pqhostaddr(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function hostaddr

    function port(pgconn) result(r)
      !! Returns the port of the active connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Port.
      type(c_ptr) :: ptr
      ptr = pqport(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function port

    function status(pgconn) result(r)
      !! Returns the status of the connection.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
        !! Status.
      r = pqstatus(pgconn)
    end function status

    function serverversion(pgconn) result(r)
      !! Returns an integer representing the server version.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
        !! Result by multiplying the server's major version number by 10000
        !! and adding the minor version number.
      r = pqserverversion(pgconn)
    end function serverversion

    function errormessage(pgconn) result(r)
      !! Returns the error message most recently generated by an operation on the connection.
      type(c_ptr), intent(in) :: pgconn

        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Error.
      type(c_ptr) :: ptr
      ptr = pqerrormessage(pgconn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function errormessage

end module fpq_status

