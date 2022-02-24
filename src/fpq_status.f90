module fpq_status
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Transaction parameters
  integer(kind=c_int), parameter, public :: PQTRANS_IDLE = 0
    !! Connection idle.
  integer(kind=c_int), parameter, public :: PQTRANS_ACTIVE = 1
    !! Command in progress.
  integer(kind=c_int), parameter, public :: PQTRANS_INTRANS = 2
    !! Idle, within transaction block.
  integer(kind=c_int), parameter, public :: PQTRANS_INERROR = 3
    !! Idle, within failed transaction
  integer(kind=c_int), parameter, public :: PQTRANS_UNKNOWNi = 4
    !! Cannot determine status.

  public :: pqdb
  public :: pquser
  public :: pqpass
  public :: pqhost
  public :: pqhostaddr
  public :: pqport
  public :: pqoptions
  public :: pqstatus
  public :: pqtransactionstatus
  public :: pqparameterstatus
  public :: pqprotocolversion
  public :: pqserverversion
  public :: pqerrormessage
  public :: pqsocket
  public :: pqbackendpid
  public :: pqconnectionneedspassword
  public :: pqconnectionusedpassword
  public :: pqsslinuse

  interface

    !! # Connection Status Functions
    !! [Documentation.](https://www.postgresql.org/docs/current/libpq-status.html)

    ! char *PQdb(const PGconn *conn);
    function pqdb(pgconn) bind(c, name='PQdb') result(r)
      !! Returns the database name of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqdb

    ! char *PQuser(const PGconn *conn);
    function pquser(pgconn) bind(c, name='PQuser') result(r)
      !! Returns the user name of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pquser

    ! char *PQpass(const PGconn *conn);
    function pqpass(pgconn) bind(c, name='PQpass') result(r)
      !! Returns the password of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqpass

    ! char *PQhost(const PGconn *conn);
    function pqhost(pgconn) bind(c, name='PQhost') result(r)
      !! Returns the server host name of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqhost

    ! char *PQhostaddr(const PGconn *conn);
    function pqhostaddr(pgconn) bind(c, name='PQhostaddr') result(r)
      !! Returns the server IP address of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqhostaddr

    ! char *PQport(const PGconn *conn);
    function pqport(pgconn) bind(c, name='PQport') result(r)
      !! Returns the port of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqport

    ! char *PQoptions(const PGconn *conn);
    function pqoptions(pgconn) bind(c, name='PQoptions') result(r)
      !! Returns the command-line options passed in the connection request.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqoptions

    ! ConnStatusType PQstatus(const PGconn *conn);
    function pqstatus(pgconn) bind(c, name='PQstatus') result(r)
      !! Returns the status of the connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqstatus

    ! PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
    function pqtransactionstatus(pgconn) bind(c, name='PQtransactionStatus') result(r)
      !! Returns the current in-transaction status of the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqtransactionstatus

    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! FIXME
    function pqparameterstatus(pgconn, param) bind(c, name='PQparameterStatus') result(r)
      !! Looks up a current parameter setting of the server.
      import :: c_char, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: param
        !! Parameter name e.g server_version, server_encoding, client_encoding,
        !! application_name, is_superuser, session_authorization, DateStyle,
        !! IntervalStyle, TimeZone, integer_datetimes, and standard_conforming_strings, etc.
      type(c_ptr) :: r
    end function pqparameterstatus

    ! int PQprotocolVersion(const PGconn *conn);
    function pqprotocolversion(pgconn) bind(c, name='PQprotocolVersion') result(r)
      !! Interrogates the frontend/backend protocol being used.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqprotocolversion

    ! int PQserverVersion(const PGconn *conn);
    function pqserverversion(pgconn) bind(c, name='PQserverVersion') result(r)
      !! Returns an integer representing the server version.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqserverversion

    ! char *PQerrorMessage(const PGconn *conn);
    function pqerrormessage(pgconn) bind(c, name='PQerrorMessage') result(r)
      !! Returns the error message most recently generated by an operation
      !! on the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pqerrormessage

    ! int PQsocket(const PGconn *conn);
    function pqsocket(pgconn) bind(c, name='PQsocket') result(r)
      !! Obtains the file descriptor number of the connection socket to the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqsocket

    ! int PQbackendPID(const PGconn *conn);
    function pqbackendpid(pgconn) bind(c, name='PQbackendPID') result(r)
      !! Returns the process ID (PID) of the backend process handling this connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqbackendpid

    ! int PQconnectionNeedsPassword(const PGconn *conn);
    function pqconnectionneedspassword(pgconn) bind(c, name='PQconnectionNeedsPassword') result(r)
      !! Returns true (1) if the connection authentication method required a password,
      !! but none was available. Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqconnectionneedspassword

    ! int PQconnectionUsedPassword(const PGconn *conn);
    function pqconnectionusedpassword(pgconn) bind(c, name='PQconnectionUsedPassword') result(r)
      !! Returns true (1) if the connection authentication method used a password.
      !! Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqconnectionusedpassword

    ! int PQsslInUse(const PGconn *conn);
    function pqsslinuse(pgconn) bind(c, name='PQsslInUse') result(r)
      !! Returns true (1) if the connection authentication method used a password.
      !! Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function pqsslinuse

    ! const char *PQsslAttribute(const PGconn *conn, const char *attribute_name);
    ! STILL TO DO

    ! const char * const * PQsslAttributeNames(const PGconn *conn);
    ! STILL TO DO

    ! void *PQsslStruct(const PGconn *conn, const char *struct_name);
    ! STILL TO DO

    ! void *PQgetssl(const PGconn *conn);
    ! STILL TO DO

  end interface

end module fpq_status

