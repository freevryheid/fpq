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

  public :: db
  public :: user
  public :: pass
  public :: host
  public :: hostaddr
  public :: port
  public :: options
  public :: status
  public :: transactionstatus
  public :: parameterstatus
  public :: protocolversion
  public :: serverversion
  public :: errormessage
  public :: socket
  public :: backendpid
  public :: connectionneedspassword
  public :: connectionusedpassword
  public :: sslinuse

  interface

    !! # Connection Status Functions
    !! [Documentation.](https://www.postgresql.org/docs/current/libpq-status.html)

    ! char *PQdb(const PGconn *conn);
    function db(pgconn) bind(c, name='PQdb') result(r)
      !! Returns the database name of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function db

    ! char *PQuser(const PGconn *conn);
    function user(pgconn) bind(c, name='PQuser') result(r)
      !! Returns the user name of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function user

    ! char *PQpass(const PGconn *conn);
    function pass(pgconn) bind(c, name='PQpass') result(r)
      !! Returns the password of the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function pass

    ! char *PQhost(const PGconn *conn);
    function host(pgconn) bind(c, name='PQhost') result(r)
      !! Returns the server host name of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function host

    ! char *PQhostaddr(const PGconn *conn);
    function hostaddr(pgconn) bind(c, name='PQhostaddr') result(r)
      !! Returns the server IP address of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function hostaddr

    ! char *PQport(const PGconn *conn);
    function port(pgconn) bind(c, name='PQport') result(r)
      !! Returns the port of the active connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function port

    ! char *PQoptions(const PGconn *conn);
    function options(pgconn) bind(c, name='PQoptions') result(r)
      !! Returns the command-line options passed in the connection request.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function options

    ! ConnStatusType PQstatus(const PGconn *conn);
    function status(pgconn) bind(c, name='PQstatus') result(r)
      !! Returns the status of the connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function status

    ! PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
    function transactionstatus(pgconn) bind(c, name='PQtransactionStatus') result(r)
      !! Returns the current in-transaction status of the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function transactionstatus

    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! FIXME
    function parameterstatus(pgconn, param) bind(c, name='PQparameterStatus') result(r)
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
    end function parameterstatus

    ! int PQprotocolVersion(const PGconn *conn);
    function protocolversion(pgconn) bind(c, name='PQprotocolVersion') result(r)
      !! Interrogates the frontend/backend protocol being used.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function protocolversion

    ! int PQserverVersion(const PGconn *conn);
    function serverversion(pgconn) bind(c, name='PQserverVersion') result(r)
      !! Returns an integer representing the server version.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function serverversion

    ! char *PQerrorMessage(const PGconn *conn);
    function errormessage(pgconn) bind(c, name='PQerrorMessage') result(r)
      !! Returns the error message most recently generated by an operation
      !! on the connection.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      type(c_ptr) :: r
    end function errormessage

    ! int PQsocket(const PGconn *conn);
    function socket(pgconn) bind(c, name='PQsocket') result(r)
      !! Obtains the file descriptor number of the connection socket to the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function socket

    ! int PQbackendPID(const PGconn *conn);
    function backendpid(pgconn) bind(c, name='PQbackendPID') result(r)
      !! Returns the process ID (PID) of the backend process handling this connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function backendpid

    ! int PQconnectionNeedsPassword(const PGconn *conn);
    function connectionneedspassword(pgconn) bind(c, name='PQconnectionNeedsPassword') result(r)
      !! Returns true (1) if the connection authentication method required a password,
      !! but none was available. Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function connectionneedspassword

    ! int PQconnectionUsedPassword(const PGconn *conn);
    function connectionusedpassword(pgconn) bind(c, name='PQconnectionUsedPassword') result(r)
      !! Returns true (1) if the connection authentication method used a password.
      !! Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function connectionusedpassword

    ! int PQsslInUse(const PGconn *conn);
    function sslinuse(pgconn) bind(c, name='PQsslInUse') result(r)
      !! Returns true (1) if the connection authentication method used a password.
      !! Returns false (0) if not.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      integer(kind=c_int) :: r
    end function sslinuse

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

