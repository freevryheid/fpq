module fpq_status
  !! # Connection Status Functions
  !! [Documentation.](https://www.postgresql.org/docs/current/libpq-status.html)
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Transaction parameters
  integer, parameter, public :: PQTRANS_IDLE = 0
    !! Connection idle.
  integer, parameter, public :: PQTRANS_ACTIVE = 1
    !! Command in progress.
  integer, parameter, public :: PQTRANS_INTRANS = 2
    !! Idle, within transaction block.
  integer, parameter, public :: PQTRANS_INERROR = 3
    !! Idle, within failed transaction
  integer, parameter, public :: PQTRANS_UNKNOWNi = 4
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
    function pqoptions(pgconn) bind(c, name='PQoptions') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      type(c_ptr) :: r
    end function pqoptions

    ! ConnStatusType PQstatus(const PGconn *conn);
    function pqstatus(pgconn) bind(c, name='PQstatus') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqstatus

    ! PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
    function pqtransactionstatus(pgconn) bind(c, name='PQtransactionStatus') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqtransactionstatus

    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! FIXME
    function pqparameterstatus(pgconn, param) bind(c, name='PQparameterStatus') result(r)
      import :: c_char, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: param
      type(c_ptr) :: r
    end function pqparameterstatus

    ! int PQprotocolVersion(const PGconn *conn);
    function pqprotocolversion(pgconn) bind(c, name='PQprotocolVersion') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqprotocolversion

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
    function pqsocket(pgconn) bind(c, name='PQsocket') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqsocket

    ! int PQbackendPID(const PGconn *conn);
    function pqbackendpid(pgconn) bind(c, name='PQbackendPID') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqbackendpid

    ! int PQconnectionNeedsPassword(const PGconn *conn);
    function pqconnectionneedspassword(pgconn) bind(c, name='PQconnectionNeedsPassword') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqconnectionneedspassword

    ! int PQconnectionUsedPassword(const PGconn *conn);
    function pqconnectionusedpassword(pgconn) bind(c, name='PQconnectionUsedPassword') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqconnectionusedpassword

    ! int PQsslInUse(const PGconn *conn);
    function pqsslinuse(pgconn) bind(c, name='PQsslInUse') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqsslinuse

    ! const char *PQsslAttribute(const PGconn *conn, const char *attribute_name);

    ! const char * const * PQsslAttributeNames(const PGconn *conn);

    ! void *PQsslStruct(const PGconn *conn, const char *struct_name);

    ! void *PQgetssl(const PGconn *conn);

  end interface

  contains

    function db(conn) result(r)
      !! Returns the database name of the connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
      type(c_ptr) :: cptr
      cptr = pqdb(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function db

    function user(conn) result(r)
      !! Returns the user name of the connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pquser(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function user

    function pass(conn) result(r)
      !! Returns the password of the connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqpass(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function pass

    function host(conn) result(r)
      !! Returns the server host name of the active connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqhost(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function host

    function hostaddr(conn) result(r)
      !! Returns the server IP address of the active connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqhostaddr(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function hostaddr

    function port(conn) result(r)
      !! Returns the port of the active connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqport(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function port

    function options(conn) result(r)
      !! Returns the command-line options passed in the connection request.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqoptions(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function options

    function status(conn) result(r)
      !! Returns the status of the connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqstatus(conn%ptr))
    end function status

    function transactionstatus(conn) result(r)
      !! Returns the current in-transaction status of the server.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqtransactionstatus(conn%ptr))
    end function transactionstatus

    function parameterstatus(conn, param) result(r)
      !! Looks up a current parameter setting of the server.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      character(len=*), intent(in) :: param
        !! Parameter name e.g server_version, server_encoding, client_encoding,
        !! application_name, is_superuser, session_authorization, DateStyle,
        !! IntervalStyle, TimeZone, integer_datetimes, and standard_conforming_strings, etc.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqparameterstatus(conn%ptr, c_str(param))
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function parameterstatus

    function protocolversion(conn) result(r)
      !! Interrogates the frontend/backend protocol being used.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqprotocolversion(conn%ptr))
    end function protocolversion

    function serverversion(conn) result(r)
      !! Returns an integer representing the server version.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqserverversion(conn%ptr))
    end function serverversion

    function errormessage(conn) result(r)
      !! Returns the error message most recently generated by an operation
      !! on the connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      type(c_ptr) :: cptr
      character(len=:), allocatable :: r
      cptr = pqerrormessage(conn%ptr)
      if (c_associated(cptr)) call c_f_str_ptr(cptr, r)
    end function errormessage

    function socket(conn) result(r)
      !! Obtains the file descriptor number of the connection socket to the server.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqsocket(conn%ptr))
    end function socket

    function backendpid(conn) result(r)
      !! Returns the process ID (PID) of the backend process handling this connection.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer :: r
      r = int(pqbackendpid(conn%ptr))
    end function backendpid

    function connectionneedspassword(conn) result(r)
      !! Returns true if the connection authentication method required a password,
      !! but none was available. Returns false if not.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer(kind=c_int) :: i
      logical :: r
      i = pqconnectionneedspassword(conn%ptr)
      if (i > 0) then
        r = .true.
      else
        r = .false.
      end if
    end function connectionneedspassword

    function connectionusedpassword(conn) result(r)
      !! Returns true if the connection authentication method used a password.
      !! Returns false if not.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer(kind=c_int) :: i
      logical :: r
      i = pqconnectionusedpassword(conn%ptr)
      if (i > 0) then
        r = .true.
      else
        r = .false.
      end if
    end function connectionusedpassword

    function sslinuse(conn) result(r)
      !! Returns true if the connection authentication method used a password.
      !! Returns false if not.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      integer(kind=c_int) :: i
      logical :: r
      i = pqsslinuse(conn%ptr)
      if (i > 0) then
        r = .true.
      else
        r = .false.
      end if
    end function sslinuse

end module fpq_status

