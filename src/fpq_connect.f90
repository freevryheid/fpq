module fpq_connect
  !! ## Database Connection Control Functions
  !! [Documentation.](https://www.postgresql.org/docs/current/libpq-connect.html)
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Connection parameters
  integer, parameter, public :: CONNECTION_OK = 0
    !! Connection success.
  integer, parameter, public :: CONNECTION_BAD = 1
    !! Connection failed, typically because of invalid connection parameters.
  integer, parameter, public :: CONNECTION_STARTED = 2
    !!  Waiting for connection to be made. Non-blocking from here on.
  integer, parameter, public :: CONNECTION_MADE = 3
    !!  Connection OK; waiting to send.
  integer, parameter, public :: CONNECTION_AWAITING_RESPONSE = 4
    !!  Waiting for a response from the postmaster.
  integer, parameter, public :: CONNECTION_AUTH_OK = 5
  !!  Received authentication; waiting for backend startup.
  integer, parameter, public :: CONNECTION_SETENV = 6
    !!  Negotiating environment.
  integer, parameter, public :: CONNECTION_SSL_STARTUP = 7
    !!  Negotiating SSL.
  integer, parameter, public :: CONNECTION_NEEDED = 8
    !!  Internal state: connect() needed.
  integer, parameter, public :: CONNECTION_CHECK_WRITABLE = 9
    !!  Check if we could make a writable connection.
  integer, parameter, public :: CONNECTION_CONSUME = 10
    !!  Wait for any pending message and consume them.
  integer, parameter, public :: CONNECTION_GSS_STARTUP = 11
    !!  Negotiating GSSAPI.
  integer, parameter, public :: CONNECTION_CHECK_TARGET = 12
    !!  Check if we have a proper target connection.

  ! Ping parameters
  integer, parameter, public :: PQPING_OK = 0
    !! Server is accepting connections.
  integer, parameter, public :: PQPING_REJECT = 1
    !! Server is alive but rejecting connections.
  integer, parameter, public :: PQPING_NO_RESPONSE = 2
    !! Could not establish connection.
  integer, parameter, public :: PQPING_NO_ATTEMPT = 3
    !! Connection not attempted (bad params).

  ! low-level
  ! public :: pqping
  public :: pqpingparams
  public :: pqconnectdbparams
  public :: pqconnectdb
  public :: pqsetdblogin
  public :: pqfinish
  public :: pqreset
  ! high-level
  public :: ping

  interface

    ! PGPing PQping(const char *conninfo);
    function pqping(conninfo) bind(c, name='PQping') result(r)
      import :: c_char, c_int
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      integer(kind=c_int) :: r
    end function pqping

    ! PGPing PQpingParams(const char * const *keywords,
    !                     const char * const *values,
    !                     int expand_dbname);
    function pqpingparams(keywords, values, expand_dbname) bind(c, name='PQpingParams') result(r)
      !! Reports the status of the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords(:)
        !! Pointer to keyword string array
      type(c_ptr), intent(in) :: values(:)
        !! Pointer to value string array
      integer(kind=c_int), intent(in) :: expand_dbname
        !! Flag (see documentation).
      integer(kind=c_int) :: r
    end function pqpingparams

    ! PGconn *PQconnectdbParams(const char * const *keywords, const char * const *values, int expand_dbname);
    function pqconnectdbparams(keywords, values, expand_dbname) bind(c, name='PQconnectdbParams') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords(:)
        !! Pointer to keyword string array
      type(c_ptr), intent(in) :: values(:)
        !! Pointer to value string array
      integer(kind=c_int), intent(in) :: expand_dbname
        !! Flag (see documentation).
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function pqconnectdbparams

    ! PGconn *PQconnectdb(const char *conninfo);
    function pqconnectdb(conninfo) bind(c, name='PQconnectdb') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
        !! Connection string.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function pqconnectdb

    ! PGconn *PQsetdbLogin(const char *pghost,
    !                      const char *pgport,
    !                      const char *pgoptions,
    !                      const char *pgtty,
    !                      const char *dbName,
    !                      const char *login,
    !                      const char *pwd);
    function pqsetdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) bind(c, name='PQsetdbLogin') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function pqsetdblogin

    ! void PQfinish(PGconn *conn);
    subroutine pqfinish(pgconn) bind(c, name='PQfinish')
      !! Closes the connection to the server.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
    end subroutine pqfinish

    ! void PQreset(PGconn *conn);
    subroutine pqreset(pgconn) bind(c, name='PQreset')
      !! Resets the communication channel to the server.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
    end subroutine pqreset

  end interface

  contains

    function ping(conninfo) result(r)
      !! Reports the status of the server.
      !! It is not necessary to supply correct user name, password,
      !! or database name values to obtain the server status;
      !! however, if incorrect values are provided,
      !! the server will log a failed connection attempt.
      character(*), intent(in) :: conninfo
        !! Connection string.
      integer :: r
      r = int(pqping(c_str(conninfo)))
    end function ping

end module fpq_connect
