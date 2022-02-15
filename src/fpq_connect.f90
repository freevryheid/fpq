module fpq_connect
  !! ## Database Connection Control Functions
  !! [Documentation.](https://www.postgresql.org/docs/current/libpq-connect.html)
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Connection parameters
  integer(kind=c_int), parameter, public :: CONNECTION_OK = 0
    !! Connection success.
  integer(kind=c_int), parameter, public :: CONNECTION_BAD = 1
    !! Connection failed, typically because of invalid connection parameters.
  integer(kind=c_int), parameter, public :: CONNECTION_STARTED = 2
    !!  Waiting for connection to be made. Non-blocking from here on.
  integer(kind=c_int), parameter, public :: CONNECTION_MADE = 3
    !!  Connection OK; waiting to send.
  integer(kind=c_int), parameter, public :: CONNECTION_AWAITING_RESPONSE = 4
    !!  Waiting for a response from the postmaster.
  integer(kind=c_int), parameter, public :: CONNECTION_AUTH_OK = 5
  !!  Received authentication; waiting for backend startup.
  integer(kind=c_int), parameter, public :: CONNECTION_SETENV = 6
    !!  Negotiating environment.
  integer(kind=c_int), parameter, public :: CONNECTION_SSL_STARTUP = 7
    !!  Negotiating SSL.
  integer(kind=c_int), parameter, public :: CONNECTION_NEEDED = 8
    !!  Internal state: connect() needed.
  integer(kind=c_int), parameter, public :: CONNECTION_CHECK_WRITABLE = 9
    !!  Check if we could make a writable connection.
  integer(kind=c_int), parameter, public :: CONNECTION_CONSUME = 10
    !!  Wait for any pending message and consume them.
  integer(kind=c_int), parameter, public :: CONNECTION_GSS_STARTUP = 11
    !!  Negotiating GSSAPI.
  integer(kind=c_int), parameter, public :: CONNECTION_CHECK_TARGET = 12
    !!  Check if we have a proper target connection.

  ! Ping parameters
  integer(kind=c_int), parameter, public :: PQPING_OK = 0
    !! Server is accepting connections.
  integer(kind=c_int), parameter, public :: PQPING_REJECT = 1
    !! Server is alive but rejecting connections.
  integer(kind=c_int), parameter, public :: PQPING_NO_RESPONSE = 2
    !! Could not establish connection.
  integer(kind=c_int), parameter, public :: PQPING_NO_ATTEMPT = 3
    !! Connection not attempted (bad params).

  public :: connectdbparams
  public :: connectdb
  public :: setdblogin
  public :: finish
  public :: reset
  public :: ping
  public :: pingparams

  interface

    ! PGconn *PQconnectdbParams(const char * const *keywords, const char * const *values, int expand_dbname);
    function connectdbparams(keywords, values, expand_dbname) bind(c, name='PQconnectdbParams') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords, values
        !! Arrays of string pointers
      integer(kind=c_int), intent(in) :: expand_dbname
        !! Flag.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function connectdbparams

    ! PGconn *PQconnectdb(const char *conninfo);
    function connectdb(conninfo) bind(c, name='PQconnectdb') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
        !! Connection string.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function connectdb

    ! PGconn *PQsetdbLogin(const char *pghost,
    !                      const char *pgport,
    !                      const char *pgoptions,
    !                      const char *pgtty,
    !                      const char *dbName,
    !                      const char *login,
    !                      const char *pwd);
    function setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) bind(c, name='PQsetdbLogin') result(pgconn)
      !! Makes a new connection to the database server.
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      type(c_ptr) :: pgconn
        !! Database connection pointer.
    end function setdblogin

    ! void PQfinish(PGconn *conn);
    subroutine finish(pgconn) bind(c, name='PQfinish')
      !! Closes the connection to the server.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
    end subroutine finish

    ! void PQreset(PGconn *conn);
    subroutine reset(pgconn) bind(c, name='PQreset')
      !! Resets the communication channel to the server.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
    end subroutine reset

    ! PGPing PQpingParams(const char * const *keywords,
    !                     const char * const *values,
    !                     int expand_dbname);
    function pingparams(keywords, values, expand_dbname) bind(c, name='PQpingParams') result(r)
      !! Reports the status of the server.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords, values
        !! Arrays of string pointers
      integer(kind=c_int), intent(in) :: expand_dbname
        !! Flag.
      integer(kind=c_int) :: r
        !! Database connection pointer.
    end function pingparams

    ! PGPing PQping(const char *conninfo);
    function ping(conninfo) bind(c, name='PQping') result(r)
      !! Reports the status of the server.
      import :: c_char, c_int
      implicit none
      character(kind=c_char), intent(in) :: conninfo
        !! Connection string.
      integer(kind=c_int) :: r
    end function ping

  end interface

end module fpq_connect
