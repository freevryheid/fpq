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

  public :: ping
  public :: pingparams
  public :: connectdbparams
  public :: connectdb
  public :: setdblogin
  public :: finish
  public :: reset

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
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords(:)
      type(c_ptr), intent(in) :: values(:)
      integer(kind=c_int), intent(in) :: expand_dbname
      integer(kind=c_int) :: r
    end function pqpingparams

    ! PGconn *PQconnectdbParams(const char * const *keywords, const char * const *values, int expand_dbname);
    function pqconnectdbparams(keywords, values, expand_dbname) bind(c, name='PQconnectdbParams') result(conn)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords(:)
      type(c_ptr), intent(in) :: values(:)
      integer(kind=c_int), intent(in) :: expand_dbname
      type(c_ptr) :: conn
    end function pqconnectdbparams

    ! PGconn *PQconnectdb(const char *conninfo);
    function pqconnectdb(conninfo) bind(c, name='PQconnectdb') result(conn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      type(c_ptr) :: conn
    end function pqconnectdb

    ! PGconn *PQsetdbLogin(const char *pghost,
    !                      const char *pgport,
    !                      const char *pgoptions,
    !                      const char *pgtty,
    !                      const char *dbName,
    !                      const char *login,
    !                      const char *pwd);
    function pqsetdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) bind(c, name='PQsetdbLogin') result(conn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      type(c_ptr) :: conn
    end function pqsetdblogin

    ! void PQfinish(PGconn *conn);
    subroutine pqfinish(conn) bind(c, name='PQfinish')
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
    end subroutine pqfinish

    ! void PQreset(PGconn *conn);
    subroutine pqreset(conn) bind(c, name='PQreset')
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
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

    function pingparams(keywords, values, expand_dbname) result(r)
      !! Reports the status of the server.
      character(len=*), intent(inout), target :: keywords(:)
        !! Keyword string array
      character(len=*), intent(inout), target :: values(:)
        !! Value string array
      integer, intent(in) :: expand_dbname
        !! Flag (see documentation).
      integer :: i, r
      do i = 1, size(keywords)
        keywords(i) = c_str(keywords(i))
        values(i) = c_str(values(i))
      end do
      r = int(pqpingparams(c_loc(keywords), c_loc(values), int(expand_dbname, kind=c_int)))
    end function pingparams

    function connectdbparams(keywords, values, expand_dbname) result(conn)
      !! Makes a new connection to the database server.
      character(len=*), intent(inout), target :: keywords(:)
        !! Keyword string array
      character(len=*), intent(inout), target :: values(:)
        !! Value string array
      integer, intent(in) :: expand_dbname
        !! Flag (see documentation).
      type(pq) :: conn
        !! Database connection pointer.
      integer :: i
      do i = 1, size(keywords)
        keywords(i) = c_str(keywords(i))
        values(i) = c_str(values(i))
      end do
      conn%ptr = pqconnectdbparams(c_loc(keywords), c_loc(values), int(expand_dbname, kind=c_int))
    end function connectdbparams

    function connectdb(connin) result(conn)
      !! Makes a new connection to the database server.
      character(len=*), intent(in), optional :: connin
      character(len=:), allocatable :: connout
        !! Connection string.
      type(pq) :: conn
        !! Database connection pointer.
      if (.not. present(connin)) then
        connout = c_str("")
      else
        connout = c_str(connin)
      end if
      conn%ptr = pqconnectdb(connout)
    end function connectdb

    function setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) result(conn)
      !! Makes a new connection to the database server.
      character(len=*), intent(in) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      type(pq) :: conn
        !! Database connection pointer.
      conn%ptr = pqsetdblogin(c_str(pghost), c_str(pgport), c_str(pgoptions), c_str(pgtty), c_str(dbname), c_str(login), c_str(pwd))
    end function setdblogin

    subroutine finish(conn)
      !! Closes the connection to the server.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      call pqfinish(conn%ptr)
    end subroutine finish

    subroutine reset(conn)
      !! Resets the communication channel to the server.
      type(pq), intent(in) :: conn
        !! Database connection pointer.
      call pqreset(conn%ptr)
    end subroutine reset

end module fpq_connect
