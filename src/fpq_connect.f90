module fpq_connect
  use, intrinsic :: iso_c_binding
  use stdlib_string_type
  use fpq_common
  implicit none
  private

  ! TODO
  ! keywords.dim[0].ubound may be used uninitialized

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

  public :: connectdbparams
  public :: connectdb
  public :: setdblogin
  public :: setdb
  public :: finish
  public :: reset
  public :: ping

  interface

    ! PGconn *PQconnectdbParams(const char * const *keywords, const char * const *values, int expand_dbname);
    function pqconnectdbparams(keywords, values, expand_dbname) bind(c, name='PQconnectdbParams') result(pgconn)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords, values
      integer(kind=c_int), intent(in) :: expand_dbname
      type(c_ptr) :: pgconn
    end function pqconnectdbparams

    ! PGconn *PQconnectdb(const char *conninfo);
    function pqconnectdb(conninfo) bind(c, name='PQconnectdb') result(pgconn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      type(c_ptr) :: pgconn
    end function pqconnectdb

    ! PGconn *PQsetdbLogin(const char *pghost,
    !                      const char *pgport,
    !                      const char *pgoptions,
    !                      const char *pgtty,
    !                      const char *dbName,
    !                      const char *login,
    !                      const char *pwd);
    function pqsetdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) bind(c, name='PQsetdbLogin') result(pgconn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      type(c_ptr) :: pgconn
    end function pqsetdblogin

    ! void PQfinish(PGconn *conn);
    subroutine pqfinish(pgconn) bind(c, name='PQfinish')
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
    end subroutine pqfinish

    ! void PQreset(PGconn *conn);
    subroutine pqreset(conn) bind(c, name='PQreset')
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
    end subroutine pqreset

    ! PGPing PQpingParams(const char * const *keywords,
    !                     const char * const *values,
    !                     int expand_dbname);
    ! STILL TO DO - FIX 2 string array issue first

    ! PGPing PQping(const char *conninfo);
    ! STILL TO DO
    function pqping(conninfo) bind(c, name='PQping') result(r)
      import :: c_char, c_int
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      integer(kind=c_int) :: r
    end function pqping

  end interface

  contains

    !! ## Database Connection Control Functions
    !! [Documentation.](https://www.postgresql.org/docs/current/libpq-connect.html)

    function connectdbparams(keywords, values, expand_dbname) result(pgconn)
      !! @Bug This uses 2 string arrays - imlementation appears buggy.
      !! Use [[connectdb]] instead.

      !! Makes a new connection to the database server.
      ! character(len=:), allocatable, intent(in) :: keywords(:)
      type(string_type), intent(in) :: keywords(0:)
        !! An array of type string_type referencing parameter keywords.
      ! character(len=:), allocatable, intent(in) :: values(:)
      type(string_type), intent(in) :: values(0:)
        !! Array of corresponding values for each keyword.
      type(string_type), target :: keywords_strs(0:50) !  There are currently 38 possible keywords
      type(string_type), target :: values_strs(0:50)
      type(c_ptr), allocatable :: keywords_ptrs(:)
      type(c_ptr), allocatable :: values_ptrs(:)
      integer, intent(in) :: expand_dbname
        !! Flag.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
      integer i, ksize, vsize
      pgconn = c_null_ptr
      ksize = size(keywords, 1)
      vsize = size(values, 1)
      if (ksize /= vsize) return
      allocate(keywords_ptrs(0:ksize))
      allocate(values_ptrs(0:ksize))
      do i = 0, ksize-1
        keywords_strs(i) = dstr(keywords(i))
        keywords_ptrs(i) = c_loc(keywords_strs(i))
        values_strs(i) = dstr(values(i))
        values_ptrs(i) = c_loc(values_strs(i))
      end do
      pgconn = pqconnectdbparams(keywords_ptrs, values_ptrs, int(expand_dbname, kind=c_int))
      deallocate(keywords_ptrs)
      deallocate(values_ptrs)
    end function connectdbparams

    function connectdb(conninfo) result(conn)
      !! Makes a new connection to the database server.
      character(len=*), intent(in) :: conninfo
        !! Connection string.
      type(c_ptr) :: conn
        !! Database connection pointer.
      conn = pqconnectdb(cstr(conninfo))
    end function connectdb

    function setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) result(pgconn)
      !! Makes a new connection to the database server.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
      character(len=*), intent(in) :: pghost
      character(len=*), intent(in) :: pgport
      character(len=*), intent(in) :: pgoptions
      character(len=*), intent(in) :: pgtty
      character(len=*), intent(in) :: dbname
      character(len=*), intent(in) :: login
      character(len=*), intent(in) :: pwd
      pgconn = pqsetdblogin(cstr(pghost), cstr(pgport), cstr(pgoptions), cstr(pgtty), cstr(dbname), cstr(login), cstr(pwd))
    end function setdblogin

    function setdb(pghost, pgport, pgoptions, pgtty, dbname) result(pgconn)
      !! Makes a new connection to the database server.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
      character(len=*), intent(in) :: pghost
      character(len=*), intent(in) :: pgport
      character(len=*), intent(in) :: pgoptions
      character(len=*), intent(in) :: pgtty
      character(len=*), intent(in) :: dbname
      pgconn = pqsetdblogin(cstr(pghost), cstr(pgport), cstr(pgoptions), cstr(pgtty), cstr(dbname), cstr(""), cstr(""))
    end function setdb

    subroutine finish(pgconn)
      !! Closes the connection to the server.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      call pqfinish(pgconn)
    end subroutine finish

    subroutine reset(pgconn)
      !! Resets the communication channel to the server.
      type(c_ptr), intent(in) :: pgconn
        !! Database connection pointer.
      call pqreset(pgconn)
    end subroutine reset

! PQpingParams
! PQpingParams reports the status of the server. It accepts connection parameters identical to those of PQconnectdbParams, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.

! PGPing PQpingParams(const char * const *keywords,
!                     const char * const *values,
!                     int expand_dbname);
! The function returns one of the following values:

! PQping
! PQping reports the status of the server. It accepts connection parameters identical to those of PQconnectdb, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.

    function ping(conninfo) result(r)
      !! Returns the status of the server.
      character(len=*), intent(in) :: conninfo
        !! Connection string.
      integer(kind=c_int) :: r
        !! Ping parameter.
      r = pqping(conninfo)
    end function ping


end module fpq_connect

