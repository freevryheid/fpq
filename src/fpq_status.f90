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
  public :: serverversion
  public :: errormessage

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
    ! HERE
    function parameterstatus(pgconn, name) bind(c, name='PQparameterStatus') result(r)
      !! Looks up a current parameter setting of the server.
      import :: c_char, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: name
        !! Parameter name.
      ! type(c_ptr) :: r
      character(kind=c_char) :: r
    end function parameterstatus

    ! int PQprotocolVersion(const PGconn *conn);
    ! STILL TO DO

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

  ! contains

    ! function db(pgconn) result(r)
    !   !! Returns the database name of the connection.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Database name.
    !   type(c_ptr) :: ptr
    !   ptr = pqdb(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function db

    ! function user(pgconn) result(r)
    !   !! Returns the user name of the connection.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Database name.
    !   type(c_ptr) :: ptr
    !   ptr = pquser(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function user

    ! function pass(pgconn) result(r)
    !   !! Returns the password of the connection.
    !   type(c_ptr), intent(in) :: pgconn

    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Password.
    !   type(c_ptr) :: ptr
    !   ptr = pqpass(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function pass

    ! function host(pgconn) result(r)
    !   !! Returns the server host name of the active connection.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Hostname.
    !   type(c_ptr) :: ptr
    !   ptr = pqhost(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function host

    ! function hostaddr(pgconn) result(r)
    !   !! Returns the server IP address of the active connection.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Hostaddr.
    !   type(c_ptr) :: ptr
    !   ptr = pqhostaddr(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function hostaddr

    ! function port(pgconn) result(r)
    !   !! Returns the port of the active connection.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Port.
    !   type(c_ptr) :: ptr
    !   ptr = pqport(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function port

    ! ! function status(pgconn) result(r)
    ! !   !! Returns the status of the connection.
    ! !   type(c_ptr), intent(in) :: pgconn
    ! !     !! Database connection pointer.
    ! !   integer(kind=c_int) :: r
    ! !     !! Status.
    ! !   r = pqstatus(pgconn)
    ! ! end function status

    ! function serverversion(pgconn) result(r)
    !   !! Returns an integer representing the server version.
    !   type(c_ptr), intent(in) :: pgconn
    !     !! Database connection pointer.
    !   integer(kind=c_int) :: r
    !     !! Result by multiplying the server's major version number by 10000
    !     !! and adding the minor version number.
    !   r = pqserverversion(pgconn)
    ! end function serverversion

    ! function errormessage(pgconn) result(r)
    !   !! Returns the error message most recently generated by an operation on the connection.
    !   type(c_ptr), intent(in) :: pgconn

    !     !! Database connection pointer.
    !   character(len=:), allocatable :: r
    !     !! Error.
    !   type(c_ptr) :: ptr
    !   ptr = pqerrormessage(pgconn)
    !   if (c_associated(ptr)) then
    !     call c_f_str_ptr(ptr, r)
    !   end if
    ! end function errormessage

end module fpq_status

