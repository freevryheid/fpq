module fpq_connect
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  integer, parameter, public :: CONNECTION_OK = 0
    !! Connection success.
  integer, parameter, public :: CONNECTION_BAD = 1
    !! Connection failed, typically because of invalid connection parameters.
  integer, parameter, public ::  CONNECTION_STARTED = 2
    !! Waiting for connection to be made.
  integer, parameter, public ::  CONNECTION_MADE = 3
    !! Connection OK; waiting to send.
  integer, parameter, public ::  CONNECTION_AWAITING_RESPONSE = 4
    !! Waiting for a response from the server.
  integer, parameter, public ::  CONNECTION_AUTH_OK = 5
    !! Received authentication; waiting for backend start-up to finish.
  integer, parameter, public ::  CONNECTION_SETENV = 6

  integer, parameter, public ::  CONNECTION_SSL_STARTUP = 7

  integer, parameter, public ::  CONNECTION_NEEDED = 8

  integer, parameter, public ::  CONNECTION_CHECK_WRITABLE = 9

  integer, parameter, public ::  CONNECTION_CONSUME = 10

  integer, parameter, public ::  CONNECTION_GSS_STARTUP = 11

  integer, parameter, public ::  CONNECTION_CHECK_TARGET = 12

  public :: connectdb, finish, reset

  interface

    ! PGconn *PQconnectdb(const char *conninfo);
    function pqconnectdb(conninfo) bind(c, name='PQconnectdb') result(pgconn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      type(c_ptr) :: pgconn
    end function pqconnectdb

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

    !! # Database Connection Control Functions
    !! The following functions deal with making a connection to a PostgreSQL backend server.
    !! An application program can have several backend connections open at one time.
    !! (One reason to do that is to access more than one database.) Each connection is
    !! represented by a conn object, which is obtained from the function connectdb.
    !! Note that these functions will always return a
    !! non-null object pointer, unless perhaps there is too little memory even to allocate
    !! the conn object. The status function should be called to check the return value
    !! for a successful connection before queries are sent via the connection object.

    function connectdb(conninfo) result(conn)
      !! ## Makes a new connection to the database server.
      !! This function opens a new database connection using the parameters taken from the string conninfo.
      !! The passed string can be empty to use all default parameters, or it can contain one or more
      !! parameter settings separated by whitespace, or it can contain a URI.
      character(len=*), intent(in) :: conninfo
        !! See [connection strings](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) for further information.
      type(c_ptr) :: conn
        !! Database connection pointer.
      conn = pqconnectdb(cstr(conninfo))
    end function connectdb

    subroutine finish(conn)
      !! ## Closes the connection to the server. Also frees memory used by the conn object.
      !! Note that even if the server connection attempt fails (as indicated by status),
      !! the application should call finish to free the memory used by the conn object.
      !! The conn pointer must not be used again after finish has been called.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      call pqfinish(conn)
    end subroutine finish

    subroutine reset(conn)
      !! ## Resets the communication channel to the server.
      !! This function will close the connection to the server and attempt to establish a new connection,
      !! using all the same parameters previously used.
      !! This might be useful for error recovery if a working connection is lost.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      call pqreset(conn)
    end subroutine reset

end module fpq_connect

