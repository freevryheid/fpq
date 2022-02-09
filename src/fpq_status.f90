module fpq_status
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private
  public :: db, user, pass, host, hostaddr, port, status, serverversion, errormessage

  interface

    ! char *PQdb(const PGconn *conn);
    function pqdb(conn) bind(c, name='PQdb') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqdb

    ! char *PQuser(const PGconn *conn);
    function pquser(conn) bind(c, name='PQuser') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pquser

    ! char *PQpass(const PGconn *conn);
    function pqpass(conn) bind(c, name='PQpass') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqpass

    ! char *PQhost(const PGconn *conn);
    function pqhost(conn) bind(c, name='PQhost') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqhost

    ! char *PQhostaddr(const PGconn *conn);
    function pqhostaddr(conn) bind(c, name='PQhostaddr') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqhostaddr

    ! char *PQport(const PGconn *conn);
    function pqport(conn) bind(c, name='PQport') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
      type(c_ptr) :: r
    end function pqport

    ! char *PQtty(const PGconn *conn);
    ! STILL TO DO

    ! char *PQoptions(const PGconn *conn);
    ! STILL TO DO

    ! ConnStatusType PQstatus(const PGconn *conn);
    function pqstatus(conn) bind(c, name='PQstatus') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: conn
      integer(kind=c_int) :: r
    end function pqstatus

    ! PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
    ! STILL TO DO

    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! STILL TO DO

    ! int PQprotocolVersion(const PGconn *conn);
    ! STILL TO DO

    ! int PQserverVersion(const PGconn *conn);
    function pqserverversion(conn) bind(c, name='PQserverVersion') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: conn
      integer(kind=c_int) :: r
    end function pqserverversion

    ! char *PQerrorMessage(const PGconn *conn);
    function pqerrormessage(conn) bind(c, name='PQerrorMessage') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: conn
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
    !! These functions can be used to interrogate the status of an existing database connection object.

    function db(conn) result(r)
      !! Returns the database name of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pqdb(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function db

    function user(conn) result(r)
      !! Returns the user name of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Database name.
      type(c_ptr) :: ptr
      ptr = pquser(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function user

    function pass(conn) result(r)
      !! Returns the password of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! This will return either the password specified in the connection parameters,
        !! or if there was none and the password was obtained from the password file,
        !! it will return that. In the latter case, if multiple hosts were specified in
        !! the connection parameters, it is not possible to rely on the result of PQpass
        !! until the connection is established. The status of the connection can be checked
        !! using the function status.[TODO: add link]
      type(c_ptr) :: ptr
      ptr = pqpass(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function pass

    function host(conn) result(r)
      !! Returns the server host name of the active connection.
      !! This can be a host name, an IP address, or a directory path if the connection is via Unix socket.
      !! (The path case can be distinguished because it will always be an absolute path, beginning with /.)
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! If the connection parameters specified both host and hostaddr, then host will return the host information.
        !! If only hostaddr was specified, then that is returned. If multiple hosts were specified in the connection
        !! parameters, host returns the host actually connected to.
        !! host returns NULL if the conn argument is NULL. Otherwise, if there is an error producing the host information
        !! (perhaps if the connection has not been fully established or there was an error), it returns an empty string.
        !! If multiple hosts were specified in the connection parameters, it is not possible to rely on the result of host
        !! until the connection is established. The status of the connection can be checked using the function status.
      type(c_ptr) :: ptr
      ptr = pqhost(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function host

    function hostaddr(conn) result(r)
      !! Returns the server IP address of the active connection.
      !! This can be the address that a host name resolved to, or an IP address provided through the hostaddr parameter.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! hostaddr returns NULL if the conn argument is NULL. Otherwise, if there is an error producing the host information
        !! (perhaps if the connection has not been fully established or there was an error), it returns an empty string.
      type(c_ptr) :: ptr
      ptr = pqhostaddr(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function hostaddr

    function port(conn) result(r)
      !! Returns the port of the active connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! If multiple ports were specified in the connection parameters, port returns the port actually connected to.
        !! port returns NULL if the conn argument is NULL. Otherwise, if there is an error producing the port information (perhaps if the connection has not been fully established or there was an error), it returns an empty string.
        !! If multiple ports were specified in the connection parameters, it is not possible to rely on the result of PQport until the connection is established. The status of the connection can be checked using the function status.
      type(c_ptr) :: ptr
      ptr = pqport(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function port

    function status(conn) result(r)
      !! Returns the status of the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      integer(kind=c_int) :: r
        !! The status can be one of a number of values. However, only two of these are seen outside of an asynchronous connection procedure: CONNECTION_OK and CONNECTION_BAD.
        !! A good connection to the database has the status CONNECTION_OK. A failed connection attempt is signaled by status CONNECTION_BAD.
        !! Ordinarily, an OK status will remain so until finish[TODO], but a communications failure might result in the status changing to CONNECTION_BAD prematurely.
        !! In that case the application could try to recover by calling reset[TODO].
        !! See the entry for PQconnectStartParams, PQconnectStart and PQconnectPoll with regards to other status codes that might be returned.[TODO]
      r = pqstatus(conn)
    end function status

    function serverversion(conn) result(r)
      !! Returns an integer representing the server version.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      integer(kind=c_int) :: r
        !! Applications might use this function to determine the version of the database server they are connected to. The result is formed by multiplying the server's major version number by 10000 and adding the minor version number. For example, version 10.1 will be returned as 100001, and version 11.0 will be returned as 110000. Zero is returned if the connection is bad.
        !! Prior to major version 10, PostgreSQL used three-part version numbers in which the first two parts together represented the major version. For those versions, PQserverVersion uses two digits for each part; for example version 9.1.5 will be returned as 90105, and version 9.2.0 will be returned as 90200.
        !! Therefore, for purposes of determining feature compatibility, applications should divide the result of PQserverVersion by 100 not 10000 to determine a logical major version number. In all release series, only the last two digits differ between minor releases (bug-fix releases).
      r = pqserverversion(conn)
    end function serverversion

    function errormessage(conn) result(r)
      !! Returns the error message most recently generated by an operation on the connection.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=:), allocatable :: r
        !! Nearly all libpq functions will set a message for PQerrorMessage if they fail. Note that by libpq convention, a nonempty PQerrorMessage result can consist of multiple lines, and will include a trailing newline. The caller should not free the result directly. It will be freed when the associated PGconn handle is passed to PQfinish.
        !! The result string should not be expected to remain the same across operations on the PGconn structure.
      type(c_ptr) :: ptr
      ptr = pqerrormessage(conn)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function errormessage




end module fpq_status

