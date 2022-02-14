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
    !! Connection success. The existence of these should never be relied upon.
    !! they should only be used for user feedback or similar purposes.
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

  ! PostgresPollingStatusType parameters
  ! TODO - perhaps move to fqp-status
  integer, parameter, public :: PGRES_POLLING_FAILED = 0
  integer, parameter, public :: PGRES_POLLING_READING = 1 !! These two indicate that one may use select before polling again.
  integer, parameter, public :: PGRES_POLLING_WRITING = 2
  integer, parameter, public :: PGRES_POLLING_OK = 3
  integer, parameter, public :: PGRES_POLLING_ACTIVE = 4  !! unused; keep for awhile for backwards compatibility

  ! [TODO - for document purposes, move this to a new module]
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

    ! PGconn *PQconnectStartParams(const char * const *keywords,
    !                              const char * const *values,
    !                              int expand_dbname);
    function pqconnectstartparams(keywords, values, expand_dbname) bind(c, name='PQconnectStartParams') result(pgconn)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: keywords, values
      integer(kind=c_int), intent(in) :: expand_dbname
      type(c_ptr) :: pgconn
    end function pqconnectstartparams

    ! PGconn *PQconnectStart(const char *conninfo);
    function pqconnectstart(conninfo) bind(c, name='PQconnectStart') result(pgconn)
      import :: c_char, c_ptr
      implicit none
      character(kind=c_char), intent(in) :: conninfo
      type(c_ptr) :: pgconn
    end function pqconnectdb

    ! PostgresPollingStatusType PQconnectPoll(PGconn *conn);
    ! STILL TO DO

    ! PQconninfoOption *PQconndefaults(void);
    !
    ! typedef struct
    ! {
    !     char   *keyword;   /* The keyword of the option */
    !     char   *envvar;    /* Fallback environment variable name */
    !     char   *compiled;  /* Fallback compiled in default value */
    !     char   *val;       /* Option's current value, or NULL */
    !     char   *label;     /* Label for field in connect dialog */
    !     char   *dispchar;  /* Indicates how to display this field
    !                           in a connect dialog. Values are:
    !                           ""        Display entered value as is
    !                           "*"       Password field - hide value
    !                           "D"       Debug option - don't show by default */
    !     int     dispsize;  /* Field size in characters for dialog */
    ! } PQconninfoOption;
    ! STILL TO DO

    ! PQconninfoOption *PQconninfo(PGconn *conn);
    ! STILL TO DO


    ! PQconninfoOption *PQconninfoParse(const char *conninfo, char **errmsg);
    ! STILL TO DO

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

    ! int PQresetStart(PGconn *conn);
    ! STILL TO DO

    ! PostgresPollingStatusType PQresetPoll(PGconn *conn);
    ! STILL TO DO

    ! PGPing PQpingParams(const char * const *keywords,
    !                     const char * const *values,
    !                     int expand_dbname);
    ! STILL TO DOi

    ! PGPing PQping(const char *conninfo);
    ! STILL TO DO

    ! void PQsetSSLKeyPassHook_OpenSSL(PQsslKeyPassHook_OpenSSL_type hook);
    ! STILL TO DO

    ! PQsslKeyPassHook_OpenSSL_type PQgetSSLKeyPassHook_OpenSSL(void);
    ! STILL TO DO

  end interface

  contains

    !! ## Database Connection Control Functions
    !! The following functions deal with making a connection to a
    !! [PostgreSQL](https://www.postgresql.org/)
    !! backend server. An application program can have several backend
    !! connections open at one time. (One reason to do that is to access more
    !! than one database.) Each connection is represented by a *PGconn* object,
    !! which is obtained from the function [[connectdb]]. Note that these functions
    !! will always return a non-null object pointer, unless perhaps there is too
    !! little memory even to allocate the *PGconn* object. The [[status]] function
    !! should be called to check the return value for a successful connection
    !! before queries are sent via the connection object.
    !!
    !! The functions [connectstartparams], [connectstart] and [connectpoll]
    !! are used to open a connection to a database
    !! server such that your application's thread of execution is
    !! not blocked on remote I/O whilst doing so.
    !! The point of this approach is that the waits for I/O to complete
    !! can occur in the application's main loop, rather than down
    !! inside PQconnectdbParams or PQconnectdb, and so the application
    !! can manage this operation in parallel with other activities.
    !!
    !! With [connectstartparams], the database connection is made using the
    !! parameters taken from the keywords and values arrays, and controlled
    !! by *expand_dbname*, as described for [connectdbparams].
    !!
    !! With [connectstart], the database connection is made using the
    !! parameters taken from the string conninfo as described above
    !! for [connectdb].
    !!
    !! Neither [connectstartparams] nor [connectstart] nor [connectpoll] will
    !! block, so long as a number of restrictions are met:
    !!    - The hostaddr parameter must be used appropriately to prevent DNS
    !! queries from being made. See the
    !! [documentation](https://www.postgresql.org/docs/9.5/libpq-connect.html#LIBPQ-PARAMKEYWORDS)
    !! of this parameter for details.
    !!    - If you call [trace], ensure that the stream object into which you
    !! trace will not block.
    !!    - You must ensure that the socket is in the appropriate state
    !! before calling [connectpoll], as described below.
    !!
    !! @Note Use of [connectstartparams] is analogous to [connectstart] shown below.
    !!
    !! To begin a nonblocking connection request, call
    !! [connectstart] or [connectstartparams]. If the result is null,
    !! then libpq has been unable to allocate a new *PGconn* structure.
    !! Otherwise, a valid *PGconn* pointer is returned
    !! (though not yet representing a valid connection to the database).
    !! Next call [status](conn). If the result is CONNECTION_BAD,
    !! the connection attempt has already failed, typically because of
    !! invalid connection parameters.
    !!
    !! If [connectstart] or [connectstartparams] succeeds,
    !! the next stage is to poll libpq so that it can proceed with the
    !! connection sequence. Use [socket](conn) to obtain the
    !! descriptor of the socket underlying the database connection.
    !! (Caution: do not assume that the socket remains the same
    !! across [connectpoll] calls.) Loop thus:
    !! If [connectpoll](conn) last returned PGRES_POLLING_READING,
    !! wait until the socket is ready to read
    !! (as indicated by select(), poll(), or similar system function).
    !! Then call [connectpoll](conn) again. Conversely, if
    !! PQconnectPoll(conn) last returned PGRES_POLLING_WRITING,
    !! wait until the socket is ready to write, then call
    !! [connectpoll](conn) again. On the first iteration, i.e.,
    !! if you have yet to call [connectpoll], behave as if it last
    !! returned PGRES_POLLING_WRITING. Continue this loop until
    !! [connectpoll](conn) returns PGRES_POLLING_FAILED,
    !! indicating the connection procedure has failed, or
    !! PGRES_POLLING_OK, indicating the connection has been successfully made.
    !!
    !! At any time during connection, the status of the connection
    !! can be checked by calling [status]. If this call returns
    !! CONNECTION_BAD, then the connection procedure has failed;
    !! if the call returns CONNECTION_OK, then the connection is ready.
    !! Both of these states are equally detectable from the return value
    !! of [connectpoll], described above. Other states might also occur
    !! during (and only during) an asynchronous connection procedure.
    !! These indicate the current stage of the connection procedure
    !! and might be useful to provide feedback to the user for example.
    !! These statuses are:

    function connectdbparams(keywords, values, expand_dbname) result(pgconn)
      !! @Bug FIXME - this uses 2 string arrays - imlementation appears buggy.
      !! Use [[dbconnect]] instead.

      !! Makes a new connection to the database server.
      !! This function opens a new database connection using the parameters
      !! taken from two string arrays.
      ! character(len=:), allocatable, intent(in) :: keywords(:)
      type(string_type), intent(in) :: keywords(0:)
        !! An array of strings, each one being a keyword. Unlike [[setdbLogin]]
        !! below, the parameter set can be extended without changing the
        !! function signature, so use of this function (or its nonblocking
        !! analogs [[connectstartparams]] and [[connectpoll]]) is preferred for
        !! new application programming.
        !! The currently recognized parameter keywords are listed
        !! [here](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PARAMKEYWORDS).
      ! character(len=:), allocatable, intent(in) :: values(:)
      type(string_type), intent(in) :: values(0:)
        !! Array of corresponding values for each keyword.
        !! The passed arrays can be empty to use all default parameters, or
        !! can contain one or more parameter settings. They must be matched in
        !! length. Processing will stop at the first NULL entry in the keywords
        !! array. Also, if the values entry associated with a non-NULL keywords
        !! entry is NULL or an empty string, that entry is ignored and
        !! processing continues with the next pair of array entries.
        !! In general the parameter arrays are processed from start to end.
        !! If any keyword is repeated, the last value (that is not NULL or
        !! empty) is used. This rule applies in particular when a keyword
        !! found in a connection string conflicts with one appearing in the
        !! keywords array. Thus, the programmer may determine whether array
        !! entries can override or be overridden by values taken from a
        !! connection string. Array entries appearing before an expanded
        !! *dbname* entry can be overridden by fields of the connection
        !! string, and in turn those fields are overridden by array entries
        !! appearing after *dbname* (but, again, only if those entries supply
        !! non-empty values). After processing all the array entries and any
        !! expanded connection string, any connection parameters that remain
        !! unset are filled with default values. If an unset parameter's
        !! corresponding [environment variable]
        !! (https://www.postgresql.org/docs/current/libpq-envars.html)
        !! is set, its value is used. If the environment variable is not set
        !! either, then the parameter's built-in default value is used.
      type(string_type), target :: keywords_strs(0:50) !  There are currently 38 possible keywords
      type(string_type), target :: values_strs(0:50)
      type(c_ptr), allocatable :: keywords_ptrs(:)
      type(c_ptr), allocatable :: values_ptrs(:)
      integer, intent(in) :: expand_dbname
        !! When *expand_dbname* is non-zero, the value for the first dbname
        !! keyword is checked to see if it is a connection string. If so, it is
        !! expanded into the individual connection parameters extracted from
        !! the string. The value is considered to be a connection string,
        !! rather than just a database name, if it contains an equal sign (=)
        !! or it begins with a URI scheme designator. (More details on
        !! connection string formats appear
        !! [here](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING).
        !! Only the first occurrence of *dbname* is treated in this way; any
        !! subsequent *dbname* parameter is processed as a plain database name.
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
      !! The prefered way to connect to a database server using these bindings.
      !! This function opens a new database connection using the parameters
      !! taken from the string *conninfo*. The passed string can be empty to
      !! use all default parameters, or it can contain one or more
      !! parameter settings separated by whitespace, or it can contain a URI.
      character(len=*), intent(in) :: conninfo
        !! See [connection strings]
        !! (https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING)
        !! for further information.
      type(c_ptr) :: conn
        !! Database connection pointer.
      conn = pqconnectdb(cstr(conninfo))
    end function connectdb

    function setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd) result(pgconn)
      !! Makes a new connection to the database server.
      !! This is the predecessor of [[connectdb]] with a fixed set of
      !! parameters. It has the same functionality except that the missing
      !! parameters will always take on default values.
      !! Write NULL or an empty string for any one of the fixed parameters
      !! that is to be defaulted. If the dbName contains an = sign or has a
      !! valid connection URI prefix, it is taken as a *conninfo* string in
      !! exactly the same way as if it had been passed to [[connectdb]], and
      !! the remaining parameters are then applied as specified for
      !! [[connectdbparams]].
      !! *pgtty* is no longer used and any value passed will be ignored.
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
      !! This is a macro that calls [[setdblogin]] with null pointers for the
      !! login and pwd parameters. It is provided for backward compatibility
      !! with very old programs.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
      character(len=*), intent(in) :: pghost
      character(len=*), intent(in) :: pgport
      character(len=*), intent(in) :: pgoptions
      character(len=*), intent(in) :: pgtty
      character(len=*), intent(in) :: dbname
      pgconn = pqsetdblogin(cstr(pghost), cstr(pgport), cstr(pgoptions), cstr(pgtty), cstr(dbname), cstr(""), cstr(""))
    end function setdb

    function connectstartparams(keywords, values, expand_dbname) result(pgconn)
      !! @Bug FIXME - this uses 2 string arrays - imlementation appears buggy.
      !! Use [[connectstart]] instead.

      !! Makes a new connection to the database server in a non-blocking manner.
      character(len=:), allocatable, intent(in) :: keywords(:)
        !! An array of strings, each one being a keyword. Unlike [[setdbLogin]]
        !! below, the parameter set can be extended without changing the
        !! function signature, so use of this function (or its nonblocking
        !! analogs [[connectstartparams]] and [[connectpoll]]) is preferred for
        !! new application programming.
        !! The currently recognized parameter keywords are listed
        !! [here](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PARAMKEYWORDS).
      character(len=:), allocatable, intent(in) :: values(:)
        !! Array of corresponding values for each keyword.
        !! The passed arrays can be empty to use all default parameters, or
        !! can contain one or more parameter settings. They must be matched in
        !! length. Processing will stop at the first NULL entry in the keywords
        !! array. Also, if the values entry associated with a non-NULL keywords
        !! entry is NULL or an empty string, that entry is ignored and
        !! processing continues with the next pair of array entries.
        !! In general the parameter arrays are processed from start to end.
        !! If any keyword is repeated, the last value (that is not NULL or
        !! empty) is used. This rule applies in particular when a keyword
        !! found in a connection string conflicts with one appearing in the
        !! keywords array. Thus, the programmer may determine whether array
        !! entries can override or be overridden by values taken from a
        !! connection string. Array entries appearing before an expanded
        !! *dbname* entry can be overridden by fields of the connection
        !! string, and in turn those fields are overridden by array entries
        !! appearing after *dbname* (but, again, only if those entries supply
        !! non-empty values). After processing all the array entries and any
        !! expanded connection string, any connection parameters that remain
        !! unset are filled with default values. If an unset parameter's
        !! corresponding [environment variable]
        !! (https://www.postgresql.org/docs/current/libpq-envars.html)
        !! is set, its value is used. If the environment variable is not set
        !! either, then the parameter's built-in default value is used.
      character, target :: keywords_strs(50)  ! There are currently 38 possible keywords
      character, target :: values_strs(50)
      type(c_ptr), allocatable :: keywords_ptrs(:)
      type(c_ptr), allocatable :: values_ptrs(:)
      integer, intent(in) :: expand_dbname
        !! When *expand_dbname* is non-zero, the value for the first dbname
        !! keyword is checked to see if it is a connection string. If so, it is
        !! expanded into the individual connection parameters extracted from
        !! the string. The value is considered to be a connection string,
        !! rather than just a database name, if it contains an equal sign (=)
        !! or it begins with a URI scheme designator. (More details on
        !! connection string formats appear
        !! [here](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING).
        !! Only the first occurrence of *dbname* is treated in this way; any
        !! subsequent *dbname* parameter is processed as a plain database name.
      type(c_ptr) :: pgconn
        !! Database connection pointer.
      integer i, ksize, vsize
      pgconn = c_null_ptr
      if (allocated(keywords)) then
        ksize = size(keywords, 1)
      end if
      if (allocated(values)) then
        vsize = size(values, 1)
      end if
      if (ksize /= vsize) return
      allocate(keywords_ptrs(ksize))
      allocate(values_ptrs(ksize))
      do i = 1 , ksize
        keywords_strs(i) = cstr(keywords(i))
        keywords_ptrs(i) = c_loc(keywords_strs(i))
        values_strs(i) = cstr(values(i))
        values_ptrs(i) = c_loc(values_strs(i))
      end do
      pgconn = pqconnectdbparams(keywords_ptrs, values_ptrs, int(expand_dbname, kind=c_int))
      deallocate(keywords_ptrs)
      deallocate(values_ptrs)
    end function connectstartparams

    function connectstart(conninfo) result(conn)
      !! Makes a new non-blocking connection to the database server.
      character(len=*), intent(in) :: conninfo
        !! See [connection strings]
        !! (https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING)
        !! for further information.
      type(c_ptr) :: conn
        !! Database connection pointer.
      conn = pqconnectstart(cstr(conninfo))
    end function connectstart


! PostgresPollingStatusType PQconnectPoll(PGconn *conn);


! PQconndefaults
! Returns the default connection options.

! PQconninfoOption *PQconndefaults(void);

! typedef struct
! {
!     char   *keyword;   /* The keyword of the option */
!     char   *envvar;    /* Fallback environment variable name */
!     char   *compiled;  /* Fallback compiled in default value */
!     char   *val;       /* Option's current value, or NULL */
!     char   *label;     /* Label for field in connect dialog */
!     char   *dispchar;  /* Indicates how to display this field
!                           in a connect dialog. Values are:
!                           ""        Display entered value as is
!                           "*"       Password field - hide value
!                           "D"       Debug option - don't show by default */
!     int     dispsize;  /* Field size in characters for dialog */
! } PQconninfoOption;
! Returns a connection options array. This can be used to determine all possible PQconnectdb options and their current default values. The return value points to an array of PQconninfoOption structures, which ends with an entry having a null keyword pointer. The null pointer is returned if memory could not be allocated. Note that the current default values (val fields) will depend on environment variables and other context. A missing or invalid service file will be silently ignored. Callers must treat the connection options data as read-only.

! After processing the options array, free it by passing it to PQconninfoFree. If this is not done, a small amount of memory is leaked for each call to PQconndefaults.

! PQconninfo
! Returns the connection options used by a live connection.

! PQconninfoOption *PQconninfo(PGconn *conn);
! Returns a connection options array. This can be used to determine all possible PQconnectdb options and the values that were used to connect to the server. The return value points to an array of PQconninfoOption structures, which ends with an entry having a null keyword pointer. All notes above for PQconndefaults also apply to the result of PQconninfo.

! PQconninfoParse
! Returns parsed connection options from the provided connection string.

! PQconninfoOption *PQconninfoParse(const char *conninfo, char **errmsg);
! Parses a connection string and returns the resulting options as an array; or returns NULL if there is a problem with the connection string. This function can be used to extract the PQconnectdb options in the provided connection string. The return value points to an array of PQconninfoOption structures, which ends with an entry having a null keyword pointer.

! All legal options will be present in the result array, but the PQconninfoOption for any option not present in the connection string will have val set to NULL; default values are not inserted.

! If errmsg is not NULL, then *errmsg is set to NULL on success, else to a malloc'd error string explaining the problem. (It is also possible for *errmsg to be set to NULL and the function to return NULL; this indicates an out-of-memory condition.)

! After processing the options array, free it by passing it to PQconninfoFree. If this is not done, some memory is leaked for each call to PQconninfoParse. Conversely, if an error occurs and errmsg is not NULL, be sure to free the error string using PQfreemem.


    subroutine finish(conn)
      !! ## Closes the connection to the server. Also frees memory used by the conn object.
      !! Note that even if the server connection attempt fails (as indicated by status),
      !! the application should call finish to free the memory used by the conn object.
      !! The conn pointer must not be used again after finish has been called.

      type(c_ptr), intent(in) :: conn
      ! integer, pointer, intent(in) :: conn
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

! PQresetStart
! PQresetPoll
! Reset the communication channel to the server, in a nonblocking manner.

! int PQresetStart(PGconn *conn);

! PostgresPollingStatusType PQresetPoll(PGconn *conn);
! These functions will close the connection to the server and attempt to establish a new connection, using all the same parameters previously used. This can be useful for error recovery if a working connection is lost. They differ from PQreset (above) in that they act in a nonblocking manner. These functions suffer from the same restrictions as PQconnectStartParams, PQconnectStart and PQconnectPoll.

! To initiate a connection reset, call PQresetStart. If it returns 0, the reset has failed. If it returns 1, poll the reset using PQresetPoll in exactly the same way as you would create the connection using PQconnectPoll.

! PQpingParams
! PQpingParams reports the status of the server. It accepts connection parameters identical to those of PQconnectdbParams, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.

! PGPing PQpingParams(const char * const *keywords,
!                     const char * const *values,
!                     int expand_dbname);
! The function returns one of the following values:

! PQping
! PQping reports the status of the server. It accepts connection parameters identical to those of PQconnectdb, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.

! PGPing PQping(const char *conninfo);
! The return values are the same as for PQpingParams.

! PQsetSSLKeyPassHook_OpenSSL
! PQsetSSLKeyPassHook_OpenSSL lets an application override libpq's default handling of encrypted client certificate key files using sslpassword or interactive prompting.

! void PQsetSSLKeyPassHook_OpenSSL(PQsslKeyPassHook_OpenSSL_type hook);
! The application passes a pointer to a callback function with signature:

! int callback_fn(char *buf, int size, PGconn *conn);
! which libpq will then call instead of its default PQdefaultSSLKeyPassHook_OpenSSL handler. The callback should determine the password for the key and copy it to result-buffer buf of size size. The string in buf must be null-terminated. The callback must return the length of the password stored in buf excluding the null terminator. On failure, the callback should set buf[0] = '\0' and return 0. See PQdefaultSSLKeyPassHook_OpenSSL in libpq's source code for an example.

! If the user specified an explicit key location, its path will be in conn->sslkey when the callback is invoked. This will be empty if the default key path is being used. For keys that are engine specifiers, it is up to engine implementations whether they use the OpenSSL password callback or define their own handling.

! The app callback may choose to delegate unhandled cases to PQdefaultSSLKeyPassHook_OpenSSL, or call it first and try something else if it returns 0, or completely override it.

! The callback must not escape normal flow control with exceptions, longjmp(...), etc. It must return normally.

! PQgetSSLKeyPassHook_OpenSSL
! PQgetSSLKeyPassHook_OpenSSL returns the current client certificate key password hook, or NULL if none has been set.

! PQsslKeyPassHook_OpenSSL_type PQgetSSLKeyPassHook_OpenSSL(void);



end module fpq_connect

