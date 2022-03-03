module async

  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  public :: pqsendquery
  public :: pqsendqueryparams
  public :: pqsendprepare
  public :: pqsendqueryprepared
  public :: pqsenddescribeprepared
  public :: pqsenddescribeportal
  public :: pqgetresult
  public :: pqconsumeinput
  public :: pqisbusy
  public :: pqsetnonblocking
  public :: pqisnonblocking
  public :: pqflush
  public :: pqsetsinglerowmode

  interface

    ! int PQsendQuery(PGconn *conn, const char *command);
    function pqsendquery(pgconn, command) bind(c, name='PQsendQuery') result(r)
      !! Submits a command to the server without waiting for the result(s).
      import :: c_ptr, c_charm c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: command
      integer(kind=c_int) :: r
        !! 1 is returned if the command was successfully dispatched and 0 if not
        !! (in which case, use [[pqerrormessage]] to get more information about the failure).
    end function pqsendquery

    ! int PQsendQueryParams(PGconn *conn,
    !                       const char *command,
    !                       int nParams,
    !                       const Oid *paramTypes,
    !                       const char * const *paramValues,
    !                       const int *paramLengths,
    !                       const int *paramFormats,
    !                       int resultFormat);
    function pqsendqueryparams(pgconn, command, nparams, paramtypes, paramvalues, paramlengths, &
      paramformats, resultformat) bind(c, name='PQsendQueryParams') result(r)
      !! Submits a command and separate parameters to the server without waiting for the result(s).
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: command
        !! The command string.
      integer(kind=c_int), intent(in), value :: nparams
        !! The number of parameters supplied.
        !! The array pointers can be NULL when *nparams* is zero.
      type(c_ptr), intent(in) :: paramtypes(:)
        !! By OID (tedious - see documentation).
      type(c_ptr), intent(in) :: paramvalues(:)
        !!  Parameter values.
      type(c_ptr), intent(in) :: paramlengths(:)
        !! Data lengths of binary-format parameters.
        !! The array pointer can be null when there are no binary parameters.
      type(c_ptr), intent(in) :: paramformats(:)
        !! Specifies whether parameters are text
        !! (put a zero in the array entry for the corresponding parameter)
        !! or binary (put a one in the array entry for the corresponding parameter).
        !! If the array pointer is null then all parameters are presumed to be text strings.
      integer(kind=c_int), intent(in), value :: resultformat
        !! Specify zero to obtain results in text format,
        !! or one to obtain results in binary format.
      integer(kind=c_int) :: r
    end function pqsendqueryparams

    ! int PQsendPrepare(PGconn *conn,
    !                   const char *stmtName,
    !                   const char *query,
    !                   int nParams,
    !                   const Oid *paramTypes);
    function pqsendprepare(pgconn, stmtname, query, nparams, paramtypes) bind(c, name='PQsendPrepare') result(r)
      !! Sends a request to create a prepared statement with the given parameters,
      !! without waiting for completion.
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: stmtname
        !! Statement name - can be "" for unnamed statement.The command string.d
      character(kind=c_char), intent(in) :: query
        !! The command string.
      integer(kind=c_int), intent(in), value :: nparams
        !! The number of parameters supplied.
        !! The array pointers can be NULL when *nparams* is zero.
      type(c_ptr), intent(in) :: paramtypes(:)
        !! By OID (tedious - see documentation).
      integer(kind=c_int) :: r
    end function pqsendprepare

    ! int PQsendQueryPrepared(PGconn *conn,
    !                         const char *stmtName,
    !                         int nParams,
    !                         const char * const *paramValues,
    !                         const int *paramLengths,
    !                         const int *paramFormats,
    !                         int resultFormat);
    function pqsendqueryprepared(pgconn, stmtname, nparams, paramtypes, paramvalues, paramlengths, &
      paramformats, resultformat) bind(c, name='PQsendQueryPrepared') result(r)
      !! Submits a command and separate parameters to the server without waiting for the result(s).
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: stmtname
        !! The command string.
      integer(kind=c_int), intent(in), value :: nparams
        !! The number of parameters supplied.
        !! The array pointers can be NULL when *nparams* is zero.
      type(c_ptr), intent(in) :: paramtypes(:)
        !! By OID (tedious - see documentation).
      type(c_ptr), intent(in) :: paramvalues(:)
        !!  Parameter values.
      type(c_ptr), intent(in) :: paramlengths(:)
        !! Data lengths of binary-format parameters.
        !! The array pointer can be null when there are no binary parameters.
      type(c_ptr), intent(in) :: paramformats(:)
        !! Specifies whether parameters are text
        !! (put a zero in the array entry for the corresponding parameter)
        !! or binary (put a one in the array entry for the corresponding parameter).
        !! If the array pointer is null then all parameters are presumed to be text strings.
      integer(kind=c_int), intent(in), value :: resultformat
        !! Specify zero to obtain results in text format,
        !! or one to obtain results in binary format.
      integer(kind=c_int) :: r
    end function pqsendqueryprepared

    ! int PQsendDescribePrepared(PGconn *conn, const char *stmtName);
    function pqsenddescribeprepared(pgconn, stmtname) bind(c, name='PQsendDescribePrepared') result(r)
      !! Submits a request to obtain information about the specified prepared statement,
      !! without waiting for completion.
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: stmtname
      integer(kind=c_int) :: r
        !! This is an asynchronous version of PQdescribePrepared:
        !! it returns 1 if it was able to dispatch the request, and 0 if not.
        !! After a successful call, call [[pqgetresult]] to obtain the results.
        !! The function's parameters are handled identically to [[pqdescribeprepared]].
    end function pqsenddescribeprepared

    ! int PQsendDescribePortal(PGconn *conn, const char *portalName);
    function pqsenddescribeportal(pgconn, portalname) bind(c, name='PQsendDescribePortal') result(r)
      !! Submits a request to obtain information about the specified portal,
      !! without waiting for completion.
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: portalname
      integer(kind=c_int) :: r
    end function pqsenddescribeportal

    ! PGresult *PQgetResult(PGconn *conn);
    function pqgetresult(pgconn, portalname) bind(c, name='PQgetResult') result(pgresult)
      !! Waits for the next result from a prior [[pqsendquery]], [[pqsendqueryparams]],
      !! [[pqsendprepare]], [[pqsendqueryprepared]], [[pqsenddescribeprepared]], or [[pqsenddescribeportal]] call,
      !! and returns it.
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: portalname
      type(c_ptr) :: pgresult
        !! A null pointer is returned when the command is complete and
        !! there will be no more results.
    end function pqgetresult

    ! int PQconsumeInput(PGconn *conn);
    function pqconsumeinput(pgconn) bind(c, name='PQconsumeInput') result(r)
      !! If input is available from the server, consume it.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqconsumeinput

    ! int PQisBusy(PGconn *conn);
    function pqisbusy(pgconn) bind(c, name='PQisBusy') result(r)
      !! Returns 1 if a command is busy, that is,
      !! [[pqgetresult]] would block waiting for input. a 0 return indicates
      !! that [[pqgetresult]] can be called with assurance of not blocking.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqisbusy

    ! int PQsetnonblocking(PGconn *conn, int arg);
    function pqsetnonblocking(pgconn, arg) bind(c, name='PQsetnonblocking') result(r)
      !! Sets the nonblocking status of the connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int), intent(in), value :: arg
      integer(kind=c_int) :: r
    end function pqisbusy

    ! int PQisnonblocking(const PGconn *conn);
    function pqisnonblocking(pgconn) bind(c, name='PQisnonblocking') result(r)
      !! Sets the nonblocking status of the connection.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: pgconn
      integer(kind=c_int) :: r
    end function pqisnonblocking

    ! int PQflush(PGconn *conn);
    function pqflush(pgconn) bind(c, name='PQflush') result(r)
      !! Attempts to flush any queued output data to the server.
      !! Returns 0 if successful (or if the send queue is empty), -1 if it failed for some reason,
      !! or 1 if it was unable to send all the data in the send queue yet
      !! (this case can only occur if the connection is nonblocking).
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int) :: r
    end function pqflush

    ! int PQsetSingleRowMode(PGconn *conn);
    function pqsetsinglerowmode(pgconn, arg) bind(c, name='PQsetSingleRowMode') result(r)
      !! Select single-row mode for the currently-executing query.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      integer(kind=c_int), intent(in), value :: arg
      integer(kind=c_int) :: r
    end function pqsetsinglerowmode

  end interface

end module async

