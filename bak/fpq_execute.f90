module fpq_execute
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Result variables
  integer, parameter, public :: PGRES_EMPTY_QUERY = 0
    !! The string sent to the server was empty.
  integer, parameter, public :: PGRES_COMMAND_OK = 1
    !! Successful completion of a command returning no data.
  integer, parameter, public :: PGRES_TUPLES_OK = 2
    !! Successful completion of a command returning data (such as a SELECT or SHOW).
  integer, parameter, public :: PGRES_COPY_OUT = 3
    !! Copy Out (from server) data transfer started.
  integer, parameter, public :: PGRES_COPY_IN = 4
    !! Copy In (to server) data transfer started.
  integer, parameter, public :: PGRES_BAD_RESPONSE = 5
    !! The server's response was not understood.
  integer, parameter, public :: PGRES_NONFATAL_ERROR = 6
    !! A nonfatal error (a notice or warning) occurred.
  integer, parameter, public :: PGRES_FATAL_ERROR = 7
    !! A fatal error occurred.
  integer, parameter, public :: PGRES_COPY_BOTH = 8
    !! Copy In/Out (to and from server) data transfer started. This feature is currently used only for streaming replication, so this status should not occur in ordinary applications.
  integer, parameter, public :: PGRES_SINGLE_TUPLE = 9
    !! The PGresult contains a single result tuple from the current command. This status occurs only when single-row mode has been selected for the query (see Section 34.6).

  public :: exec
  ! public :: resultstatus
  ! public :: resstatus
  ! public :: resulterrormessage
  ! public :: resulterrorfield
  ! public :: clear
  ! public :: ntuples
  ! public :: nfields
  ! public :: fname
  ! public :: fnumber
  ! public :: ftablecol
  ! public :: fformat
  ! public :: fmod
  ! public :: fsize
  ! public :: binarytuples
  ! public :: nparams
  ! public :: describeprepared

  interface

    ! PGresult *PQexec(PGconn *conn, const char *command);
    function exec(pgconn, command) bind(c, name='PQexec') result(pgresult)
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in) :: pgconn
      character(kind=c_char), intent(in) :: command
      type(c_ptr) :: pgresult
    end function exec

    ! PGresult *PQexecParams(PGconn *conn,
    !                        const char *command,
    !                        int nParams,
    !                        const Oid *paramTypes,
    !                        const char * const *paramValues,
    !                        const int *paramLengths,
    !                        const int *paramFormats,
    !                        int resultFormat);
    ! STILL TO DO

    ! PGresult *PQprepare(PGconn *conn,
    !                  const char *stmtName,
    !                  const char *query,
    !                  int nParams,
    !                  const Oid *paramTypes);
    ! STILL TO DO

    ! PGresult *PQexecPrepared(PGconn *conn,
    !                          const char *stmtName,
    !                          int nParams,
    !                          const char * const *paramValues,
    !                          const int *paramLengths,
    !                          const int *paramFormats,
    !                          int resultFormat);
    ! STILL TO DO

    ! ! PGresult *PQdescribePrepared(PGconn *conn, const char *stmtName);
    ! function pqdescribeprepared(conn, stmtname) bind(c, name='PQdescribePrepared') result(pgresult)
    !   import :: c_ptr, c_char
    !   implicit none
    !   type(c_ptr), intent(in) :: conn
    !   character(kind=c_char), intent(in) :: stmtname
    !   type(c_ptr) :: pgresult
    ! end function PQdescribePrepared

    ! ! PGresult *PQdescribePortal(PGconn *conn, const char *portalName);
    ! ! STILL TO DO

    ! ! ExecStatusType PQresultStatus(const PGresult *res);
    ! function pqresultstatus(pgresult) bind(c, name='PQresultStatus') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function pqresultstatus

    ! ! char *PQresStatus(ExecStatusType status);
    ! function pqresstatus(status) bind(c, name='PQresStatus') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   integer(kind=c_int), intent(in) :: status
    !   type(c_ptr) :: r
    ! end function pqresstatus

    ! ! char *PQresultErrorMessage(const PGresult *res);
    ! function pqresulterrormessage(pgresult) bind(c, name='PQresultErrorMessage') result(r)
    !   import :: c_ptr
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   type(c_ptr) :: r
    ! end function pqresulterrormessage

    ! ! char *PQresultVerboseErrorMessage(const PGresult *res,
    ! !                                   PGVerbosity verbosity,
    ! !                                   PGContextVisibility show_context);
    ! ! STILL TO DO

    ! ! char *PQresultErrorField(const PGresult *res, int fieldcode);
    ! function pqresulterrorfield(pgresult, fieldcode) bind(c, name='PQresultErrorField') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: fieldcode
    !   type(c_ptr) :: r
    ! end function pqresulterrorfield

    ! ! void PQclear(PGresult *res);
    ! subroutine pqclear(pgresult) bind(c, name='PQclear')
    !   import :: c_ptr
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    ! end subroutine pqclear

    ! ! int PQntuples(const PGresult *res);
    ! function pqntuples(pgresult) bind(c, name='PQntuples') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function pqntuples

    ! ! int PQnfields(const PGresult *res);
    ! function pqnfields(pgresult) bind(c, name='PQnfields') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function pqnfields

    ! ! char *PQfname(const PGresult *res, int column_number);
    ! function pqfname(pgresult, column_number) bind(c, name='PQfname') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   type(c_ptr) :: r
    ! end function pqfname

    ! ! int PQfnumber(const PGresult *res, const char *column_name);
    ! function pqfnumber(pgresult, column_name) bind(c, name='PQfnumber') result(r)
    !   import :: c_ptr, c_char, c_int
    !   implicit none
    !   type(c_ptr), intent(in) :: pgresult
    !   character(kind=c_char), intent(in) :: column_name
    !   integer(kind=c_int) :: r
    ! end function pqfnumber

    ! ! Oid PQftable(const PGresult *res,
    ! !              int column_number);
    ! ! STILL TO DO

    ! ! int PQftablecol(const PGresult *res, int column_number);
    ! function pqftablecol(pgresult, column_number) bind(c, name='PQftablecol') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   integer(kind=c_int) :: r
    ! end function pqftablecol

    ! ! int PQfformat(const PGresult *res, int column_number);
    ! function pqfformat(pgresult, column_number) bind(c, name='PQfformat') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   integer(kind=c_int) :: r
    ! end function pqfformat

    ! ! Oid PQftype(const PGresult *res,
    ! !         int column_number);
    ! ! STILL TO DO

    ! ! int PQfmod(const PGresult *res, int column_number);
    ! function pqfmod(pgresult, column_number) bind(c, name='PQfmod') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   integer(kind=c_int) :: r
    ! end function pqfmod

    ! ! int PQfsize(const PGresult *res, int column_number);
    ! function pqfsize(pgresult, column_number) bind(c, name='PQfsize') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   integer(kind=c_int) :: r
    ! end function pqfsize

    ! ! int PQbinaryTuples(const PGresult *res);
    ! function PQbinarytuples(pgresult) bind(c, name='PQbinaryTuples') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function PQbinarytuples

    ! ! char *PQgetvalue(const PGresult *res,
    ! !                  int row_number,
    ! !                  int column_number);
    ! ! STILL TO DO

    ! ! int PQgetisnull(const PGresult *res,
    ! !                 int row_number,
    ! !                 int column_number);
    ! ! STILL TO DO

    ! ! int PQgetlength(const PGresult *res,
    ! !                 int row_number,
    ! !                 int column_number);
    ! ! STILL TO DO

    ! ! int PQnparams(const PGresult *res);
    ! function PQnparams(pgresult) bind(c, name='PQnparams') result(r)
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function PQnparams

    ! ! Oid PQparamtype(const PGresult *res, int param_number);
    ! ! STILL TO DO

    ! ! void PQprint(FILE *fout,      /* output stream */
    ! !              const PGresult *res,
    ! !              const PQprintOpt *po);
    ! ! typedef struct
    ! ! {
    ! !     pqbool  header;      /* print output field headings and row count */
    ! !     pqbool  align;       /* fill align the fields */
    ! !     pqbool  standard;    /* old brain dead format */
    ! !     pqbool  html3;       /* output HTML tables */
    ! !     pqbool  expanded;    /* expand tables */
    ! !     pqbool  pager;       /* use pager for output if needed */
    ! !     char    *fieldSep;   /* field separator */
    ! !     char    *tableOpt;   /* attributes for HTML table element */
    ! !     char    *caption;    /* HTML table caption */
    ! !     char    **fieldName; /* null-terminated array of replacement field names */
    ! ! } PQprintOpt;
    ! ! STILL TO DO

    ! ! char *PQcmdStatus(PGresult *res);
    ! ! STILL TO DO

    ! ! char *PQcmdTuples(PGresult *res);
    ! ! STILL TO DO

    ! ! Oid PQoidValue(const PGresult *res);
    ! ! STILL TO DO

    ! ! char *PQoidStatus(const PGresult *res);
    ! ! STILL TO DO

    ! ! char *PQescapeLiteral(PGconn *conn, const char *str, size_t length);
    ! ! STILL TO DO

    ! ! char *PQescapeIdentifier(PGconn *conn, const char *str, size_t length);
    ! ! STILL TO DO

    ! ! size_t PQescapeStringConn(PGconn *conn,
    ! !                           char *to, const char *from, size_t length,
    ! !                           int *error);
    ! ! STILL TO DO

    ! ! size_t PQescapeString (char *to, const char *from, size_t length);
    ! ! STILL TO DO

    ! ! unsigned char *PQescapeByteaConn(PGconn *conn,
    ! !                              const unsigned char *from,
    ! !                              size_t from_length,
    ! !                              size_t *to_length);
    ! ! STILL TO DO

    ! ! unsigned char *PQescapeBytea(const unsigned char *from,
    ! !                           size_t from_length,
    ! !                           size_t *to_length);
    ! ! STILL TO DO

    ! ! unsigned char *PQunescapeBytea(const unsigned char *from, size_t *to_length);
    ! ! STILL TO DO

  end interface

  ! contains

  !   function exec(conn, command) result(r)
  !     !! Submits a command to the server and waits for the result.
  !     type(c_ptr), intent(in) :: conn
  !       !! Database connection pointer.
  !     character(len=*), intent(in) :: command
  !       !! The command string can include multiple SQL commands (separated by semicolons).
  !       !! Multiple queries sent in a single PQexec call are processed in a single transaction,
  !       !! unless there are explicit BEGIN/COMMIT commands included in the query string to divide
  !       !! it into multiple transactions. (See Section 53.2.2.1 for more details about how the
  !       !! server handles multi-query strings.)
  !     type(c_ptr) :: r
  !       !! Note however that the returned PGresult structure describes only the result of the last
  !       !! command executed from the string. Should one of the commands fail, processing of the
  !       !! string stops with it and the returned PGresult describes the error condition.
  !     r = pqexec(conn, cstr(command))
  !   end function exec

  !   function describeprepared(conn, stmtname) result(r)
  !     !! Submits a request to obtain information about the specified prepared statement,
  !     !! and waits for completion.
  !     type(c_ptr), intent(in) :: conn
  !       !! Database connection pointer.
  !     character(len=*), intent(in) :: stmtname
  !       !! stmtName can be "" or NULL to reference the unnamed statement,
  !       !! otherwise it must be the name of an existing prepared statement.
  !     type(c_ptr) :: r
  !       !! On success, a PGresult with status PGRES_COMMAND_OK is returned.
  !       !! The functions PQnparams and PQparamtype can be applied to this PGresult
  !       !! to obtain information about the parameters of the prepared statement,
  !       !! and the functions PQnfields, PQfname, PQftype, etc provide information
  !       !! about the result columns (if any) of the statement.
  !     r = pqdescribeprepared(conn, cstr(stmtname))
  !   end function describeprepared

  !   function resultstatus(pgresult) result(r)
  !     !! Returns the result status of the command.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: r
  !       !! PQresultStatus can return one of the following values:[TODO]
  !     r = pqresultstatus(pgresult)
  !   end function resultstatus

  !   function resstatus(status) result(r)
  !     !! Converts the enumerated type returned by PQresultStatus into a string constant describing the status code.
  !     !! The caller should not free the result.
  !     integer(kind=c_int), intent(in) :: status
  !       !! Integer returned by resultstatus.
  !     character(len=:), allocatable :: r
  !       !! String constant describing the submitted code.
  !     type(c_ptr) :: ptr
  !     ptr = pqresstatus(status)
  !     if (c_associated(ptr)) then
  !       call c_f_str_ptr(ptr, r)
  !     end if
  !   end function resstatus

  !   function resulterrormessage(pgresult) result(r)
  !     !! Returns the error message associated with the command, or an empty string if there was no error.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     character(len=:), allocatable :: r
  !       !! If there was an error, the returned string will include a trailing newline. i
  !       !! The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
  !       !! Immediately following a PQexec or PQgetResult call, PQerrorMessage (on the connection) will return the same string as
  !       !! PQresultErrorMessage (on the result). However, a PGresult will retain its error message until destroyed, whereas the
  !       !! connection's error message will change when subsequent operations are done. Use PQresultErrorMessage when you want to
  !       !! know the status associated with a particular PGresult; use PQerrorMessage when you want to know the status from
  !       !! the latest operation on the connection.
  !     type(c_ptr) :: ptr
  !     ptr = pqresulterrormessage(pgresult)
  !     if (c_associated(ptr)) then
  !       call c_f_str_ptr(ptr, r)
  !     end if
  !   end function resulterrormessage

  !   function resulterrorfield(pgresult, fieldcode) result(r)
  !     !! Returns an individual field of an error report.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: fieldcode
  !       !! An error field identifier; see the symbols listed below.[todo]
  !     character(len=:), allocatable :: r
  !       !! NULL is returned if the PGresult is not an error or warning result, or does not include the specified field.
  !       !! Field values will normally not include a trailing newline. The caller should not free the result directly.
  !       !! It will be freed when the associated PGresult handle is passed to PQclear.
  !     type(c_ptr) :: ptr
  !     ptr = pqresulterrorfield(pgresult, fieldcode)
  !     if (c_associated(ptr)) then
  !       call c_f_str_ptr(ptr, r)
  !     end if
  !   end function resulterrorfield

  !   subroutine clear(pgresult)
  !     !! Frees the storage associated with a PGresult.
  !     !! Every command result should be freed via PQclear when it is no longer needed.
  !     !! You can keep a PGresult object around for as long as you need it; it does not go away when you issue a new command,
  !     !! nor even if you close the connection. To get rid of it, you must call PQclear. Failure to do this will result in
  !     !! memory leaks in your application.
  !     type(c_ptr), intent(in) :: pgresult
  !     call pqclear(pgresult)
  !   end subroutine clear

  !   function ntuples(pgresult) result(r)
  !     !! Returns the number of rows (tuples) in the query result. (Note that PGresult objects are limited to no more
  !     !! than INT_MAX rows, so an int result is sufficient.)
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: r
  !       !! Number of rows (tuples) in the query result.
  !     r = pqntuples(pgresult)
  !   end function ntuples

  !   function nfields(pgresult) result(r)
  !     !! Returns the number of columns (fields) in each row of the query result.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: r
  !       !! Number of fields.
  !     r = pqnfields(pgresult)
  !   end function nfields

  !   function fname(pgresult, column_number) result(r)
  !     !! Returns the column name associated with the given column number.
  !     !! The caller should not free the result directly.
  !     !! It will be freed when the associated PGresult handle is passed to PQclear.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: column_number
  !       !! P!! Column numbers start at 0.
  !     character(len=:), allocatable :: r
  !       !! NULL is returned if the PGresult is not an error or warning result, or does not include the specified field.
  !       !! Field values will normally not include a trailing newline. The caller should not free the result directly.
  !       !! It will be freed when the associated PGresult handle is passed to PQclear.
  !     type(c_ptr) :: ptr
  !     ptr = pqfname(pgresult, column_number)
  !     if (c_associated(ptr)) then
  !       call c_f_str_ptr(ptr, r)
  !     end if
  !   end function fname

  !   function fnumber(pgresult, column_name) result(r)
  !     !! Returns the column number associated with the given column name.
  !     type(c_ptr), intent(in) :: pgresult
  !       !! PGresult pointer.
  !     character(len=*), intent(in) :: column_name
  !       !! The given name is treated like an identifier in an SQL command, that is, it is downcased unless double-quoted.
  !     integer(kind=c_int) :: r
  !       !! -1 is returned if the given name does not match any column.
  !     r = pqfnumber(pgresult, cstr(column_name))
  !   end function fnumber

  !   function ftablecol(pgresult, column_number) result(r)
  !     !! Returns the column number (within its table) of the column making up the specified query result column.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int), intent(in) :: column_number
  !       !! Query-result column numbers start at 0, but table columns have nonzero numbers.
  !     integer(kind=c_int) :: r
  !       !! Zero is returned if the column number is out of range, or if the specified column is not a simple reference to a table column.
  !     r = pqftablecol(pgresult, column_number)
  !   end function ftablecol

  !   function fformat(pgresult, column_number) result(r)
  !     !! Returns the format code indicating the format of the given column.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int), intent(in) :: column_number
  !       !! Query-result column numbers start at 0, but table columns have nonzero numbers.
  !     integer(kind=c_int) :: r
  !       !! Format code zero indicates textual data representation, while format code one indicates binary representation.
  !     r = pqfformat(pgresult, column_number)
  !   end function fformat

  !   function fmod(pgresult, column_number) result(r)
  !     !! Returns the type modifier of the column associated with the given column number.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int), intent(in) :: column_number
  !       !! Query-result column numbers start at 0, but table columns have nonzero numbers.
  !     integer(kind=c_int) :: r
  !       !! The interpretation of modifier values is type-specific; they typically indicate precision or size limits.
  !       !! The value -1 is used to indicate no information available.
  !       !! Most data types do not use modifiers, in which case the value is always -1.
  !     r = pqfmod(pgresult, column_number)
  !   end function fmod

  !   function fsize(pgresult, column_number) result(r)
  !     !! Returns the size in bytes of the column associated with the given column number.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int), intent(in) :: column_number
  !       !! Query-result column numbers start at 0, but table columns have nonzero numbers.
  !     integer(kind=c_int) :: r
  !       !! Returns the space allocated for this column in a database row, in other words
  !       !! the size of the server's internal representation of the data type.
  !       !! (Accordingly, it is not really very useful to clients.)
  !       !! A negative value indicates the data type is variable-length.
  !     r = pqfsize(pgresult, column_number)
  !   end function fsize

  !   function binarytuples(pgresult) result(r)
  !     !! Returns 1 if the PGresult contains binary data and 0 if it contains text data.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: r
  !       !! This function is deprecated (except for its use in connection with COPY),
  !       !! because it is possible for a single PGresult to contain text data in some columns
  !       !! and binary data in others. PQfformat is preferred.
  !       !! PQbinaryTuples returns 1 only if all columns of the result are binary (format 1).
  !     r = pqbinarytuples(pgresult)
  !   end function binarytuples

  !   function nparams(pgresult) result(r)
  !     !! Returns the number of parameters of a prepared statement.
  !     type(c_ptr), intent(in), value :: pgresult
  !       !! PGresult pointer.
  !     integer(kind=c_int) :: r
  !       !! This function is only useful when inspecting the result of PQdescribePrepared.
  !       !! For other types of queries it will return zero.
  !     r = pqnparams(pgresult)
  !   end function nparams


end module fpq_execute


