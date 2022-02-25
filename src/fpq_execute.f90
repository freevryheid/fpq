module fpq_execute

  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

  ! Result variables
  integer(kind=c_int), parameter, public :: PGRES_EMPTY_QUERY = 0
    !! The string sent to the server was empty.
  integer(kind=c_int), parameter, public :: PGRES_COMMAND_OK = 1
    !! Successful completion of a command returning no data.
  integer(kind=c_int), parameter, public :: PGRES_TUPLES_OK = 2
    !! Successful completion of a command returning data (such as a SELECT or SHOW).
  integer(kind=c_int), parameter, public :: PGRES_COPY_OUT = 3
    !! Copy Out (from server) data transfer started.
  integer(kind=c_int), parameter, public :: PGRES_COPY_IN = 4
    !! Copy In (to server) data transfer started.
  integer(kind=c_int), parameter, public :: PGRES_BAD_RESPONSE = 5
    !! The server's response was not understood.
  integer(kind=c_int), parameter, public :: PGRES_NONFATAL_ERROR = 6
    !! A nonfatal error (a notice or warning) occurred.
  integer(kind=c_int), parameter, public :: PGRES_FATAL_ERROR = 7
    !! A fatal error occurred.
  integer(kind=c_int), parameter, public :: PGRES_COPY_BOTH = 8
    !! Copy In/Out (to and from server) data transfer started. This feature is currently used only for streaming replication, so this status should not occur in ordinary applications.
  integer(kind=c_int), parameter, public :: PGRES_SINGLE_TUPLE = 9
    !! The PGresult contains a single result tuple from the current command. This status occurs only when single-row mode has been selected for the query (see Section 34.6).
  character(kind=c_char), parameter, public :: PG_DIAG_SEVERITY	= 'S'
    !! The severity; the field contents are ERROR, FATAL, or PANIC
    !! (in an error message), or WARNING, NOTICE, DEBUG, INFO, or LOG
    !! (in a notice message), or a localized translation of one of these.
    !! Always present.
  character(kind=c_char), parameter, public :: PG_DIAG_SEVERITY_NONLOCALIZED = 'V'

  character(kind=c_char), parameter, public :: PG_DIAG_SQLSTATE	= 'C'

  character(kind=c_char), parameter, public :: PG_DIAG_MESSAGE_PRIMARY = 'M'

  character(kind=c_char), parameter, public :: PG_DIAG_MESSAGE_DETAIL = 'D'

  character(kind=c_char), parameter, public :: PG_DIAG_MESSAGE_HINT = 'H'

  character(kind=c_char), parameter, public :: PG_DIAG_STATEMENT_POSITION = 'P'

  character(kind=c_char), parameter, public :: PG_DIAG_INTERNAL_POSITION = 'p'

  character(kind=c_char), parameter, public :: PG_DIAG_INTERNAL_QUERY = 'q'

  character(kind=c_char), parameter, public :: PG_DIAG_CONTEXTi = 'W'

  character(kind=c_char), parameter, public :: PG_DIAG_SCHEMA_NAME = 's'

  character(kind=c_char), parameter, public :: PG_DIAG_TABLE_NAME = 't'

  character(kind=c_char), parameter, public :: PG_DIAG_COLUMN_NAME = 'c'

  character(kind=c_char), parameter, public :: PG_DIAG_DATATYPE_NAME = 'd'

  character(kind=c_char), parameter, public :: PG_DIAG_CONSTRAINT_NAME = 'n'

  character(kind=c_char), parameter, public :: PG_DIAG_SOURCE_FILE = 'F'

  character(kind=c_char), parameter, public :: PG_DIAG_SOURCE_LINE = 'L'

  character(kind=c_char), parameter, public :: PG_DIAG_SOURCE_FUNCTION = 'R'


  public :: pqexec
  public :: pqresultstatus
  public :: pqclear
  public :: pqexecparams
  public :: pqprepare
  public :: pqexecprepared
  public :: pqdescribeprepared
  public :: pqresstatus
  public :: pqresulterrormessage
  public :: pqresulterrorfield
  public :: pqntuples
  public :: pqnfields
  public :: pqfname
  public :: pqfnumber
  public :: pqftablecol
  public :: pqfformat
  ! public :: fmod
  ! public :: fsize
  public :: pqbinarytuples
  public :: pqgetvalue
  public :: pqgetisnull
  public :: pqgetlength

  ! public :: nparams

  interface

    ! PGresult *PQexec(PGconn *conn, const char *command);
    function pqexec(pgconn, command) bind(c, name='PQexec') result(pgresult)
      !! Submits a command to the server and waits for the result.
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: command
        !! The command string can include multiple SQL commands
        !! (separated by semicolons).
      type(c_ptr) :: pgresult
        !! Result pointer.
    end function pqexec

    ! PGresult *PQexecParams(PGconn *conn,
    !                        const char *command,
    !                        int nParams,
    !                        const Oid *paramTypes,
    !                        const char * const *paramValues,
    !                        const int *paramLengths,
    !                        const int *paramFormats,
    !                        int resultFormat);
    function pqexecparams(pgconn, command, nparams, paramtypes, paramvalues, paramlengths, &
      paramformats, resultformat) bind(c, name='PQexecParams') result(pgresult)
      !! Submits a command to the server and waits for the result.
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
      type(c_ptr) :: pgresult
        !! Result pointer.
    end function pqexecparams

    ! PGresult *PQprepare(PGconn *conn,
    !                  const char *stmtName,
    !                  const char *query,
    !                  int nParams,
    !                  const Oid *paramTypes);
    function pqprepare(pgconn, stmtname, query, nparams, paramtypes) bind(c, name='PQprepare') result(pgresult)
      !! Submits a request to create a prepared statement with the given parameters,
      !! and waits for completion.
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
      type(c_ptr) :: pgresult
        !! Result pointer.
    end function pqprepare

    ! PGresult *PQexecPrepared(PGconn *conn,
    !                          const char *stmtName,
    !                          int nParams,
    !                          const char * const *paramValues,
    !                          const int *paramLengths,
    !                          const int *paramFormats,
    !                          int resultFormat);
    function pqexecprepared(pgconn, stmtname, nparams, paramvalues, paramlengths, &
      paramformats, resultformat) bind(c, name='PQexecPrepared') result(pgresult)
      !! Sends a request to execute a prepared statement with given parameters,
      !! and waits for the result.
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: stmtname
        !! Statement name - as indicated in [[prepare]].
      integer(kind=c_int), intent(in), value :: nparams
        !! The number of parameters supplied.
        !! The array pointers can be NULL when *nparams* is zero.
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
      type(c_ptr) :: pgresult
        !! Result pointer.
    end function pqexecprepared

    ! PGresult *PQdescribePrepared(PGconn *conn, const char *stmtName);
    function pqdescribeprepared(pgconn, stmtname) bind(c, name='PQdescribePrepared') result(pgresult)
      !! Submits a request to obtain information about the specified prepared statement,
      !! and waits for completion.
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: stmtname
      type(c_ptr) :: pgresult
        !! The functions [[pqnparams]] and [[pqparamtype]] can be applied to this *pgresult* to obtain
        !! information about the parameters of the prepared statement,
        !! and the functions [[pqnfields]], [[pqfname]], [[pqftype]], etc provide information about
        !! the result columns (if any) of the statement.
    end function pqdescribeprepared

    ! PGresult *PQdescribePortal(PGconn *conn, const char *portalName);
    function pqdescribeportal(pgconn, portalname) bind(c, name='PQdescribePortal') result(pgresult)
      !! Submits a request to obtain information about the specified portal,
      !! and waits for completion.
      ! FIXME - don't use portals - not sure howto test.
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in), value :: pgconn
      character(kind=c_char), intent(in) :: portalname
      type(c_ptr) :: pgresult
        !! The functions [[pqnparams]] and [[pqparamtype]] can be applied to this *pgresult* to obtain
        !! information about the parameters of the prepared statement,
        !! and the functions [[pqnfields]], [[pqfname]], [[pqftype]], etc provide information about
        !! the result columns (if any) of the statement.
    end function pqdescribeportal

    ! ExecStatusType PQresultStatus(const PGresult *res);
    function pqresultstatus(pgresult) bind(c, name='PQresultStatus') result(r)
      !! Returns the result status of the command.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqresultstatus

    ! char *PQresStatus(ExecStatusType status);
    function pqresstatus(status) bind(c, name='PQresStatus') result(r)
      !! Converts the enumerated type returned by PQresultStatus into a string constant
      !! describing the status code. The caller should not free the result.
      import :: c_ptr, c_int
      implicit none
      integer(kind=c_int), intent(in), value :: status
      type(c_ptr) :: r
    end function pqresstatus

    ! char *PQresultErrorMessage(const PGresult *res);
    function pqresulterrormessage(pgresult) bind(c, name='PQresultErrorMessage') result(r)
      !! Returns the error message associated with the command,
      !! or an empty string if there was no error.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      type(c_ptr) :: r
        !! If there was an error, the returned string will include a trailing newline.
        !! The caller should not free the result directly. It will be freed when the associated
        !! *pgresult* handle is passed to [[pqclear]].
    end function pqresulterrormessage

    ! ! char *PQresultVerboseErrorMessage(const PGresult *res,
    ! !                                   PGVerbosity verbosity,
    ! !                                   PGContextVisibility show_context);
    ! ! STILL TO DO

    ! char *PQresultErrorField(const PGresult *res, int fieldcode);
    function pqresulterrorfield(pgresult, fieldcode) bind(c, name='PQresultErrorField') result(r)
      !! Returns an individual field of an error report.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in) :: fieldcode
        !! fieldcode is an error field identifier.
      type(c_ptr) :: r
        !! NULL is returned if the *pgresult* is not an error or warning result,
        !! or does not include the specified field.
        !! Field values will normally not include a trailing newline.
        !! The caller should not free the result directly.
        !! It will be freed when the associated PGresult handle is passed to [[pqclear]].
    end function pqresulterrorfield

    ! void PQclear(PGresult *res);
    subroutine pqclear(pgresult) bind(c, name='PQclear')
      !! Frees the storage associated with a PGresult.
      !! Every command result should be freed via [[clear]] when it is no longer needed.
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgresult
    end subroutine pqclear

    ! int PQntuples(const PGresult *res);
    function pqntuples(pgresult) bind(c, name='PQntuples') result(r)
      !! Returns the number of rows (tuples) in the query result.
      !! (Note that *pgresult* objects are limited to no more than INT_MAX rows,
      !! so an int result is sufficient.)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqntuples

    ! int PQnfields(const PGresult *res);
    function pqnfields(pgresult) bind(c, name='PQnfields') result(r)
      !! Returns the number of columns (fields) in each row of the query result.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqnfields

    ! char *PQfname(const PGresult *res, int column_number);
    function pqfname(pgresult, column_number) bind(c, name='PQfname') result(r)
      !! Returns the column name associated with the given column number.
      !! Column numbers start at 0. The caller should not free the result directly.
      !! It will be freed when the associated *pgresult* handle is passed to [[pqclear]].
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number
      type(c_ptr) :: r
    end function pqfname

    ! int PQfnumber(const PGresult *res, const char *column_name);
    function pqfnumber(pgresult, column_name) bind(c, name='PQfnumber') result(r)
      !! Returns the column number associated with the given column name.
      !! Column numbers start at 0.
      !! -1 is returned if the given name does not match any column.
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      character(kind=c_char), intent(in) :: column_name
      integer(kind=c_int) :: r
    end function pqfnumber

    ! ! Oid PQftable(const PGresult *res, int column_number);
    !! Returns the OID of the table from which the given column was fetched.
    !! Column numbers start at 0.
    ! ! STILL TO DO

    ! int PQftablecol(const PGresult *res, int column_number);
    function pqftablecol(pgresult, column_number) bind(c, name='PQftablecol') result(r)
      !! Returns the column number (within its table) of the column making up the specified
      !! query result column. Query-result column numbers start at 0, but table columns
      !! have nonzero numbers.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number
      integer(kind=c_int) :: r
        !! Zero is returned if the column number is out of range, or if the specified
        !! column is not a simple reference to a table column.
    end function pqftablecol

    ! int PQfformat(const PGresult *res, int column_number);
    function pqfformat(pgresult, column_number) bind(c, name='PQfformat') result(r)
      !! Returns the format code indicating the format of the given column.
      !! Column numbers start at 0.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number
      integer(kind=c_int) :: r
        !! Format code zero indicates textual data representation,
        !! while format code one indicates binary representation.
    end function pqfformat

    ! ! Oid PQftype(const PGresult *res, int column_number);
    !   !! Returns the data type associated with the given column number. The integer returned is the internal OID number of the type.
    !   !! Column numbers start at 0.
    ! ! STILL TO DO

    ! ! int PQfmod(const PGresult *res, int column_number);
    ! function pqfmod(pgresult, column_number) bind(c, name='PQfmod') result(r)
    !   !! Returns the type modifier of the column associated with the given column number.
    !   !! Column numbers start at 0.
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int), intent(in) :: column_number
    !   integer(kind=c_int) :: r
    !     !! Most data types do not use modifiers, in which case the value is always -1.
    ! end function pqfmod

    ! int PQfsize(const PGresult *res, int column_number);
    function pqfsize(pgresult, column_number) bind(c, name='PQfsize') result(r)
      !! Returns the size in bytes of the column associated with the given column number.
      !! Column numbers start at 0.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in) :: column_number
      integer(kind=c_int) :: r
        !! Returns the space allocated for this column in a database row,
        !! in other words the size of the server's internal representation of the data type.
        !! (Accordingly, it is not really very useful to clients.)
        !! A negative value indicates the data type is variable-length.
    end function pqfsize

    ! int PQbinaryTuples(const PGresult *res);
    function pqbinarytuples(pgresult) bind(c, name='PQbinaryTuples') result(r)
      !! Returns 1 if the *pgresult* contains binary data and 0 if it contains text data.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function PQbinarytuples

    ! char *PQgetvalue(const PGresult *res, int row_number, int column_number);
    function pqgetvalue(pgresult, row_number, column_number) bind(c, name='PQgetvalue') result(r)
      !! Returns a single field value of one row of a PGresult.
      !! Row and column numbers start at 0. The caller should not free the result directly.
      !! It will be freed when the associated *pgresult* handle is passed to [[pqclear]].
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number, row_number
      type(c_ptr) :: r
        !! For data in text format, the value returned by PQgetvalue is a null-terminated
        !! character string representation of the field value. For data in binary format,
        !! the value is in the binary representation determined by the data type's typsend
        !! and typreceive functions. (The value is actually followed by a zero byte in this
        !! case too, but that is not ordinarily useful, since the value is likely to contain
        !! embedded nulls.)
        !! An empty string is returned if the field value is null.
        !! See [[pqgetisnull]] to distinguish null values from empty-string values.
    end function pqgetvalue

    ! int PQgetisnull(const PGresult *res, int row_number, int column_number);
    function pqgetisnull(pgresult, row_number, column_number) bind(c, name='PQgetisnull') result(r)
      !! Tests a field for a null value. Row and column numbers start at 0.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number, row_number
      integer(kind=c_int) :: r
        !! This function returns 1 if the field is null and 0 if it contains a non-null value.
        !! (Note that [[pqgetvalue]] will return an empty string, not a null pointer, for a null field.)
    end function pqgetisnull

    ! int PQgetlength(const PGresult *res, int row_number, int column_number);
    function pqgetlength(pgresult, row_number, column_number) bind(c, name='PQgetlength') result(r)
      !! Returns the actual length of a field value in bytes. Row and column numbers start at 0.
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in), value :: column_number, row_number
      integer(kind=c_int) :: r
        !! This is the actual data length for the particular data value, that is,
        !! the size of the object pointed to by [[pqgetvalue]].
        !! For text data format this is the same as strlen().
        !! For binary format this is essential information.
        !! Note that one should not rely on [[pqfsize]] to obtain the actual data length.
    end function pqgetlength


    ! STILL TO DO

    ! ! int PQnparams(const PGresult *res);
    ! function PQnparams(pgresult) bind(c, name='PQnparams') result(r)
    !   !! Returns the number of parameters of a prepared statement.
    !   import :: c_ptr, c_int
    !   implicit none
    !   type(c_ptr), intent(in), value :: pgresult
    !   integer(kind=c_int) :: r
    ! end function PQnparams

    ! ! Oid PQparamtype(const PGresult *res, int param_number);
    !   !! Returns the data type of the indicated statement parameter. Parameter numbers start at 0.
    ! ! STILL TO DO

    ! ! void PQprint(FILE *fout,      /* output stream */
    ! !              const PGresult *res,
    ! !              const PQprintOpt *po);
    !    !! Prints out all the rows and, optionally, the column names to the specified output stream.
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
    !   !! Returns the command status tag from the SQL command that generated the PGresult.
    ! ! STILL TO DO

    ! ! char *PQcmdTuples(PGresult *res);i
    !   !! Returns the number of rows affected by the SQL command.
    ! ! STILL TO DO

    ! ! Oid PQoidValue(const PGresult *res);
    !   !! Returns the OID of the inserted row
    ! ! STILL TO DO

    ! ! char *PQoidStatus(const PGresult *res);
    !   !! This function is deprecated
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

end module fpq_execute


