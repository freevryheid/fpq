module fpq_execute
  use, intrinsic :: iso_c_binding
  use fpq_common
  implicit none
  private

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

  public :: exec, resultstatus, resstatus, resulterrormessage, resulterrorfield, clear, ntuples, nfields, fname, fnumber

  interface

    ! PGresult *PQexec(PGconn *conn, const char *command);
    function pqexec(conn, command) bind(c, name='PQexec') result(pgresult)
      import :: c_ptr, c_char
      implicit none
      type(c_ptr), intent(in) :: conn
      character(kind=c_char), intent(in) :: command
      type(c_ptr) :: pgresult
    end function pqexec

    ! ExecStatusType PQresultStatus(const PGresult *res);
    function pqresultstatus(pgresult) bind(c, name='PQresultStatus') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqresultstatus

    ! char *PQresStatus(ExecStatusType status);
    function pqresstatus(status) bind(c, name='PQresStatus') result(r)
      import :: c_ptr, c_int
      implicit none
      integer(kind=c_int), intent(in) :: status
      type(c_ptr) :: r
    end function pqresstatus

    ! char *PQresultErrorMessage(const PGresult *res);
    function pqresulterrormessage(pgresult) bind(c, name='PQresultErrorMessage') result(r)
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      type(c_ptr) :: r
    end function pqresulterrormessage

    ! char *PQresultErrorField(const PGresult *res, int fieldcode);
    function pqresulterrorfield(pgresult, fieldcode) bind(c, name='PQresultErrorField') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in) :: fieldcode
      type(c_ptr) :: r
    end function pqresulterrorfield

    ! void PQclear(PGresult *res);
    subroutine pqclear(pgresult) bind(c, name='PQclear')
      import :: c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgresult
    end subroutine pqclear

    ! int PQntuples(const PGresult *res);
    function pqntuples(pgresult) bind(c, name='PQntuples') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqntuples

    ! int PQnfields(const PGresult *res);
    function pqnfields(pgresult) bind(c, name='PQnfields') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int) :: r
    end function pqnfields

    ! char *PQfname(const PGresult *res, int column_number);
    function pqfname(pgresult, column_number) bind(c, name='PQfname') result(r)
      import :: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: pgresult
      integer(kind=c_int), intent(in) :: column_number
      type(c_ptr) :: r
    end function pqfname

    ! int PQfnumber(const PGresult *res, const char *column_name);
    function pqfnumber(pgresult, column_name) bind(c, name='PQfnumber') result(r)
      import :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr), intent(in) :: pgresult
      character(kind=c_char), intent(in) :: column_name
      integer(kind=c_int) :: r
    end function pqfnumber






  end interface

  contains

    function exec(conn, command) result(r)
      !! Submits a command to the server and waits for the result.
      type(c_ptr), intent(in) :: conn
        !! Database connection pointer.
      character(len=*), intent(in) :: command
        !! The command string can include multiple SQL commands (separated by semicolons).
        !! Multiple queries sent in a single PQexec call are processed in a single transaction,
        !! unless there are explicit BEGIN/COMMIT commands included in the query string to divide
        !! it into multiple transactions. (See Section 53.2.2.1 for more details about how the
        !! server handles multi-query strings.)
        !! Note however that the returned PGresult structure describes only the result of the last
        !! command executed from the string. Should one of the commands fail, processing of the
        !! string stops with it and the returned PGresult describes the error condition.
      type(c_ptr) :: r
      r = pqexec(conn, cstr(command))
    end function exec

    function resultstatus(pgresult) result(r)
      !! Returns the result status of the command.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      integer(kind=c_int) :: r
        !! PQresultStatus can return one of the following values:[TODO]
      r = pqresultstatus(pgresult)
    end function resultstatus

    function resstatus(status) result(r)
      !! Converts the enumerated type returned by PQresultStatus into a string constant describing the status code.
      !! The caller should not free the result.
      integer(kind=c_int), intent(in) :: status
        !! Integer returned by resultstatus.
      character(len=:), allocatable :: r
        !! String constant describing the submitted code.
      type(c_ptr) :: ptr
      ptr = pqresstatus(status)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function resstatus

    function resulterrormessage(pgresult) result(r)
      !! Returns the error message associated with the command, or an empty string if there was no error.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      character(len=:), allocatable :: r
        !! If there was an error, the returned string will include a trailing newline. i
        !! The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
        !! Immediately following a PQexec or PQgetResult call, PQerrorMessage (on the connection) will return the same string as
        !! PQresultErrorMessage (on the result). However, a PGresult will retain its error message until destroyed, whereas the
        !! connection's error message will change when subsequent operations are done. Use PQresultErrorMessage when you want to
        !! know the status associated with a particular PGresult; use PQerrorMessage when you want to know the status from
        !! the latest operation on the connection.
      type(c_ptr) :: ptr
      ptr = pqresulterrormessage(pgresult)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function resulterrormessage

    function resulterrorfield(pgresult, fieldcode) result(r)
      !! Returns an individual field of an error report.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      integer(kind=c_int) :: fieldcode
        !! An error field identifier; see the symbols listed below.[todo]
      character(len=:), allocatable :: r
        !! NULL is returned if the PGresult is not an error or warning result, or does not include the specified field.
        !! Field values will normally not include a trailing newline. The caller should not free the result directly.
        !! It will be freed when the associated PGresult handle is passed to PQclear.
      type(c_ptr) :: ptr
      ptr = pqresulterrorfield(pgresult, fieldcode)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function resulterrorfield

    subroutine clear(pgresult)
      !! Frees the storage associated with a PGresult.
      !! Every command result should be freed via PQclear when it is no longer needed.
      !! You can keep a PGresult object around for as long as you need it; it does not go away when you issue a new command,
      !! nor even if you close the connection. To get rid of it, you must call PQclear. Failure to do this will result in
      !! memory leaks in your application.
      type(c_ptr), intent(in) :: pgresult
      call pqclear(pgresult)
    end subroutine clear

    function ntuples(pgresult) result(r)
      !! Returns the number of rows (tuples) in the query result. (Note that PGresult objects are limited to no more
      !! than INT_MAX rows, so an int result is sufficient.)
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      integer(kind=c_int) :: r
        !! Number of rows (tuples) in the query result.
      r = pqntuples(pgresult)
    end function ntuples

    function nfields(pgresult) result(r)
      !! Returns the number of columns (fields) in each row of the query result.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      integer(kind=c_int) :: r
        !! Number of fields.
      r = pqnfields(pgresult)
    end function nfields

    function fname(pgresult, column_number) result(r)
      !! Returns the column name associated with the given column number.
      !! The caller should not free the result directly.
      !! It will be freed when the associated PGresult handle is passed to PQclear.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      integer(kind=c_int) :: column_number
        !! P!! Column numbers start at 0.
      character(len=:), allocatable :: r
        !! NULL is returned if the PGresult is not an error or warning result, or does not include the specified field.
        !! Field values will normally not include a trailing newline. The caller should not free the result directly.
        !! It will be freed when the associated PGresult handle is passed to PQclear.
      type(c_ptr) :: ptr
      ptr = pqfname(pgresult, column_number)
      if (c_associated(ptr)) then
        call c_f_str_ptr(ptr, r)
      end if
    end function fname

    function fnumber(pgresult, column_name) result(r)
      !! Returns the column number associated with the given column name.
      type(c_ptr), intent(in) :: pgresult
        !! PGresult pointer.
      character(len=*), intent(in) :: column_name
        !! The given name is treated like an identifier in an SQL command, that is, it is downcased unless double-quoted.
      integer(kind=c_int) :: r
        !! -1 is returned if the given name does not match any column.
      r = pqfnumber(pgresult, cstr(column_name))
    end function fnumber





end module fpq_execute


