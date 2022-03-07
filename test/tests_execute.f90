module tests_execute

  ! use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_execute

  contains

    subroutine collect_tests_execute(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                            &
          new_unittest("test for exec", test_exec)                         &
        , new_unittest("test for execparams", test_execparams)             &
        , new_unittest("test for execprepared", test_execprepared)         &
        , new_unittest("test for describeprepared", test_describeprepared) &
        , new_unittest("test for tuples", test_tuples)                     &
        , new_unittest("test for table", test_table)                       &
      ]
    end subroutine collect_tests_execute

    subroutine test_exec(error)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = exec(pgconn, 'select 1 as foo, 2 as "BAR"')
      call check(error, resultstatus(pgresult), PGRES_TUPLES_OK)
      call clear(pgresult)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_exec

    subroutine test_execparams(error)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult
      character(len=10), dimension(1) :: paramvalues
      character(len=:), allocatable :: msg
      paramvalues = [character(len=10) :: "FOO"]
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = execparams(pgconn                                                  &
        , "select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1' &
        , 1                                                                         &  ! one parameter
        , paramvalues                                                               &
        , 0)                                                                           ! ask for text results
      call check(error, resultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        print *, errormessage(pgconn)
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call clear(pgresult)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_execparams

    subroutine test_execprepared(error)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult
      character(len=10), dimension(1) :: paramvalues
      character(len=:), allocatable :: msg
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      paramvalues = [character(len=10) :: "FOO"]
      pgresult = prepare(pgconn                                                     &
        , "stmt123"                                                                 &
        , "select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1' &
        , 1)
      call check(error, resultstatus(pgresult), PGRES_COMMAND_OK)
      if (allocated(error)) then
        print *, errormessage(pgconn)
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      pgresult = execprepared(pgconn &
        , "stmt123"                  &
        , 1                          &
        , paramvalues                &
        , 1)
      call check(error, resultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        print *, errormessage(pgconn)
        call clear(pgresult)
        call finish(pgconn)
      end if
      call clear(pgresult)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_execprepared

    subroutine test_describeprepared(error)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult1, pgresult2
      integer :: status1
      character(len=10), dimension(1) :: paramvalues
      character(len=:), allocatable :: msg
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      paramvalues = [character(len=10) :: "FOO"]
      pgresult1 = prepare(pgconn                                                    &
        , "stmt456"                                                                 &
        , "select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1' &
        , 1)
      call check(error, resultstatus(pgresult1), PGRES_COMMAND_OK)
      if (allocated(error)) then
        print *, errormessage(pgconn)
        call clear(pgresult1)
        call finish(pgconn)
        return
      end if
      pgresult2 = describeprepared(pgconn, "stmt456")
      status1 = resultstatus(pgresult2)
      call check(error, status1, PGRES_COMMAND_OK)
      if (allocated(error)) then
        print *, errormessage(pgconn)
        call clear(pgresult2)
        call finish(pgconn)
        return
      end if
      msg = resstatus(status1)
      call check(error, msg, "PGRES_COMMAND_OK")
      if (allocated(error)) then
        call clear(pgresult2)
        call finish(pgconn)
        return
      end if
      call clear(pgresult2)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_describeprepared

    subroutine test_tuples(error)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult
      character(len=:), allocatable :: name
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = exec(pgconn, "select 'FOO' as foo, " // '2 as "BAR"')
      call check(error, resultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, ntuples(pgresult), 1)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, nfields(pgresult), 2)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      name = fname(pgresult, 0)
      call check(error, name, "foo")
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, fnumber(pgresult, '"BAR"'), 1)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call clear(pgresult)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_tuples

    subroutine test_table(error)
      ! TODO - revisit this test (iftablecol)
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: pgconn, pgresult
      character(len=:), allocatable :: val
      pgconn = connectdb("dbname=postgres")
      call check(error, status(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = exec(pgconn, "select 'FOO' as foo, " // '2 as "BAR"')
      call check(error, resultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, ftablecol(pgresult, 0), 0)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, fformat(pgresult, 0), 0)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, binarytuples(pgresult), .false.)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      val = getvalue(pgresult, 0, 1)
      call check(error, val, "2")
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, getisnull(pgresult, 0, 1), .false.)
      if (allocated(error)) then
        call clear(pgresult)
        call finish(pgconn)
        return
      end if
      call check(error, getlength(pgresult, 0, 1), 1)
      call clear(pgresult)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_table

end module tests_execute
