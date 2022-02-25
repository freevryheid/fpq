module tests_execute

  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_execute

  contains

    subroutine collect_tests_execute(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                            &
          new_unittest("test for pqexec", test_pqexec)                         &
        , new_unittest("test for pqexecparams", test_pqexecparams)             &
        , new_unittest("test for pqexecprepared", test_pqexecprepared)         &
        , new_unittest("test for pqdescribeprepared", test_pqdescribeprepared) &
        , new_unittest("test for pqtuples", test_pqtuples)                     &
        , new_unittest("test for pqtable", test_pqtable)                       &
      ]
    end subroutine collect_tests_execute

    subroutine test_pqexec(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = pqexec(pgconn, c_str('select 1 as foo, 2 as "BAR"'))
      call check(error, pqresultstatus(pgresult), PGRES_TUPLES_OK)
      call pqclear(pgresult)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqexec

    subroutine test_pqexecparams(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult, mptr
      character(len=10), dimension(1), target :: paramvalues
      character(len=:), allocatable :: msg
      paramvalues = [character(len=10) :: c_str("FOO")]
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = pqexecparams(pgconn                                                       &
        , c_str("select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1') &
        , 1_c_int                                                                          &  ! one parameter
        , c_null_ptr                                                                       &  ! let the backend deduce param type
        , c_loc(paramvalues)                                                               &
        , c_null_ptr                                                                       &  ! don't need param lengths since text
        , c_null_ptr                                                                       &  ! defaults to all text params
        , 0_c_int)                                                                            ! ask for text results
      call check(error, pqresultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        mptr = pqerrormessage(pgconn)
        call c_f_str_ptr(mptr, msg)
        print *, msg
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call pqclear(pgresult)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqexecparams

    subroutine test_pqexecprepared(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult, mptr
      character(len=10), dimension(1), target :: paramvalues
      character(len=:), allocatable :: msg
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      paramvalues = [character(len=10) :: c_str("FOO")]
      pgresult = pqprepare(pgconn                                                          &
        , c_str("stmt123")                                                                 &
        , c_str("select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1') &
        , 1_c_int                                                                          &
        , c_null_ptr)
      call check(error, pqresultstatus(pgresult), PGRES_COMMAND_OK)
      if (allocated(error)) then
        mptr = pqerrormessage(pgconn)
        call c_f_str_ptr(mptr, msg)
        print *, msg
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      pgresult = pqexecprepared(pgconn &
        , c_str("stmt123")             &
        , 1_c_int                      &
        , c_loc(paramvalues)           &
        , c_null_ptr                   &
        , c_null_ptr                   &
        , 1_c_int)
      call check(error, pqresultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        mptr = pqerrormessage(pgconn)
        call c_f_str_ptr(mptr, msg)
        print *, msg
        call pqclear(pgresult)
        call pqfinish(pgconn)
      end if
      call pqclear(pgresult)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqexecprepared

    subroutine test_pqdescribeprepared(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult1, pgresult2, mptr
      integer(kind=c_int) :: status
      character(len=10), dimension(1), target :: paramvalues
      character(len=:), allocatable :: msg
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      paramvalues = [character(len=10) :: c_str("FOO")]
      pgresult1 = pqprepare(pgconn                        &
        , c_str("stmt456")                                &
        , c_str("select * from (select 'FOO' as foo, " // '2 as "BAR") a1 where foo = $1') &
        , 1_c_int                                         &
        , c_null_ptr)
      call check(error, pqresultstatus(pgresult1), PGRES_COMMAND_OK)
      if (allocated(error)) then
        mptr = pqerrormessage(pgconn)
        call c_f_str_ptr(mptr, msg)
        print *, msg
        call pqclear(pgresult1)
        call pqfinish(pgconn)
        return
      end if
      pgresult2 = pqdescribeprepared(pgconn, c_str("stmt456"))
      status = pqresultstatus(pgresult2)
      call check(error, status, PGRES_COMMAND_OK)
      if (allocated(error)) then
        mptr = pqerrormessage(pgconn)
        call c_f_str_ptr(mptr, msg)
        print *, msg
        call pqclear(pgresult2)
        call pqfinish(pgconn)
        return
      end if
      mptr = pqresstatus(status)
      call c_f_str_ptr(mptr, msg)
      call check(error, msg, "PGRES_COMMAND_OK")
      if (allocated(error)) then
        call pqclear(pgresult2)
        call pqfinish(pgconn)
        return
      end if
      call pqclear(pgresult2)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqdescribeprepared

    subroutine test_pqtuples(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult, pname
      character(len=:), allocatable :: fname
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = pqexec(pgconn, c_str("select 'FOO' as foo, " // '2 as "BAR"'))
      call check(error, pqresultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqntuples(pgresult), 1_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqnfields(pgresult), 2_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      pname = pqfname(pgresult, 0_c_int)
      call c_f_str_ptr(pname, fname)
      call check(error, fname, "foo")
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqfnumber(pgresult, c_str('"BAR"')), 1_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call pqclear(pgresult)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqtuples

    subroutine test_pqtable(error)
      ! TODO - revisit this test (pqiftablecol)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn, pgresult, pval
      character(len=:), allocatable :: val
      pgconn = pqconnectdb(c_str("dbname=postgres"))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      pgresult = pqexec(pgconn, &
        c_str("select 'FOO' as foo, " // '2 as "BAR"'))
      call check(error, pqresultstatus(pgresult), PGRES_TUPLES_OK)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqftablecol(pgresult, 0_c_int), 0_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqfformat(pgresult, 0_c_int), 0_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqbinarytuples(pgresult), 0_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      pval = pqgetvalue(pgresult, 0_c_int, 1_c_int)
      call c_f_str_ptr(pval, val)
      call check(error, val, "2")
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqgetisnull(pgresult, 0_c_int, 1_c_int), 0_c_int)
      if (allocated(error)) then
        call pqclear(pgresult)
        call pqfinish(pgconn)
        return
      end if
      call check(error, pqgetlength(pgresult, 0_c_int, 1_c_int), 1_c_int)
      call pqclear(pgresult)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqtable

end module tests_execute
