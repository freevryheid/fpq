module tests_connect
  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use stdlib_string_type
  implicit none
  private
  public :: collect_tests_connect
  contains
    subroutine collect_tests_connect(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                     &
        new_unittest("test for ping", test_ping),                       &
        new_unittest("test for connectdb", test_connectdb),             &
        new_unittest("test for setdblogin", test_setdblogin),           &
        new_unittest("test for setdb", test_setdb),                     &
        new_unittest("test for connectdbparams", test_connectdbparams)  &
        ! TODO - FIX connectdbparams first
        ! new_unittest("test for connectstartparams", test_connectstartparams),        &
      ]
    end subroutine collect_tests_connect
    subroutine test_ping(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: conninfo
      conninfo = ""
      call check(error, ping(conninfo), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_ping
    subroutine test_connectdbparams(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      type(string_type) :: keywords(1)
      type(string_type) :: values(1)
      keywords(1) = " "
      values(1) = " "
      pgconn = connectdbparams(keywords, values, 0)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdbparams
    subroutine test_connectdb(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(len=:), allocatable :: conninfo
      conninfo = ""
      pgconn = connectdb(conninfo)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdb
    subroutine test_setdblogin(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(len=:), allocatable :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      pghost = ""
      pgport = ""
      pgoptions = ""
      pgtty = ""
      dbname = ""
      login = ""
      pwd = ""
      pgconn = setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_setdblogin
    subroutine test_setdb(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(len=:), allocatable :: pghost, pgport, pgoptions, pgtty, dbname
      pghost = ""
      pgport = ""
      pgoptions = ""
      pgtty = ""
      dbname = ""
      pgconn = setdb(pghost, pgport, pgoptions, pgtty, dbname)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_setdb
end module tests_connect
