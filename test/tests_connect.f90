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
        new_unittest("test for connectdbparams", test_connectdbparams), &
        new_unittest("test for connectdbs", test_connectdb),            &
        new_unittest("test for setdblogin", test_setdblogin),           &
        new_unittest("test for setdb", test_setdb)                      &
      ]
    end subroutine collect_tests_connect
    subroutine test_connectdbparams(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      type(string_type) :: keywords(3)
      type(string_type) :: values(3)
      keywords(1) = "host"
      values(1) = "localhost"
      keywords(2) = "port"
      values(2) = "5432"
      keywords(3) = "dbname"
      values(3) = "smgr"
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
