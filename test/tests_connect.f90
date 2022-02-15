module tests_connect
  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_connect

  contains

    subroutine collect_tests_connect(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                      &
        new_unittest("test for ping", test_ping)                         &
        , new_unittest("test for pingparams", test_pingparams)           &
        , new_unittest("test for connectdbparams", test_connectdbparams) &
        , new_unittest("test for connectdb", test_connectdb)             &
        , new_unittest("test for setdblogin", test_setdblogin)           &
      ]
    end subroutine collect_tests_connect

    subroutine test_ping(error)
      ! ping the postgresql server with defaults
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      call check(error, ping(conninfo), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_ping

    subroutine test_pingparams(error)
      ! keys and vals are arrays of string pointers
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char), target :: key
      character(kind=c_char), target :: val
      type(c_ptr), allocatable :: keys(:)
      type(c_ptr), allocatable :: vals(:)
      allocate(keys(1))
      allocate(vals(1))
      key = "" // c_null_char
      val = "" // c_null_char
      keys(1) = c_loc(key)
      vals(1) = c_loc(val)
      call check(error, pingparams(keys, vals, int(0, kind=c_int)), PQPING_OK)
      deallocate(keys)
      deallocate(vals)
      if (allocated(error)) return
    end subroutine test_pingparams

    subroutine test_connectdbparams(error)
      ! keys and vals are arrays of string pointers
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char), target :: key
      character(kind=c_char), target :: val
      type(c_ptr), allocatable :: keys(:)
      type(c_ptr), allocatable :: vals(:)
      allocate(keys(1))
      allocate(vals(1))
      key = "" // c_null_char
      val = "" // c_null_char
      keys(1) = c_loc(key)
      vals(1) = c_loc(val)
      pgconn = connectdbparams(keys, vals, int(0, kind=c_int))
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      deallocate(keys)
      deallocate(vals)
      if (allocated(error)) return
    end subroutine test_connectdbparams

    subroutine test_connectdb(error)
      ! simple connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdb

    subroutine test_setdblogin(error)
      ! yet another connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: pghost, pgport, pgoptions, pgtty, dbname, login, pwd
      pghost = "" // c_null_char
      pgport = "" // c_null_char
      pgoptions = "" // c_null_char
      pgtty = "" // c_null_char
      dbname = "" // c_null_char
      login = "" // c_null_char
      pwd = "" // c_null_char
      pgconn = setdblogin(pghost, pgport, pgoptions, pgtty, dbname, login, pwd)
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_setdblogin

end module tests_connect
