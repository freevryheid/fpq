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
      ! ping the postgresql server.
      type(error_type), allocatable, intent(out) :: error
      call check(error, ping(cstr("dbname=smgr")), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_ping

    subroutine test_pingparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2), target :: keys, vals
      ! not sure howto define arrays have varying string lengths
      ! shouldn't be an issue as c uses null char to delineate strings,
      ! which we'll provide:
      keys = [character(len=10) :: cstr("user"), cstr("dbname")]
      vals = [character(len=10) :: cstr("grassy"), cstr("smgr")]
      call check(error, pingparams(c_loc(keys), c_loc(vals), int(0, kind=c_int)), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_pingparams

    subroutine test_connectdbparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2), target :: keys, vals
      type(c_ptr) :: pgconn
      ! not sure howto define arrays have varying string lengths
      ! shouldn't be an issue as c uses null char to delineate strings,
      ! which we'll provide:
      keys = [character(len=10) :: cstr("user"), cstr("dbname")]
      vals = [character(len=10) :: cstr("grassy"), cstr("smgr")]
      pgconn = connectdbparams(c_loc(keys), c_loc(vals), int(0, kind=c_int))
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdbparams

    subroutine test_connectdb(error)
      ! simple connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = connectdb(cstr(""))
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdb

    subroutine test_setdblogin(error)
      ! yet another connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = setdblogin(cstr(""), cstr("") , cstr(""), cstr(""), cstr(""), cstr(""), cstr(""))
      call check(error, status(pgconn), CONNECTION_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_setdblogin

end module tests_connect
