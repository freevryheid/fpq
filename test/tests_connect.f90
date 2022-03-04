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
          new_unittest("test for ping", test_ping)                       &
        , new_unittest("test for pingparams", test_pingparams)           &
        , new_unittest("test for connectdbparams", test_connectdbparams) &
        , new_unittest("test for connectdb", test_connectdb)             &
        , new_unittest("test for setdblogin", test_setdblogin)           &
      ]
    end subroutine collect_tests_connect

    subroutine test_ping(error)
      ! Ping the postgresql server
      type(error_type), allocatable, intent(out) :: error
      call check(error, ping("dbname=smgr"), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_ping

    subroutine test_pingparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2) :: keys, vals
      ! length required for string arrays
      keys = [character(len=10) :: "user", "dbname"]
      vals = [character(len=10) :: "grassy", "smgr"]
      call check(error, pingparams(keys, vals, 0), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_pingparams

    subroutine test_connectdbparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2) :: keys, vals
      type(pq) :: conn
      keys = [character(len=10) :: c_str("user"), c_str("dbname")]
      vals = [character(len=10) :: c_str("grassy"), c_str("smgr")]
      conn = connectdbparams(keys, vals, 0)
      call check(error, status(conn), CONNECTION_OK)
      call finish(conn)
      if (allocated(error)) return
    end subroutine test_connectdbparams

    subroutine test_connectdb(error)
      ! simple connect
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: conn
      conn = connectdb()
      call check(error, status(conn), CONNECTION_OK)
      call finish(conn)
      if (allocated(error)) return
    end subroutine test_connectdb

    subroutine test_setdblogin(error)
      ! yet another connect
      type(error_type), allocatable, intent(out) :: error
      type(pq) :: conn
      conn = setdblogin("", "", "", "", "", "", "")
      call check(error, status(conn), CONNECTION_OK)
      call finish(conn)
      if (allocated(error)) return
    end subroutine test_setdblogin

end module tests_connect
