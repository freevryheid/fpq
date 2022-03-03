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
      testsuite = [                                                          &
          ! new_unittest("test for pqping", test_pqping)                       &
          new_unittest("test for ping", test_ping)                           &
        , new_unittest("test for pqpingparams", test_pqpingparams)           &
        , new_unittest("test for pqconnectdbparams", test_pqconnectdbparams) &
        , new_unittest("test for pqconnectdb", test_pqconnectdb)             &
        , new_unittest("test for pqsetdblogin", test_pqsetdblogin)           &
      ]
    end subroutine collect_tests_connect

    ! subroutine test_pqping(error)
    !   ! Ping the postgresql server.
    !   type(error_type), allocatable, intent(out) :: error
    !   call check(error, pqping(c_str("dbname=smgr")), PQPING_OK)
    !   if (allocated(error)) return
    ! end subroutine test_pqping

    subroutine test_ping(error)
      ! Ping the postgresql server.
      type(error_type), allocatable, intent(out) :: error
      call check(error, ping("dbname=smgr"), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_ping

    subroutine test_pqpingparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2), target :: keys, vals
      ! not sure howto define arrays have varying string lengths
      ! shouldn't be an issue as c uses null char to delineate strings,
      ! which we'll provide:
      keys = [character(len=10) :: c_str("user"), c_str("dbname")]
      vals = [character(len=10) :: c_str("grassy"), c_str("smgr")]
      call check(error, pqpingparams(c_loc(keys), c_loc(vals), 0_c_int), PQPING_OK)
      if (allocated(error)) return
    end subroutine test_pqpingparams

    subroutine test_pqconnectdbparams(error)
      ! keys and vals are string arrays
      type(error_type), allocatable, intent(out) :: error
      character(len=10), dimension(2), target :: keys, vals
      type(c_ptr) :: pgconn
      ! not sure howto define arrays have varying string lengths
      ! shouldn't be an issue as c uses null char to delineate strings,
      ! which we'll provide:
      keys = [character(len=10) :: c_str("user"), c_str("dbname")]
      vals = [character(len=10) :: c_str("grassy"), c_str("smgr")]
      pgconn = pqconnectdbparams(c_loc(keys), c_loc(vals), 0_c_int)
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqconnectdbparams

    subroutine test_pqconnectdb(error)
      ! simple connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqconnectdb

    subroutine test_pqsetdblogin(error)
      ! yet another connect
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqsetdblogin(c_str(""), c_str("") , c_str(""), c_str(""), c_str(""), c_str(""), c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqsetdblogin

end module tests_connect
