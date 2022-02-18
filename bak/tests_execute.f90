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
      testsuite = [                                                      &
        new_unittest("test for exec", test_exec)                         &
        ! , new_unittest("test for pingparams", test_pingparams)           &
        ! , new_unittest("test for connectdbparams", test_connectdbparams) &
        ! , new_unittest("test for connectdb", test_connectdb)             &
        ! , new_unittest("test for setdblogin", test_setdblogin)           &
      ]
    end subroutine collect_tests_execute

    subroutine test_exec(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo, cmd
      type(c_ptr) :: pgconn, pgresult

      conninfo = "dbname = smgr" // c_null_char
      cmd = "select * from texas;" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, status(pgconn), CONNECTION_OK)
      ! pgresult = exec(pgconn, cmd)
      ! call check(error, ping(conninfo), PQPING_OK)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_exec

end module tests_execute
