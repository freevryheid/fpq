module tests
  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests
  contains
    subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                    &
        new_unittest("test for connectdbparams", test_connectdbparams) &
      ]
    end subroutine collect_tests
    subroutine test_connectdbparams(error)
      type(error_type), allocatable, intent(out) :: error
      ! integer, pointer :: conn
      type(c_ptr) :: pgconn
      character(len=:), allocatable :: keywords(:)
      character(len=:), allocatable :: values(:)
      keywords = [""]
      values   = [""]
      pgconn = connectdbparams(keywords, values, 0)
      call check(error, status(pgconn), CONNECTION_OK)

      print *, "db: " // db(pgconn)
      print *, "user: " // user(pgconn)
      print *, "pass: " // pass(pgconn)
      print *, "host: " // host(pgconn)
      print *, "hostaddr: " // hostaddr(pgconn)
      print *, "port: " // port(pgconn)

      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectdbparams
end module tests
