module tests_misc

  ! use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_misc

  contains

    subroutine collect_tests_misc(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                          &
          new_unittest("test for libversion", test_libversion)                       &
        ! , new_unittest("test for pqpingparams", test_pqpingparams)           &
      ]
    end subroutine collect_tests_misc

    subroutine test_libversion(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, libversion(), 130006)
      if (allocated(error)) return
    end subroutine test_libversion

end module tests_misc
