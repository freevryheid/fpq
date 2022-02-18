program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite
  use tests_connect, only : collect_tests_connect
  use tests_status, only : collect_tests_status
  use tests_execute, only : collect_tests_execute
  implicit none
  integer :: stat
  character(len=:), allocatable :: test

  test = "3"

  if (scan(test, "1") > 0) then
    stat = 0
    print *, " ... database connection tests ... "
    call run_testsuite(collect_tests_connect, error_unit, stat)
    if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
    end if
  end if
  if (scan(test, "2") > 0) then
    stat = 0
    print *, " ... database status tests ... "
    call run_testsuite(collect_tests_status, error_unit, stat)
    if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
    end if
  end if
  if (scan(test, "3") > 0) then
    stat = 0
    print *, " ... database execute tests ... "
    call run_testsuite(collect_tests_execute, error_unit, stat)
    if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
    end if
  end if

end program tester
