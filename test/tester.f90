program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite
  use tests_connect, only : collect_tests_connect
  use tests_status, only : collect_tests_status
  implicit none
  integer :: stat
  stat = 0
  print *, " ... database connection tests ... "
  call run_testsuite(collect_tests_connect, error_unit, stat)
  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if
  stat = 0
  print *, " ... database status tests ... "
  call run_testsuite(collect_tests_status, error_unit, stat)
  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if
end program tester
