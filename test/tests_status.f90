module tests_status
  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_status
  contains

    subroutine collect_tests_status(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                                          &
        new_unittest("test for db", test_db)                                 &
        , new_unittest("test for user", test_user)                           &
        , new_unittest("test for pass", test_pass)                           &
        , new_unittest("test for host", test_host)                           &
        , new_unittest("test for hostaddr", test_hostaddr)                   &
        , new_unittest("test for port", test_port)                           &
        , new_unittest("test for options", test_options)                     &
        , new_unittest("test for transactionstatus", test_transactionstatus) &
        , new_unittest("test for parameterstatus", test_parameterstatus)     &
      ]
    end subroutine collect_tests_status

    subroutine test_db(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = "grassy"
      ptr = db(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_db

    subroutine test_user(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = "grassy"
      ptr = user(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_user

    subroutine test_pass(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = ""
      ptr = pass(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_pass

    subroutine test_host(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = "/run/postgresql"
      ptr = host(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_host

    subroutine test_hostaddr(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = "" ! localhost (no IP?)
      ptr = hostaddr(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_hostaddr

    subroutine test_port(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = "5432"
      ptr = port(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_port

    subroutine test_options(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: name, r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      name = ""
      ptr = options(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, name)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_options

    subroutine test_transactionstatus(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, transactionstatus(pgconn), PQTRANS_IDLE)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_transactionstatus

    subroutine test_parameterstatus(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(kind=c_char) :: name, r, rstatus
      ! character(len=:), allocatable :: rstatus
      ! character(len=:), allocatable :: r
      type(c_ptr) :: pgconn
      conninfo = "" // c_null_char
      ! name = "server_version" // c_null_char
      ! name = "server_encoding" // c_null_char
      name = "is_superuser" // c_null_char
      rstatus = "" // c_null_char
      pgconn = connectdb(conninfo)
      r = parameterstatus(pgconn, name)
      ! if (c_associated(ptr)) then
      !   print *, "associated"
      !   call c_f_str_ptr(ptr, r)
      ! else
      !   print *, "not associated"
      ! end if
      ! print *, "here3"
      ! print *, loc(r)
      ! print *, "here4"
      call check(error, r, rstatus)
      ! print *, "here5"
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_parameterstatus

end module tests_status
