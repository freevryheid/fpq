module tests_status
  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check, skip_test
  implicit none
  private
  public :: collect_tests_status
  character(len=255) :: logname

  contains

    subroutine collect_tests_status(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call get_environment_variable("LOGNAME", logname)
      testsuite = [                                                                      &
        new_unittest("test for db", test_db)                                             &
        , new_unittest("test for user", test_user)                                       &
        , new_unittest("test for pass", test_pass)                                       &
        , new_unittest("test for host", test_host)                                       &
        , new_unittest("test for hostaddr", test_hostaddr)                               &
        , new_unittest("test for port", test_port)                                       &
        , new_unittest("test for options", test_options)                                 &
        , new_unittest("test for transactionstatus", test_transactionstatus)             &
        , new_unittest("test for parameterstatus", test_parameterstatus)                 &
        , new_unittest("test for protocolversion", test_protocolversion)                 &
        , new_unittest("test for serverversion", test_serverversion)                     &
        , new_unittest("test for socket", test_socket)                                   &
        , new_unittest("test for backendpid", test_backendpid)                           &
        , new_unittest("test for connectionneedspassword", test_connectionneedspassword) &
        , new_unittest("test for connectionusedpassword", test_connectionusedpassword)   &
        , new_unittest("test for sslinuse", test_sslinuse)                               &
      ]
    end subroutine collect_tests_status

    subroutine test_db(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = db(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, trim(logname))
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_db

    subroutine test_user(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = user(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, trim(logname))
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_user

    subroutine test_pass(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = pass(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_pass

    subroutine test_host(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = host(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "/run/postgresql")
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_host

    subroutine test_hostaddr(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = hostaddr(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_hostaddr

    subroutine test_port(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = port(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "5432")
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_port

    subroutine test_options(error)
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      ptr = options(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
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
      ! FIXME
      type(error_type), allocatable, intent(out) :: error
      character(kind=c_char) :: conninfo, param
      character(len=:), allocatable :: rstatus, r
      type(c_ptr) :: pgconn
      type(c_ptr) :: ptr
      conninfo = "" // c_null_char
      param = "server_version" // c_null_char
      rstatus = "13.5"
      pgconn = connectdb(conninfo)
      call check(error, status(pgconn), CONNECTION_OK)
      ptr = parameterstatus(pgconn, param)
      if (.not. c_associated(ptr)) then
        call skip_test(error, "FIXME")
      else
        call c_f_str_ptr(ptr, r)
        call check(error, r, rstatus)
      end if
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_parameterstatus

    subroutine test_protocolversion(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, protocolversion(pgconn), 3)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_protocolversion

    subroutine test_serverversion(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, serverversion(pgconn), 130005)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_serverversion

    subroutine test_socket(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, socket(pgconn) >= 0)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_socket

    subroutine test_backendpid(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, backendpid(pgconn) > 0)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_backendpid

    subroutine test_connectionneedspassword(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, connectionneedspassword(pgconn), 0)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectionneedspassword

    subroutine test_connectionusedpassword(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, connectionusedpassword(pgconn), 0)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_connectionusedpassword

    subroutine test_sslinuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      character(kind=c_char) :: conninfo
      conninfo = "" // c_null_char
      pgconn = connectdb(conninfo)
      call check(error, sslinuse(pgconn), 0)
      call finish(pgconn)
      if (allocated(error)) return
    end subroutine test_sslinuse

end module tests_status
