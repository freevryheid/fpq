module tests_status

  use, intrinsic :: iso_c_binding
  use fpq
  use testdrive, only: error_type, unittest_type, new_unittest, check, skip_test
  implicit none
  character(len=255) :: logname
  private
  public :: collect_tests_status

  contains

    subroutine collect_tests_status(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call get_environment_variable("LOGNAME", logname)
      testsuite = [                                                                          &
          new_unittest("test for pqdb", test_pqdb)                                           &
        , new_unittest("test for pquser", test_pquser)                                       &
        , new_unittest("test for pqpass", test_pqpass)                                       &
        , new_unittest("test for pqhost", test_pqhost)                                       &
        , new_unittest("test for pqhostaddr", test_pqhostaddr)                               &
        , new_unittest("test for pqport", test_pqport)                                       &
        , new_unittest("test for pqoptions", test_pqoptions)                                 &
        , new_unittest("test for pqtransactionstatus", test_pqtransactionstatus)             &
        , new_unittest("test for pqparameterstatus", test_pqparameterstatus)                 &
        , new_unittest("test for pqprotocolversion", test_pqprotocolversion)                 &
        , new_unittest("test for pqserverversion", test_pqserverversion)                     &
        , new_unittest("test for pqsocket", test_pqsocket)                                   &
        , new_unittest("test for pqbackendpid", test_pqbackendpid)                           &
        , new_unittest("test for pqconnectionneedspassword", test_pqconnectionneedspassword) &
        , new_unittest("test for pqconnectionusedpassword", test_pqconnectionusedpassword)   &
        , new_unittest("test for pqsslinuse", test_pqsslinuse)                               &
      ]
    end subroutine collect_tests_status

    subroutine test_pqdb(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqdb(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, trim(logname))
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqdb

    subroutine test_pquser(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pquser(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, trim(logname))
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pquser

    subroutine test_pqpass(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqpass(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqpass

    subroutine test_pqhost(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqhost(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "/run/postgresql")
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqhost

    subroutine test_pqhostaddr(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqhostaddr(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqhostaddr

    subroutine test_pqport(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqport(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "5432")
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqport

    subroutine test_pqoptions(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(c_ptr) :: pgconn, ptr
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqoptions(pgconn)
      if (c_associated(ptr)) call c_f_str_ptr(ptr, r)
      call check(error, r, "")
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqoptions

    subroutine test_pqtransactionstatus(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqtransactionstatus(pgconn), PQTRANS_IDLE)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqtransactionstatus

    subroutine test_pqparameterstatus(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: rstatus, r
      type(c_ptr) :: pgconn, ptr
      rstatus = "13.5"
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      ptr = pqparameterstatus(pgconn, c_str("server_version"))
      call c_f_str_ptr(ptr, r)
      call check(error, r, rstatus)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqparameterstatus

    subroutine test_pqprotocolversion(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqprotocolversion(pgconn), 3)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqprotocolversion

    subroutine test_pqserverversion(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqserverversion(pgconn), 130005)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqserverversion

    subroutine test_pqsocket(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqsocket(pgconn) >= 0)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqsocket

    subroutine test_pqbackendpid(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqbackendpid(pgconn) > 0)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqbackendpid

    subroutine test_pqconnectionneedspassword(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqconnectionneedspassword(pgconn), 0)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqconnectionneedspassword

    subroutine test_pqconnectionusedpassword(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqconnectionusedpassword(pgconn), 0)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqconnectionusedpassword

    subroutine test_pqsslinuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(c_ptr) :: pgconn
      pgconn = pqconnectdb(c_str(""))
      call check(error, pqstatus(pgconn), CONNECTION_OK)
      if (allocated(error)) return
      call check(error, pqsslinuse(pgconn), 0)
      call pqfinish(pgconn)
      if (allocated(error)) return
    end subroutine test_pqsslinuse

end module tests_status
