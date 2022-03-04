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
      testsuite = [                            &
          new_unittest("test for status", test_status) &
      ]
    end subroutine collect_tests_status

    subroutine test_status(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: r
      type(pq) :: conn
      conn = connectdb()
      call check(error, status(conn), CONNECTION_OK)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = db(conn)
      call check(error, r, trim(logname))
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = user(conn)
      call check(error, r, trim(logname))
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = pass(conn)
      call check(error, r, "")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = host(conn)
      call check(error, r, "/run/postgresql")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = hostaddr(conn)
      call check(error, r, "")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = port(conn)
      call check(error, r, "5432")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = options(conn)
      call check(error, r, "")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, transactionstatus(conn), PQTRANS_IDLE)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      r = parameterstatus(conn, "server_version")
      call check(error, r, "13.6")
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, protocolversion(conn), 3)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, serverversion(conn), 130006)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, socket(conn) >= 0)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, backendpid(conn) > 0)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, connectionneedspassword(conn), .false.)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, connectionusedpassword(conn), .false.)
      if (allocated(error)) then
        call finish(conn)
        return
      end if
      call check(error, sslinuse(conn), .false.)
      call finish(conn)
      if (allocated(error)) return
    end subroutine test_status

end module tests_status
