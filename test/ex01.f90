program ex01
  use, intrinsic :: iso_c_binding
  use fpq
  implicit none
  character(len=*), parameter :: CONNINFO = 'host=localhost port=5432 dbname=smgr connect_timeout=10'
  type(c_ptr) :: conn, ress
  conn = connectdb(CONNINFO)
  print *, "db: " // db(conn)
  print *, "user: " // user(conn)
  print *, "pass: " // pass(conn)
  print *, "host: " // host(conn)
  print *, "hostaddr: " // hostaddr(conn)
  print *, "port: " // port(conn)
  print '(a, i6)', "status: ", status(conn)
  print '(a, i6)', "serverversion: ", serverversion(conn)
  print *, "errormessage: " // errormessage(conn)

  ress = exec(conn, "select * from texas;")
  call finish(conn)
  call exit(0)
end program ex01

