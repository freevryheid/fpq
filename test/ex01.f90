program ex01
  use, intrinsic :: iso_c_binding
  use fpq
  implicit none
  character(len=*), parameter :: CONNINFO = 'host=localhost port=5432 dbname=smgr connect_timeout=10'
  type(c_ptr) :: conn
  conn = connectdb(CONNINFO)
  print *, "Database name: " // db(conn)
  print *, "Database user: " // user(conn)
  call finish(conn)
  call exit(0)
end program ex01

