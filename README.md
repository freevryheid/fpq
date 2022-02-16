# fpq
(most) postgreSQL (libpq) fortran bindings

WIP!

Opted to break the functions into various fpq_modules as done per the libpq documentation.

The non-blocking functions have **NOT** been wrapped. Also, I don't use SSL so have no way of testing - so, didn't wrap the SSL functions either.

[Documentation.](http://198.58.104.30:8080/static/doc/index.html)

Having an issue with the parameterstatus function, shown below, result is not c_associated for some reason. Any help appreciated.

```fortran
    ! const char *PQparameterStatus(const PGconn *conn, const char *paramName);
    ! FIXME
    function parameterstatus(pgconn, param) bind(c, name='PQparameterStatus') result(r)
      !! Looks up a current parameter setting of the server.
      import :: c_char, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: pgconn
        !! Database connection pointer.
      character(kind=c_char), intent(in) :: param
        !! Parameter name e.g server_version, server_encoding, client_encoding,
        !! application_name, is_superuser, session_authorization, DateStyle,
        !! IntervalStyle, TimeZone, integer_datetimes, and standard_conforming_strings, etc.
      type(c_ptr) :: r
    end function parameterstatus
```

