# fpq
(most) postgreSQL (libpq) fortran bindings

WIP!

Opted to break the functions into various fpq_modules as done per the libpq documentation.

The non-blocking and SSL functions have **NOT** been wrapped.

[Documentation.](http://198.58.104.30:8080/static/doc/index.html)

Uses stdlib string_type to allow arbitrary character lengths in string arrays. Tests currently failing when using these functions (see connectdbparams).


