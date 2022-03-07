# fpq
postgreSQL (libpq) fortran bindings

Opted to break the functions into various fpq_modules as done per the [libpq documentation](https://www.postgresql.org/docs/current/libpq.html).

## build using fpm
Add to your fpm.toml:

```
[build]
link = "pq"
[dependencies]
fpq.git = "https://github.com/freevryheid/fpq"
```

## Documentation
Generate the source code documentation with
[FORD](https://github.com/cmacmackin/ford). Add FORD with `pip`, for example:

```
$ python3 -m venv virtual-environment/
$ source virtual-environment/bin/activate
$ python3 -m pip install ford
```

Or, instead, just install the package in your user directory:

```
$ python3 -m pip install --user ford
```

Then, run:

```
$ ford fpq.md
```

Open `doc/index.html` in a web browser.
