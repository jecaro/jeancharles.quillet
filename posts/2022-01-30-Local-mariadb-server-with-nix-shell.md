---

title: Local mariadb server with nix-shell
lastmod: 2024-05-31

---

[Nix][nix] is a great technology. If you don't know what it is I urge you to
stop what you're doing now and start reading a bit about it.

It tries to bring FP concepts, such as purity, to package management. It makes
you think of a package as the result of a function which takes dependencies and
some configuration as inputs. The results of this function is stored in a path
which contains a unique hash computed with the inputs. Referential transparency
should be preserved: same inputs would produce the same outputs. That's for the
basics.

As a result, it completely messes up the FHS. And so it can feels very weird at
first. But once I adopted it every other package management system felt
completely wrong. Also nix shines when it goes to reproducibility.

One thing that [nix][nix] is notoriously lacking is good and usable
documentation. Additionally its learning curve is very steep. That's why I
intend to share now and then some simple tricks that, I hope, will contribute
to make this great technology more approachable.

Today, I'll show you how I start a local `mariadb` server when I develop web
services.

Assuming you have a working [nix][nix] installation, save this snippet in a
`nix-shell.nix` file and start `nix-shell`. [Nix][nix] will starts start a
local server one can connect locally with the command `mysql`.

```nix
let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [ pkgs.mariadb ];
  shellHook = ''
    MYSQL_BASEDIR=${pkgs.mariadb}
    MYSQL_HOME="$PWD/mysql"
    MYSQL_DATADIR="$MYSQL_HOME/data"
    export MYSQL_UNIX_PORT="$MYSQL_HOME/mysql.sock"
    MYSQL_PID_FILE="$MYSQL_HOME/mysql.pid"
    alias mysql='mysql -u root'

    if [ ! -d "$MYSQL_HOME" ]; then
      # Make sure to use normal authentication method otherwise we can only
      # connect with unix account. But users do not actually exists in nix.
      mysql_install_db --no-defaults --auth-root-authentication-method=normal \
        --datadir="$MYSQL_DATADIR" --basedir="$MYSQL_BASEDIR" \
        --pid-file="$MYSQL_PID_FILE"
    fi

    # Starts the daemon
    # - Don't load mariadb global defaults in /etc with `--no-defaults`
    # - Disable networking with `--skip-networking` and only use the socket so 
    #   multiple instances can run at once
    mysqld --no-defaults --skip-networking --datadir="$MYSQL_DATADIR" --pid-file="$MYSQL_PID_FILE" \
      --socket="$MYSQL_UNIX_PORT" 2> "$MYSQL_HOME/mysql.log" &
    MYSQL_PID=$!

    finish()
    {
      mysqladmin -u root --socket="$MYSQL_UNIX_PORT" shutdown
      kill $MYSQL_PID
      wait $MYSQL_PID
    }
    trap finish EXIT
  '';
}
```

The script itself is not very complicated to understand.

First it creates a shell with `mariadb` as a dependency. The `buildInputs`
attribute is a list of packages that needs to be brought in scope for the
shell. Here, any package available in
[nixpkgs](https://search.nixos.org/packages) can be added to this list. Once
started, the shell will thus have the `mariadb` binaries in its path.

Then it executes the `shellHook`: a `bash` script. This script initializes the
`mariadb` data directory in `$(pwd)/mariadb/data` then starts the server and
stores its `PID` in an environment variable.

When the shell ends the server is shutdown. If `nix-shell` it started again,
the previous database will be reused.

That's it for this simple trick. That pattern can easily be reused. Using a
`shellHook` with some dependencies is incredibly handy and can have a lot of
usages.

[Nix]: https://nixos.org
