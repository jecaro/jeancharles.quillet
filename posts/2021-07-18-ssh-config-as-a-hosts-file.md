---

title: ssh config as a hosts file

---

I'm late adopter of bash completion. I tried it a few times but I always gave
up because I found it slow. And I hate when it blocks my shell waiting for a
command to finish. I've recently upgrade my computers and on recent hardware,
it now runs pretty smoothly. So it's time to give it another go.

One feature I use the most is `ssh` host completion. At work I work with a lot
of VMs. There is no way I can remember all of their IPs but it's pretty easy to
put a name on each machine relevant enough to remember.

This has to be done in `~/.ssh/config`, for example:

```
Host webserver
    User myuser
    Hostname xx.xx.xx.xx
```

with theses lines, calling:

```
$ ssh webserver
```

is the same as calling:

```
$ ssh myuser@xx.xx.xx.xx
```

That hostname alias for `ssh` can be auto-completed hitting tab which makes it
super useful. This trick also works for any `ssh` aware program like: `rsync`,
`sshfs`, `scp` etc...

But what if I want to use that same hostname with a non `ssh` aware program ? I
would need a way to convert that hostname into the corresponding IP address of
the `~/.ssh/config` file.

It seems to be possible to have a user `/etc/hosts` file using the environment
variable `HOSTALIASES` as described in [`man
gethostbyname`](https://man7.org/linux/man-pages/man3/gethostbyname.3.html).
I've tried this approach but I never managed to have it working properly. And
one problem with this solution is that you now have to maintain two files:
`~/.ssh/config` and that `HOSTALIASES` file.

So I came up with a small script:
[`ssh-host.sh`](https://github.com/jecaro/dotfiles/blob/master/bin/bin/ssh-host.sh)
to output the `Hostname` corresponding to a `Host` in `~/.ssh/config`.

```
$ ssh-host.sh webserver
xx.xx.xx.xx
```

With that script in your `PATH` it is now easy to write commands like:

```
$ curl https://$(ssh-host.sh webserver)
```

and have it using the IP address of the `~/.ssh/config` file.

This is already cool but, of course, that trick wouldn't be complete without a
bit of auto-completion !

``` bash
# Completion for ssh-host.sh
_complete()
{
  if [ "${#COMP_WORDS[@]}" != "2" ]; then
    return
  fi

  HOSTS=$(cat ~/.ssh/config | awk '/^Host / { print $2 }')
  COMPREPLY=($(compgen -W "$HOSTS" "${COMP_WORDS[1]}"))
}

complete -F _complete ssh-host.sh
```

Add these lines to your `~/.bashrc` and the `ssh-host.sh` script now complete
just like `ssh` does.

With this simple script, I can now use the `~/.ssh/config` file as a `/etc/hosts`
file at the cost of calling that intermediate script. And it is super handy in
my workflow !
