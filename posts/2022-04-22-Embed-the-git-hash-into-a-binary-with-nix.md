---

title: Embed the git hash into a binary with nix

---

When debugging it's important to have a reliable way to identify a binary. How
many times when digging into a bug, I found out that the version I debug and
the version used by the user reporting the bug mismatch. It makes the
debugging session more difficult than it needs to be.

This issue is usually solved by embedding in the binary something to identify
the exact sources used to build it such, as the git hash. It is common to have
it as a reply to a `--version` switch for instance. Having that information
makes it way easier to reproduce bugs.

Personally and at work I've been more and more into nix. And something we had a
hard time figuring out is how to achieve this kind of things for a nix
derivation.

A solution to embed the git hash into the binary is described
[here](https://discourse.nixos.org/t/accessing-git-directory-in-flake-with-local-source/17370/4).

As it is addressed to knowledgeable users, I think it would be useful to have a
concrete example here along a few explanations.

Checkout [this repo](https://github.com/jecaro/test-git-hash).

It contains [a simple Haskell
program](https://github.com/jecaro/test-git-hash/blob/master/Main.hs) that
outputs the git hash using the Haskell package [gitrev][gitrev]. The git hash
is embedded into the binary using template Haskell. Under the hood, it calls
the git command at compile time to get that information.

Let's start a few commands to see what is going on.

``` bash
$ nix-shell
....
$ cabal run -v0 test-git-hash
031aacbc162305f6d3e65efa5a37ed072214e171
```

Here, no problem, into the nix shell the git command is rightly called and
returns the hash of the sources.

Now let's try with `nix-build`:

``` bash
$ nix-build
this derivation will be built:
  /nix/store/nadpbm5b0a15nw59kjiwzq3p0sdg3ndg-test-git-hash-0.1.0.0.drv
building '/nix/store/nadpbm5b0a15nw59kjiwzq3p0sdg3ndg-test-git-hash-0.1.0.0.drv'...
setupCompilerEnvironmentPhase
...
checking for references to /build/ in /nix/store/3bci3qjrx2kz414g2wrmwjylx5aqiqik-test-git-hash-0.1.0.0-doc...
/nix/store/k9h17vad8p8lb0qlg7zrrqc4nmr22mv6-test-git-hash-0.1.0.0
$ ./result/bin/test-git-hash
UNKNOWN
```

Oh no ! What's going on here ? Let's have a look at the derivation itself first:

``` bash
$ nix-instantiate default.nix
warning: you did not specify '--add-root'; the result might be removed by the garbage collector
/nix/store/qg6xl4lyymygwzl5s2fj26kjgyj1nvig-test-git-hash-0.1.0.0.drv
$ nix show-derivation /nix/store/qg6xl4lyymygwzl5s2fj26kjgyj1nvig-test-git-hash-0.1.0.0.drv
{
  "/nix/store/qg6xl4lyymygwzl5s2fj26kjgyj1nvig-test-git-hash-0.1.0.0.drv": {
    ...
    "env": {
      ...
      "name": "test-git-hash-0.1.0.0",
      "nativeBuildInputs": "/nix/store/x4zfzcl0188si8wf0c2xlgx3mb21ljwh-ghc-8.10.7 /nix/store/4ng3fj1n7hv98f9r99zwdvkf8w02v64m-remove-references-to",
      "out": "/nix/store/j7vid7kfpz2zxpb6cgv4nls2b4fyiagk-test-git-hash-0.1.0.0",
      ...
    }
  }
}
```

Here we can see that the git command is not into the list of build tools
(identified here with the key `nativeBuildInputs`). That's why the invocation
of
[gitHash](https://hackage.haskell.org/package/gitrev-1.3.1/docs/Development-GitRev.html#v:gitHash)
fails.

OK let's fix this. The git command can be added easily enough using
`haskell.lib.addBuildTool`. Here is our new derivation `default-with-git.nix`

``` nix
{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "test-git-hash" ./. {};
in
  pkgs.haskell.lib.addBuildTool drv pkgs.git
```

Now let's try it:

``` bash
$ nix-build ./default-with-git.nix
this derivation will be built:
  /nix/store/bcavqkvp9z3m3wpqqm2zm6qy7ic7738q-test-git-hash-0.1.0.0.drv
building '/nix/store/bcavqkvp9z3m3wpqqm2zm6qy7ic7738q-test-git-hash-0.1.0.0.drv'...
setupCompilerEnvironmentPhase
...
checking for references to /build/ in /nix/store/vq96lkglkbngabfmcn948xjvd9v8lb65-test-git-hash-0.1.0.0-doc...
/nix/store/qgh9ipqwdk58zclmdsqanhyhcj4gbb15-test-git-hash-0.1.0.0
$ ./result/bin/test-git-hash
031aacbc162305f6d3e65efa5a37ed072214e171
```

OK that works fine. Are we done yet ? Let's see.

If your work ends up in production there is a good chance that your package is
part of some kind of package set, and more important it might be pinned to a
specific version with niv, flake or anything similar. Or it can also be part of
nixpkgs, or even NixOS ? In both cases, same story, the package will be pinned
and part of some kind of bigger package set.

Does it change anything for our problem ? Let's see and simulate this with the
file `package-set.nix`:

``` nix
{ pkgs ? import <nixpkgs> {} }:
let
  testGitHashSrc = fetchGit {
      url = "https://github.com/jecaro/test-git-hash";
      rev = "362f4267885d28cfdb5b2dd719e3533b50c437f5";
    };
in
  {
    test-git-hash = (import testGitHashSrc) {};
  }
```

Here we reference the `test-git-hash` sources and import the derivation it
defines in the field `test-git-hash` of the returned set. Now let's try to
build the derivation inside:

``` bash
$ nix-build ./package-set.nix -A test-git-hash
this derivation will be built:
  /nix/store/qshr36mhssxpa1cvnmvy8ng0lf4gnj3s-test-git-hash-0.1.0.0.drv
building '/nix/store/qshr36mhssxpa1cvnmvy8ng0lf4gnj3s-test-git-hash-0.1.0.0.drv'...
setupCompilerEnvironmentPhase
...
checking for references to /build/ in /nix/store/6yrv8iq8hkis3fl491dr0851rlkl1w4y-test-git-hash-0.1.0.0-doc...
/nix/store/adcq98pih1l5fw2sqkz4yzczynp0bhiw-test-git-hash-0.1.0.0
$ ./result/bin/test-git-hash
UNKNOWN
```

What's going on here ? Ah yes, that git command is not available in our
`default.nix`. No problem, let's add it afterward in `package-set-with-git.nix`
and let's try again.

``` bash
$ nix-build package-set-with-git.nix -A test-git-hash
this derivation will be built:
  /nix/store/p04g7ldmv7jdg8ninp9xxvx8lq9771lq-test-git-hash-0.1.0.0.drv
building '/nix/store/p04g7ldmv7jdg8ninp9xxvx8lq9771lq-test-git-hash-0.1.0.0.drv'...
setupCompilerEnvironmentPhase
...
checking for references to /build/ in /nix/store/3gc8rxvj1ir9hrjhxirbl9x62hxxj6aq-test-git-hash-0.1.0.0-doc...
/nix/store/a1gzk89sc6vdb8jab3k3a498133pk4rb-test-git-hash-0.1.0.0
$ ./result/bin/test-git-hash
UNKNOWN
```

Oh no ! Still not working ! What is going on this time ? nix fetches the
sources, then wipes up the git directory before starting the build. So the git
command is available but returns some kind of error when called. Could we ask
nix to not delete that directory ? Unfortunately this deletion is necessary as
the git repository is not [fully
deterministic](https://github.com/NixOS/nixpkgs/issues/8567).

So how to get this git hash?

Here is a solution:

``` nix
{ pkgs ? import <nixpkgs> {} }:
let
  testGitHashSrc = fetchGit {
      url = "https://github.com/jecaro/test-git-hash";
      rev = "362f4267885d28cfdb5b2dd719e3533b50c437f5";
    };
  fakeGit = pkgs.writeShellScriptBin "git"
    ''
      echo ${testGitHashSrc.rev}
    '';
  test-git-hash-drv = (import testGitHashSrc) {};
in
  {
    test-git-hash = pkgs.haskell.lib.addBuildTool test-git-hash-drv fakeGit;
  }
```

In this file, we create a script called `git`. When called that script returns
the git hash of the sources fetched with `fetchGit`. Then we add that script as
an additional build tool to our final derivation. Our script will be called by
[gitrev][gitrev] in place of the actual git.

Let's try again:

``` bash
$ nix-build ./package-set-with-fake-git.nix -A test-git-hash
this derivation will be built:
  /nix/store/zkshicqchhxcr1q39y9i4q9ddmpx0fc6-test-git-hash-0.1.0.0.drv
building '/nix/store/zkshicqchhxcr1q39y9i4q9ddmpx0fc6-test-git-hash-0.1.0.0.drv'...
setupCompilerEnvironmentPhase
...
checking for references to /build/ in /nix/store/xass9crjyqpvmqz86k54y69raqsby1bk-test-git-hash-0.1.0.0-doc...
/nix/store/94pd70ml8njyvm03j6yyy6nb5l2mkxqv-test-git-hash-0.1.0.0
$ ./result/bin/test-git-hash
362f4267885d28cfdb5b2dd719e3533b50c437f5
```

Yes this time it works !

Now let's step a step back. At the beginning the approach was to get the git
hash from *inside* the sources so to speak. We've seen that for
reproductibility the `.git` directory cannot be made available to get the git
hash. So there is no way around, that work must be somehow done from *outside*
the sources. When fetching the sources we know for sure that git hash, so we
need to take advantage of this and make it available to our derivation. That
can be done easily with that fake git approach.

That was quite a journey. The lesson to be taken out of this is that
reproductibility is never as simple as it looks and there is a lot in our way
when we try to take that path. Even granted things such as having a git hash in
a binary can have unforeseen implications as we've seen.

There are probably other ways to do this. But that solution is easy and
flexible enough to respond to most use cases. Feel free to [reach
out](/pages/contact.html) if you find another one.

[gitrev]: https://hackage.haskell.org/package/gitrev
