---

title: Multiple components in ghci is finally here
lastmod: 2025-10-01

---

Loading multiple components in `ghci` is finally possible with `ghc 9.4` and 
`cabal 3.12`. They have been available for a while both in `nixos-2411` and 
`ghcup`. There is no reason not to use it.

This is a big deal because it directly affects the length of the feedback loop. 
I have been writing about it 
[in](/posts/2024-09-04-Haskell-dev-workflow-with-ghcid-and-neovim.html) 
[the](/posts/2024-11-28-ghcid-error-file.nvim.html) 
[past](/posts/2025-02-21-Fast-compilation-loop-with-tmux-and-neovim.html). And 
I do think that a short feedback loop is the key to a good productivity.

Back to the story, until recently, `ghci` were unable to load multiple 
components, say an executable, like a test suite for example, and the library 
it depends on at the same time. And `ghci` is the centerpiece to get the 
fastest feedback loop when working with Haskell. Until now, working on a 
feature usually consists in doing some kind of back and forth between working 
on the library that contains the actual code and working on the test suite to 
make sure it works, restarting `ghci` between each iteration. This is far from 
being ideal and makes the feedback loop way longer than it needs to be.

That's only a concern for `cabal` users by the way. `stack` have had a 
workaround for this for a while (see [the 
documentation](https://docs.haskellstack.org/en/stable/commands/ghci_command/) 
for `stack ghci`). What it does is merging all `ghc` options of the components 
before loading them in `ghci`. Basically, it is like creating a fake component 
which is the union of all wanted components. Pretty basic, but it has been an 
effective workaround for this issue.

But now it is possible to do it as well in `cabal`. At last, hurrah 🎉

Now, how does that work in practice?

The first thing is of course to have at least the versions of `ghc 9.4 `and 
`cabal 3.12`.

Then `cabal` must be configured to enable the feature. When trying to load two 
components with `cabal`. It outputs a pretty informative message:

```
$ cabal repl test:my-app-tests lib:my-app
Error: [Cabal-7076]
Cannot open a repl for multiple components at once. The targets 'my-app' and 'tests' refer to different components..

Your compiler supports a multiple component repl but support is not enabled.
The experimental multi repl can be enabled by
  * Globally: Setting multi-repl: True in your .cabal/config
  * Project Wide: Setting multi-repl: True in your cabal.project file
  * Per Invocation: By passing --enable-multi-repl when starting the repl
```

As for me, I just turn it on globally. It is just too useful.

Once `cabal` is configured, that's it really. If you try again that last 
command, that should work. Then after you edit a file in any of the loaded 
components, all required files will be recompiled by a simple `:r` in `ghci`.

The feedback loop I explained 
[here](/posts/2024-09-04-Haskell-dev-workflow-with-ghcid-and-neovim.html) works 
out of the box. Interestingly, it seems that it sometime makes `ghc` output the 
absolute path of the files that contains errors. [This 
workaround](/posts/2024-11-28-ghcid-error-file.nvim.html) is thus not always
needed which is also awesome.

One caveat though, `ghcid` is able to run a command after the compilation 
succeeds. It is super useful to run the tests on any change for example. Let's 
try this:

```
$ ghcid -c "cabal repl test:my-app-tests lib:my-app" -T :main
Command is not supported (yet) in multi-mode
```

Oh no, that doesn't work.

But there is a way to work around this. I learned it in [this 
video](https://www.youtube.com/watch?v=B1WFMave-r4)
(recommended watch to see the feature in action). Instead or running a command, 
we'll just execute the `main`. For example, if the `main` function is in the 
`Main` module, we can do this:

```
$ ghcid -c "cabal repl test:my-app-tests lib:my-app" -T Main.main
```

This works fine. Watch out that the executable must come first in the list of 
the components for some reason. If one needs to pass arguments to the main 
function, it is possible to use the `:set` command in `ghci`. 

For example:

```
$ ghcid -c "cabal repl test:my-app-tests lib:my-app" -s ":set args -p /testpattern/" -T Main.main
```

That's it. Once one tried it, it is hard to believe we have been waiting for so 
long for this.

Note: [cabal 
documentation](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cmdoption-enable-multi-repl)

