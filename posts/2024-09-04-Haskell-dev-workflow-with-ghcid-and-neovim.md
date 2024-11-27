---

title: Haskell dev workflow with ghcid and neovim
lastmod: 2024-11-28
toc: true

---

# Introduction

The tooling in the Haskell ecosystem has greatly improved these last years. 
Especially, the introduction of [Haskell Language Server][hls] dramatically 
lowered the barrier to entry for newcomers. But despite its undeniable quality, 
[HLS][hls] still chokes on big codebases such as the ones we find in 
professional environments. 

Also, for a fast feedback loop, it is not always the most suited tool to reach 
for. And having the fastest feedback loop is extremely important for a good 
developer experience. 

# [ghcid]

The tool that always works, no matter what, even on big codebases, is `ghcid`. I 
find myself constantly getting back to it for my feedback loops. 

`ghcid` basically starts a `ghci` session and watches for changes in the loaded 
files. Just save a file with your editor, it instantaneously reloads it and 
prints the eventual errors or warnings, depending on your project 
configuration. It is extremely versatile and can adapt to many workflows. 

This is how I use it.

![](/images/ghcid-manual.png)

I open a `tmux` session with two vertical panes. On the left, I have `neovim` 
with my Haskell project. On the right, I have `ghcid` running. When I save a 
file, `ghcid` reloads the file and prints the errors. I can then look at the 
errors and fix them in `neovim`, save again, repeat.

That's pretty cool. And this kind of setup is super general by the way. It 
basically works with any language. Have an editor on the left, and runs a 
compilation loop on the right. There are a lot of tools that watch for file 
changes and run a command when a file is modified. I personally like 
[`entr`][entr] and [`feedback`][feedback] for this kind of task.

But anyway, we can do better. `neovim` has [a built-in feature][errorformat] 
for this kind of compilation loop. If your compilation task can produce a file 
(and `ghcid` can do that), `neovim` can parse it and populate the quickfix list 
with the errors and warnings. Then one can browse the quickfix list and fix the 
errors one after another.

This makes the feedback loop even faster: I save a Haskell file in my editor, 
`ghcid` automatically updates the error file, then, with one single command 
(`:cfile` or `:cf`), I reload the file in `neovim` and jump to the first error. 

Simple, easy, fast.

What I like with this workflow is also that it is not intrusive at all. It 
doesn't force me to fix any issue right now. I can jump to the error whenever I 
want while having the compilation state of my project right under my eyes all 
the time.

![](/images/ghcid-errorformat.gif)

# Configuration

Now let's talk about the required configuration for this to work. First we must 
configure `ghcid`. This can be done at the root of the project in a file called 
`.ghcid`.

```bash
$ echo "-a -o errors.err" > .ghcid
```

This tells `ghcid` to output the errors in the file `errors.err`, the default 
error file for `neovim` (see [`:h errorfile`][errorfile]). `-a` is not strictly 
necessary. It allows executing REPL commands in the `ghci` session. I find it 
sometimes useful. More information about this can be found on 
[here](https://github.com/ndmitchell/ghcid#evaluation).

Now we only need to tell `neovim` how to parse the error file. This is done 
with the option `errorformat` (see [`:h errorformat`][errorformat]). A good 
place to set it is in the `ftplugin` file for Haskell. That file will be 
sourced whenever a Haskell file is opened.


```bash
$ cat ~/.config/nvim/after/ftplugin/haskell.lua
```

```lua
vim.opt_local.errorformat =
    -- %W multi-line warning
    -- For some reason, %m doesn't work with %\\?, we need to add two lines for
    -- each case
    "%W%f:(%l\\,%c)-(%e\\,%k): warning: %m," ..
    "%W%f:(%l\\,%c)-(%e\\,%k): warning:," ..
    "%W%f:%l:%c-%k: warning: %m," ..
    "%W%f:%l:%c-%k: warning:," ..
    "%W%f:%l:%c: warning: %m," ..
    "%W%f:%l:%c: warning:," ..

    -- %E multi-line error
    "%E%f:(%l\\,%c)-(%e\\,%k): error: %m," ..
    "%E%f:(%l\\,%c)-(%e\\,%k): error:," ..
    "%E%f:%l:%c-%k: error: %m," ..
    "%E%f:%l:%c-%k: error:," ..
    "%E%f:%l:%c: error: %m," ..
    "%E%f:%l:%c: error:," ..

    -- %Z Ends a multi-line message. We end it on the first line of the carret
    -- message.
    "%Z %\\+|%.%#," ..

    -- Continue a multi-line message
    "%C    %m," ..

    -- Swallow everything else
    "%-G%.%#"
```

And that's it. Now when calling the command `:cf` in `neovim`, it will read the 
error file, parse it according to the `errorformat` option and jump to the first 
error.

Then one can browse the errors with the usual `quickfix` commands: `:cnext`, 
`:cprev`, `:cfirst`, `:clast`, etc...

# Conclusion

This simple trick allows having a fast feedback loop when working with Haskell.

One shortcoming of this setup is that it doesn't work when working in a 
multi-package project. This is because the errors returned by GHC are relative 
to the project. This can be easily solved by `cd` into the directory before 
starting `ghcid`.

I have a better solution for this as well, but it is slightly more involved and 
requires a bit of lua code. I will write about it in a future post.

That's it for today. Let me know if you find this useful.

28-11-2024: Read the follow-up in [this next 
post](/posts/2024-11-28-ghcid-error-file.nvim.html).

# Related tools

- [ghcid.nvim] : A `neovim` plugin for `ghcid`. I was using it at some point. 
  But I like my solution better because it is simpler and only use `neovim` 
  built-in features.
- [ghcidwatch] : A rewrite of `ghcid` in rust developed by Mercury. I have 
  never tried it, but it seems that it is able to write an error file in the 
  same format as `ghcid`. It should then work with this setup.

[ghcid]: https://github.com/ndmitchell/ghcid
[ghcid.nvim]: https://github.com/ndmitchell/ghcid/tree/master/plugins/nvim
[hls]: https://github.com/haskell/haskell-language-server
[ghcidwatch]: https://github.com/MercuryTechnologies/ghcidwatch
[errorformat]: https://neovim.io/doc/user/quickfix.html#errorformat
[errorfile]: https://neovim.io/doc/user/options.html#'errorfile'
[entr]: https://github.com/eradman/entr/
[feedback]: https://github.com/NorFairKing/feedback

