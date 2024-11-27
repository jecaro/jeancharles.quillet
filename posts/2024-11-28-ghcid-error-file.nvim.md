---

title: "ghcid-error-file.nvim: A ghcid plugin for neovim"
lastmod: 2024-11-28

---

In [my previous 
post](/posts/2024-09-04-Haskell-dev-workflow-with-ghcid-and-neovim.html), I 
presented a simple workflow for a fast compilation loop using [ghcid] and 
`neovim`. 

Unfortunately, this doesn't work with multi-package projects. This is because 
GHC outputs filenames relative to a package and not relative to where it runs 
(see [this GHC issue](https://gitlab.haskell.org/ghc/ghc/-/issues/15680) and 
also [this cabal issue](https://github.com/haskell/cabal/issues/6670)). That 
means that `neovim` could be able to load the error file in the same way as 
previously, but it would not be able to find the files in the quickfix list, 
which would not be very useful.

To solve this, I came up with a relatively simple solution. I wrote some lua 
code to manually parse the error file and fill up the quickfix list prepending 
the relative path of the package currently loaded by [ghcid]. I mapped the lua 
function to a `neovim` user command, and it results in a very similar workflow 
as before. One just need to give to `neovim` the path to the package worked on.

See below for an example:

![](/images/ghcid-error-file.gif)

This code was present in my personal `neovim` config for a while. But to share 
it, I thought it would be easier to make a plugin out of it. And so I did. The 
plugin includes both approaches and is then working out of the box for both 
cases.

You can find it on [GitHub][ghcid-error-file.nvim].

[ghcid-error-file.nvim]: https://github.com/jecaro/ghcid-error-file.nvim
[ghcid]: https://github.com/ndmitchell/ghcid

