---

title: Viming Haskell in 2021
toc: true

---

# Introduction

This post describes my Haskell/Vim setup for work and fun. The full setup is
available on [github][github].

# Language Server Protocol

I use [neovim][neovim] because it has a built-in implementation of Language
Server Protocol (LSP). It might not be as good as the other solutions such as
theses plug-ins: [ALE](https://github.com/dense-analysis/ale),
[Coc](https://github.com/neoclide/coc.nvim),
[vim-lsp](https://github.com/prabirshrestha/vim-lsp). Actually I have no idea.
But I tend to prefer built-in features. Firstly because it makes my experience
more consistent when I switch between machines. And also I think it will have
more chance to get maintained in the long run so won't have to switch to
another solution at some point.

[Neovim][neovim] has a built-in implementation of LSP since its version 5.x. As
the time of writing it is not released yet but it is possible to use one of the
nightly versions available on various places
([snap](https://snapcraft.io/nvim), [github nightly
binaries](https://github.com/neovim/neovim/releases),
[AUR](https://aur.archlinux.org/packages/neovim-git/), etc...).

As for me, I use [NixOS](https://nixos.org/) and I have an overlay in
`~/.config/nixpkgs/overlays/neovim.nix`

```
self: super: {
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: {
    version = "nightly";
    src = super.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "nightly";
      sha256 = super.lib.fakeSha256;
    };
    buildInputs = oldAttrs.buildInputs ++ [ super.tree-sitter ];
  });
}
```

Install it with:

```
nix-env -iA nixos-unstable.neovim
```

It will fail and complain about the bad hash but it will give you the right
hash to put in the attribute `sha256`.

```
installing 'neovim-nightly'
these derivations will be built:
  /nix/store/ciwsbv9av383j9dq04r761088wkj1sm2-source.drv
  /nix/store/kfz14kflipjyh10lwmcs5lfc7yw4yflw-neovim-unwrapped-nightly.drv
  /nix/store/82a9s16dxgxr2001gzqzcjz5kl7kpjn4-neovim-nightly.drv
building '/nix/store/ciwsbv9av383j9dq04r761088wkj1sm2-source.drv'...

trying https://github.com/neovim/neovim/archive/nightly.tar.gz
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   122  100   122    0     0    450      0 --:--:-- --:--:-- --:--:--   448
100 9839k    0 9839k    0     0  1392k      0 --:--:--  0:00:07 --:--:-- 1568k
unpacking source archive /build/nightly.tar.gz
hash mismatch in fixed-output derivation '/nix/store/qkx75z9mbnpq8rm88mk56kgavamk3ar5-source':
  wanted: sha256:0000000000000000000000000000000000000000000000000000
  got:    sha256:0f9g33vjfp1dhgm6skyvf6qgr2z4anwl5pz1dqp55fpkh8r1a0ws
cannot build derivation '/nix/store/kfz14kflipjyh10lwmcs5lfc7yw4yflw-neovim-unwrapped-nightly.drv': 1 dependencies couldn't be built
cannot build derivation '/nix/store/82a9s16dxgxr2001gzqzcjz5kl7kpjn4-neovim-nightly.drv': 1 dependencies couldn't be built
error: build of '/nix/store/82a9s16dxgxr2001gzqzcjz5kl7kpjn4-neovim-nightly.drv' failed
```

So replace the hash and restart the command and it should work this time.

# Configuration

Even if LSP is built-in in [neovim][neovim], it still requires a few external
plug-ins to make the experience simple and straightforward.

```
call plug#begin('~/.vim/plugged')
...
if has('nvim-0.5.0')
    " Good default for many languages
    Plug 'neovim/nvim-lspconfig'
    " Autocompletion framework for LSP
    Plug 'nvim-lua/completion-nvim'
endif
...
call plug#end()

```

I use guard around these plug-ins to make my configuration compatible with
vanilla vim.

And here is the main configuration in lua.

```
" Activate language servers on neovim
if has('nvim-0.5.0')
lua << EOF
  local nvim_lsp = require('lspconfig')

  local on_attach = function(client)
      -- Activate completion
      require'completion'.on_attach(client)

      -- Mappings
      local opts = { noremap=true }
      vim.api.nvim_buf_set_keymap(0, 'n', '<c-]>',
          '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
      vim.api.nvim_buf_set_keymap(0, 'n', 'K',
          '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
      vim.api.nvim_buf_set_keymap(0, 'n', 'gd',
          '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
      vim.api.nvim_buf_set_keymap(0, 'n', 'gD',
          '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
      vim.api.nvim_buf_set_keymap(0, 'n', 'gr',
          '<cmd>lua vim.lsp.buf.references()<CR>', opts)

      vim.api.nvim_buf_set_keymap(0, 'n', '<leader>ca',
          '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
      vim.api.nvim_buf_set_keymap(0, 'n', '<leader>cr',
          '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
      vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ls',
          '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)

      vim.api.nvim_buf_set_keymap(0, 'i', '<C-s>',
          '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)

      -- autoformat only for haskell
      if vim.api.nvim_buf_get_option(0, 'filetype') == 'haskell' then
          vim.api.nvim_command[[
              autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
      end
  end

  nvim_lsp.hls.setup({
      on_attach = on_attach,
      settings = {
          haskell = {
              hlintOn = true,
              formattingProvider = "fourmolu"
          }
       }
  })

EOF
endif
```

Basically what it does is activate [Haskell Language Server][hls], turn on some
options and set shortcuts.

# Usage

Here is the result:

![](/images/nvim-lsp.png)

The LSP should start as soon as you open a Haskell file assuming that you have
`haskell-language-server` in your path.

The main shortcuts I use are:

* `K`: to show the type of the symbol under the cursor
* `CRTL-]`: to go to the definition of the symbol
* `<leader>-ca`: code action. This one is very useful. Type it on an undefined
  symbol and it will propose you to add the import line automatically, same
  applies for extensions. It can also fill up type holes in some cases, write
  inferred types of top level functions, and even apply
  [hlint](https://github.com/ndmitchell/hlint) hints.

# Handy things to know

To find out if LSP is running:

```
:lua print(vim.inspect(vim.lsp.buf_get_clients()))
```

It should output a big JSON object. If it returns `{}` that means that LSP is
not running for the current buffer. To find out what is going on, it is sometimes
useful to run the language server from the command line and read its output.
Very often it is a missing or bad `hie.yaml` file for the project. Find out more
about `hie.yaml` files on the
[README.md](https://github.com/haskell/haskell-language-server#configuring-your-project-build)
of `haskell-language-server`.

Sometimes the server crashes. Restarting it is just a matter of opening the file
again, so `:e` should do it.

In [my configuration][github], I also have a status line activated. It is not a
mandatory thing to have. But sometimes `haskell-language-server` can take a
long time to initialize and this status line shows its progression so I know
the server is not crashed and is just initializing.

# Ghcid

LSP helps me mainly to remember the types of the symbols and to browse the
source. But for compilation, I rely heavily on
[ghcid](https://github.com/ndmitchell/ghcid).  This tool runs a command that
starts `ghci`, like `cabal repl` or `stack repl` and when the source code has
changed it triggers a `:r` and show you the first error. The only one we
actually care about.

It's got a [neovim
plugin](https://github.com/ndmitchell/ghcid/tree/master/plugins/nvim) which
works quite well out of the box and loads the errors in the quicklist.

# Conclusion

With [haskell-language-server][hls] it has never been so easy to hack in
Haskell in no time. I'm very impressed on how it changed my workflow and helped
me to get more productive. I'm looking forward to seeing which features new
versions of [haskell-language-server][hls] will bring.

[neovim]: https://neovim.io/
[github]: https://github.com/jecaro/dotfiles/tree/master/vim
[hls]: https://github.com/haskell/haskell-language-server
