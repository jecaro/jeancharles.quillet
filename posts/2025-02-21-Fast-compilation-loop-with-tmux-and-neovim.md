---

title: Fast compilation loop with tmux and neovim
lastmod: 2025-02-21

---

Following up my two previous posts about getting a fast compilation loop when 
working with Haskell in neovim (see 
[here](/posts/2024-09-04-Haskell-dev-workflow-with-ghcid-and-neovim.html) and 
[here](/posts/2024-11-28-ghcid-error-file.nvim.html)), today I want to share a 
more general approach that works with pretty much any compiled language. I have 
successfully used it with: C, C++, Java, Rust, Haskell and Zig with no 
modification whatsoever.

But first, a short demo of me working on [one of my projects][pomodozig]:

![](/images/tmux-neovim-workflow.gif)

Basically, I have a tmux session with two panes. On the left, I have my code 
editor neovim. On the right, I have the compiler running each time I save a 
file.

When the compiler finds an error in my code, with a shortcut I find the error 
in the compilation panel and yank it. Back to neovim, with another shortcut I 
jump straight to the error in my code, fix it, save the file, rinse and repeat.

# Moving between panes

By default, moving the focus between panes in tmux is done with `CTRL-b` 
followed by an arrow key (as for me, I use `CTRL-Space` as my tmux prefix). In 
neovim, to move the focus between windows, one uses `CTRL-w` followed by a vim 
direction key: `h`, `j`, `k` and `l`.

I find it cumbersome. To make it easier, I use the neovim plugin [tmux.nvim]. 
With it, one can move the focus with `CTRL-h`, `CTRL-j`, `CTRL-k` and `CTRL-l` 
seemingly between tmux panes and neovim windows.

Check out the [README.md][tmux.nvim] of the plugin for details on the 
configuration.

# Compilation pane

For the compilation pane, I run the compiler through [feedback]. By default, 
this tool watches for changes in files tracked by git and runs a command on any 
change. 99% of the time, this is what I want. For the remaining 1%, [feedback] 
has a few options to define the files to watch. Have a look at [its 
repository][feedback] for more information.

Now, to find the error in the compilation pane, tmux is able to search for 
regexes. I use this feature in my `~/.config/tmux/tmux.conf` and map such 
searches to keybindings.

```tmux
# Find errors and warnings in the current pane
bind C-e copy-mode \; send -X search-backward "[^\s]+:[^\s]+: (error|Error):"
bind C-w copy-mode \; send -X search-backward "[^\s]+:[^\s]+: (error|Error|warning|Warning):"
```

With this configuration, if I hit `CTRL-Space` (my tmux prefix) followed by 
`CTRL-e`, tmux will search in the current pane for an error and highlight it. 
With `CTRL-Space` followed by `CTRL-w`, tmux will search for an error or a 
warning.

Once the error has been found, I can yank it with `Enter`. The error string is 
now available in the system clipboard and, in neovim, in the `+` register.

# Neovim pane

Back to neovim, to jump to the error I can enter command mode, type `:edit ` 
and paste the error string with `CTRL-SHIFT-v` or `CTRL-r-+`.

```
:edit some/file.zig:12:10: error:
```

But don't hit `Enter` yet, that's not a proper vim command. I still need to 
edit the line such as:

```
:edit +12 some/file.zig
```

or even simpler:

```
:edit some/file.zig | 12
```

Now hit `Enter` and neovim will jump to the file and line number.

This is already better than trying to find the error by hand, open the file and 
scroll to the right line number. But we can do better.

# Lua to the rescue

Here is a Lua module that can go in your `~/.config/nvim/lua` directory. Let's 
call it `parse_error_string.lua` for example.

```lua
local M = {}

local parse_error_string = function(str)
  -- src/Parser.hs:(30,3)-(32,11): warning:
  local file, line = str:match('([^:]+):%((%d+),%d+%)%-%(%d+,%d+%): .+')

  -- src/Parser.hs:10:3-8: error:
  if not file then
    file, line = str:match('([^:]+):(%d+):%d+%-%d+: .+:')
  end

  -- src/Model.hs:4:12: error:
  if not file then
    file, line = str:match('([^:]+):(%d+):%d+: .+:')
  end

  -- /absolute/path/to/some/java/source.java:229: error:
  if not file then
    file, line = str:match('([^:]+):(%d+): .+:')
  end

  return file, line
end

local function file_exists(filename)
  return vim.fn.filereadable(filename) == 1
end

local function open_file_line(filename, line)
  local line_arg = ''
  if line then
    line_arg = '+' .. line
  end

  vim.cmd(table.concat({ 'edit', line_arg, filename }, ' '))
end

function M.open_error(error_str)
  local filename, line = parse_error_string(error_str)

  if file_exists(filename) then
    open_file_line(filename, line)
  else
    vim.notify('Could not open file: ' .. filename, vim.log.levels.ERROR)
  end
end

function M.open_yanked_error()
  local error_str = vim.fn.getreg('+')
  M.open_error(error_str)
end

return M
```

The code is pretty straight forward. Most of the work is done in the parsing 
function `parse_error_string` which extract the filename and the line from the 
error string. Then the exposed function `open_yanked_error` reads the `+` 
register, parses the string it contains and opens the file at the right line.

As for calling the function itself, this is the mapping I use:

```lua
local parse_error_string = require('parse_error_string')

vim.keymap.set('n', '<Leader>fy', parse_error_string.open_yanked_error,
  { desc = 'Open yanked error' })
```

Now hitting `<Leader>fy` in normal mode will make neovim jump on the error 
found in the compilation pane.

# Conclusion

I hope this post has demonstrated that it is not too difficult to set up a good
development workflow in the terminal. With a few basic tools, some regexes, a 
bit of Lua and we can have a fast and efficient compilation loop that is 
language agnostic.

Feel free to adapt it to your needs and let me know if you like it.

[pomodozig]: https://github.com/jecaro/pomodozig
[tmux.nvim]: https://github.com/aserowy/tmux.nvim
[feedback]: https://github.com/NorfairKing/feedback

