---

title: My best tmux tips
lastmod: 2026-06-21
toc: true

---

I have been working on Linux for a while now. Since I started, `tmux` has
always taken a very central place in my workflow. In this post, I will share my 
favorite simple tricks from my workflow.

# Why tmux?

I live in the terminal. Whatever I do is in the terminal, whether I work 
locally on my computer or on a remote machine through SSH. `tmux` 
allows me to open any number of panes and windows and run programs in there. 
While this sounds like the job of a window manager, `tmux`'s killer feature is that 
when using it over SSH, it supports session recovery. And that is what makes it 
essential to me.

Here are two examples of my most common workflows:

- For programming: editor on the left, compiler on the right, errors appear the 
  instant I save, no switching context
- For devops: one window per service, each tailing its logs, so I can watch a 
  deploy unfold across the whole system at once

# Moving around

I use `neovim` as my editor. Both `tmux` and `neovim` let you split your screen 
into panes. The problem is that by default they don't share navigation 
keybindings, pressing `ctrl-w h` inside `neovim` won't move you to the adjacent 
`tmux` pane. The plugin [`tmux.nvim`](https://github.com/aserowy/tmux.nvim) 
solves this by letting you navigate and resize panes seamlessly across both 
tools with the same keys.

Refer to the plugin for documentation. As for me, I have chosen these 
keybindings:

- `ctrl-h`, `ctrl-j`, `ctrl-k`, `ctrl-l`: to move between panes
- `ctrl-alt-h`, `ctrl-alt-j`, `ctrl-alt-k`, `ctrl-alt-l`: to resize panes

# Zooming

One feature I cannot live without in `tmux` is the zoom feature. Imagine you 
have many panes open and need all of them, but want to focus on just one. Put 
the focus on that pane and hit `ctrl-prefix z`. It will expand to fill the 
whole screen. Hit the same key again to restore the original layout.

# Session management

There are tons of plugins to handle sessions in `tmux`. But I like simple 
solutions. I use one `tmux` session per machine with one window per project. To 
find out which window corresponds to which project, I simply show the current 
directory as the window text. It is usually the root of the `git` repository 
and also the name of the project. Simple, easy.

That can be achieved with:

```tmux
# Set the automatic title to the current basename
set-option -g automatic-rename-format '#{b:pane_current_path}'
```

Another efficient trick is to open new panes in the same directory as the 
current one:

```tmux
# Set new splits to open in current directory
bind '"' split-window -c "#{pane_current_path}"
bind % split-window -h -c "#{pane_current_path}"
```

With these two simple options, I don't need anything else to work on multiple 
projects at the same time. I can always see all the projects I'm working on and 
jump to any of them by switching windows.

# `SSH_AUTH_SOCK` recovery

I usually forward my ssh-agent to the hosts I connect to. SSH conveniently sets 
up the env variable `SSH_AUTH_SOCK` for this purpose. However, consider the 
following scenario:

- I connect to a remote machine and start working on a project
- `SSH_AUTH_SOCK` is set and points to the right socket
- now there is an issue with the network or I turn off my main computer for some 
  reason
- I connect again to the remote machine, reattach the session. But now my 
  `SSH_AUTH_SOCK` is stale and points to a non-existent socket
- It is now impossible to `git fetch` or `git push` to a remote repository

The first obvious solution is to detach, go to the project from the main shell 
and do the `git` operation, then reattach the `tmux` session. That works, but 
it's annoying.

A better option: `tmux` already provides a built-in workaround. When starting 
new shells, which happens when opening a new pane or window, it recovers 
certain env variables from the parent process. `SSH_AUTH_SOCK` is among them. 
So the simplest fix is just to open a new pane.

If that's not desirable because you want to keep the shell history of that 
specific pane, you can add this function to your `~/.bashrc` and call it to 
update the variable in place.

```bash
# Fix ssh reattaching to tmux sessions
fixssh() {
  eval "$(tmux showenv -s SSH_AUTH_SOCK)";
}
```

# Conclusion

This concludes this list of quality of life improvements for tmux. Hope you 
found it interesting and if you found something useful please [let me 
know](/pages/contact.html).

