---

title: Copying from anywhere
lastmod: 2022-10-31
toc: true

---

I do most of my work in vim running in a tmux session over ssh. This workflow
has been working great for me so far and will probably be the object of a
future post.

One thing that gave me a lot of trouble though is how
to handle copy/paste across these remote tmux sessions.

# A clipboard issue

Let me explain the problem. Let's say I have to do some work on a remote
machine, I connect to it with ssh, attach or start a new tmux session and start
vim to begin editing some code or some configuration file.

Now, what if I have some code or a piece of configuration I need to copy from
my local browser for example to this remote vim, behind tmux and ssh? How would
I do that?

# Primary selection

The most straightforward solution would be to reach out for the mouse, select
the text to be copied in the browser, return to vim, switch in `INSERT` mode
and paste the text with the middle button.

This solution is using the [primary selection][clipboard] clipboard.

# Clipboard selection

But I don't like the mouse, can we do it with the keyboard only ?

Most terminal emulators support paste command with `CTRL-SHIFT-V` (I personally
use [alacritty](https://github.com/alacritty/alacritty)). OK fine that works.
Let's copy the text from the browser using `CTRL-C` and switch back to vim, go
into `INSERT` mode and paste with `CTRL-SHIFT-V`.

This would also work from a local vim to a remote one. Copy some text into the
`+` register and paste it with `CTRL-SHIFT-V`.

This solution uses the [clipboard selection][clipboard].

# Copy in remote, paste elsewhere

Now, let's say I am working in vim in a remote machine, I want to copy some
text from my current vim session to a vim running on another machine. Since I
am already in vim and probably doing some heavy copy/paste with vim registers
already, I really don't want to grab the mouse for this. It would be really too
awkward.

Let's paste the text in the `+` register, will I been able to use it from the
local OS?

No, that `+` register is bound to the system on which vim currently runs which
is in this case the remote machine.

# Enter `OSC 52`

This is the problem solved by the plugin [vim-oscyank]. This plugin uses a
special [OSC sequence](https://en.wikipedia.org/wiki/ANSI_escape_code) that
allows to copy some text to the system clipboard from pretty much anywhere as
long as it runs into a terminal emulator! The only requirement is that the
terminal emulator used must support this sequence. Check out the list of
terminal emulators implementing this feature on [github][vim-oscyank].

That means that when you copy some text from vim using this plugin, that text
ends up in the [clipboard selection][clipboard] of the local OS no matter what.
And then you can paste it into any terminal with `CTRL-SHIFT-V` or for a local
vim, using the `+` register as you would do with a local copy command.

Keyboard only workflow, at last!

# Configuration

Among the different configuration options described on the [project
page][vim-oscyank], the one I like the most is:

```vim
autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '+' | OSCYankReg + | endif
```

With this `autocmd` in `.vimrc`, copying some text into the `+` register make
the plugin copy the content to the local OS clipboard using the `OSC 52`
sequence.

# Summary

As a summary, the table below shows the different ways to copy/paste texts
between remote and distant machines. On the last line, what the plugin
[vim-oscyank] makes possible.

+----------+---------------------------------------------+---------------------------------+
|          | from                                        | to                              |
+:========:+=============================================+=================================+
| mouse    | local browser: selection\                   | local terminal: middle click\   |
+----------+---------------------------------------------+---------------------------------+
| mouse +  | local browser: selection + `ctrl-c`\        | remote vim: middle click\       |
| keyboard | local terminal: selection + `ctrl-shift-c`  | local vim: `*` register         |
+----------+---------------------------------------------+---------------------------------+
| keyboard | local or remote vim: `+` register           | local terminal: `ctrl-shift-v`\ |
| only     |                                             | remote vim: `ctrl-shift-v`\     |
|          |                                             | local vim: `+` register         |
+----------+---------------------------------------------+---------------------------------+

This plugin makes it possible to copy/paste seamlessly between vim instance
across different hosts with the keyboard only. That's awesome! If you know
better way to do it please [let me know](/pages/contact.html).

[vim-oscyank]: https://github.com/ojroques/vim-oscyank
[clipboard]: https://specifications.freedesktop.org/clipboards-spec/clipboards-latest.txt
