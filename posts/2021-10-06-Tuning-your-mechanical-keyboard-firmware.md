---

title: Tuning your mechanical keyboard firmware
toc: true

---

# Introduction

In a previous
[post](/posts/2021-06-25-A-good-enough-mechanical-keyboard.html), I built an
awesome mechanical keyboard. The cool thing is that the board I chose supports
[qmk]. That means that building the keyboard is only half the fun, the other
half is tuning the software to fit my needs.

# Missing keys

The layout I chose doesn't have function keys. I find them unnecessary most of
the time. But in fact, there are a few shortcuts that need them that I actually
use sometime:

- `ALT-F4`: to close a window in Gnome
- `F2`: to rename a file in Gnome file manager
- `CTRL-ALT-F{1,8}`: to switch to a virtual console

I also neither have a `SUPPR` nor an `INSERT` key. I never toggle in insert
mode and I don't use much the `SUPPR` key. So that's OK. But more annoying,
that means that these shortcuts don't work either:

- `CTRL-ALT-SUPPR`: reboot the machine
- `SHIFT-INSERT`: paste highlighted text in X terminals

And lastly, I don't really have room for the `APP` key: the key used to trigger
a context menu in most GUI.

# qmk to the rescue

[qmk] is an open source firmware for keyboards. It support a [huge number of
boards](https://qmk.fm/keyboards/) and has plenty of
[features](https://beta.docs.qmk.fm/using-qmk/software-features). In its
[repository][qmk], it has a lot of examples for the different boards it
supports. I recommend starting from there by copying an existing firmware
source for your board, then tweak it to fit your needs.

# Additional layer

First of all, I made all all missing keys available on an additional layer.
This is the same feature one can find on laptop keyboards to access special keys
such as multimedia keys, multi screen toggle, or WiFi activation. [qmk] is able
to handle up to 16 layers which should be more than enough for any power user.

This is my base layer. It strictly follows the `ISO-AZERTY-FR` layout I wanted.
I only changed `CAPSLOCK` to `ESC` as I am a heavy vim user. I also wanted to
have an easy access to `HOME`, `END`, `PAGEUP` and `PAGEDOWN`, so I made these
keys available on the right row.

```c
/* Keymap _BL: (Base Layer) Default Layer
 * ,----------------------------------------------------------------.
 * |²  |  &|  é|  "|  '|  (|  -|  è|  _|  ç|  à|  )|  =|Backsp |Home|
 * |----------------------------------------------------------------|
 * |Tab  |  A|  Z|  E|  R|  T|  Y|  U|  I|  O|  P|  ^|  $|     |PgUp|
 * |-------------------------------------------------------    -----|
 * |ESC    |  Q|  S|  D|  F|  G|  H|  J|  K|  L|  M| ù|  *|Entr|PgDn|
 * |----------------------------------------------------------------|
 * |Shift|   <|  W|  X|  C|  V|  B|  N|  ,|  ;|  :| !|Rshift|Up|End |
 * |----------------------------------------------------------------|
 * |Ctrl |Win/App|Alt|         Space       |Agr|Ctrl|Fn|Lef|Dow|Rig |
 * `----------------------------------------------------------------'
 */
```

And this is my additional layer, temporarily available when holding the
`FUNCTION` key.

```C
/* Keymap _FL1: Function Layer 1
 * ,----------------------------------------------------------------.
 * |ESC| F1| F2| F3| F4| F5| F6| F7| F8| F9|F10|F11|F12| Delete| Ins|
 * |----------------------------------------------------------------|
 * |RESET|   | ↑ |   |   |   |   |   |   |   |   |   |   |     | NXT|
 * |-------------------------------------------------------    -----|
 * |       | ← | ↓ | → |   |   |   |   |   |   |   |  |   |    | PRV|
 * |----------------------------------------------------------------|
 * |HUI |SAI|VAI |RGBMOD| L+|LED| L-|   |   |    |   |  |Play|V+|Mut|
 * |----------------------------------------------------------------|
 * |HUD |SAD |VAD |         RGB_Tog       |   |   |   | << | V-| >> |
 * `----------------------------------------------------------------'
 */
```

I put the missing keys as close as possible to their original position. Along
the missing keys, I also added a few handy multimedia keys.

# Combo events

Having these keys available on a separate layer makes the shortcuts that use
them even more complicated than they are. For example `CTRL-ALT-SUPPR` is
already a three keys shortcut, can I avoid to make it a four keys shortcut ?

The combo events feature of [qmk] allows to map a shortcut to a new one. That
means that I can make my keyboard send `CTRL-ALT-SUPPR` when I hit
`CTRL-ALT-DEL` which is pretty close to the original shortcut.

Below is the full list of shortcuts I have remapped with `qmk`:

| From | To |
| - | - |
| LSHIFT-BACKSPACE | SHIFT-INSERT |
| LALT-QUOTE | ALT-F4 |
| LCTRL-LALT-BACKSPACE | CTRL-ALT-SUPPR |
| LCTRL-LALT-AMPERSAND | CTRL-ALT-F1 |
| LCTRL-LALT-EACUTE | CTRL-ALT-F2 |
| LCTRL-LALT-DQUOTE | CTRL-ALT-F3 |
| LCTRL-LALT-QUOTE | CTRL-ALT-F4 |
| LCTRL-LALT-LPARENTHESIS | CTRL-ALT-F5 |
| LCTRL-LALT-MINUS | CTRL-ALT-F6 |
| LCTRL-LALT-EGRAVE | CTRL-ALT-F7 |
| LCTRL-LALT-UNDERSCORE | CTRL-ALT-F8 |
| LCTRL-LALT-CCEDILLA | CTRL-ALT-F9 |
| LCTRL-LALT-AGRAVE | CTRL-ALT-F10 |
| LCTRL-LALT-RPARENTHESIS | CTRL-ALT-F11 |
| LCTRL-LALT-EQUAL | CTRL-ALT-F12 |

# App key

One last thing is that missing `APP` key. It is not that I use it very often
but I thought I needed to make it available for completeness. I could have put
it on the additional layer on top of the `WIN` key. But there is already a
light control keys their.

So I used another neat [qmk] feature which is [tap
dance](https://beta.docs.qmk.fm/using-qmk/software-features/feature_tap_dance).
When I hit the `WIN` key twice during a short interval of time, the keyboard
sends the `APP` key to the computer.

# Upgrade procedure

This firmware is available on my [github] like most of my projects. I put here
for the record how to setup, compile and flash the firmware to the keyboard.

First, one needs to access the keyboard as an unprivileged user, this udev rule
to be put in `configuration.nix` adds the needed rights to the `users` group.

``` nix
# Additional udev rule for qmk
services.udev.extraRules = "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"03eb\", ATTRS{idProduct}==\"2ff4\", GROUP+=\"users\"";
```

Then the initial setup, it writes the default keyboard and keymap in the file
`~/.config/qmk/qmk.ini`.

``` bash
$ nix-shell
$ qmk config user.keyboard=xd68
$ qmk config user.keymap=jecaro_iso_azerty
```

To compile the firmware:

``` bash
$ qmk compile
```

To install, one need to put the keyboard in a special mode called DFU mode by
pressing `RESET` with `FUNCTION-TAB`.

``` bash
$ qmk flash
```

And that's it !

# Conclusion

[qmk] is pretty awesome. Its documentation is great. It requires a bit of
reading to find out how the different features work and what is the best for
one needs. But after that everything is pretty smoothly. And it feels good to
be able to tune just everything about the keyboard that I use everyday.

[qmk]: https://github.com/qmk/qmk_firmware
[github]: https://github.com/jecaro/qmk_firmware/tree/jecaro_iso_azerty/keyboards/xd68/keymaps/jecaro_iso_azerty
