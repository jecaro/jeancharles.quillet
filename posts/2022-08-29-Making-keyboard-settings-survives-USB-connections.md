---

title: Making keyboard settings survives USB connections

---

As a developer and a vim user, I rely heavily on a good typing experience. I am 
constantly trying to improve it whether it is in becoming a better touch typist 
or using a better keyboard. About this, as [stated 
before](/posts/2021-03-15-Touch_typing_tips_for_French_developers.html), I like 
to use the not so good French `ISO-AZERTY-FR` layout. As much as I like to 
stick with the default, some keys are purely not compatible with vim. I'm 
talking here about the key: `^` which is used in vim to get back to the first 
character of the line.

First of all, what is a dead key ? When you press one of these keys, nothing 
appears on the screen. The character appears only when you press a second key. 
Usually that second key outputs a character slightly modified by this dead key. 
On the French keyboard, its purpose is to type these characters: ê, î for `^.`.

The French keyboard have a few other dead keys such as `¨` or `` ` `` (the one 
triggered by `AltGr-*`). But these are not as important in vim as `` ` `` can 
also be triggered by `AltGr-7` which is not a dead key.

Back to vim, in normal mode, you want to go back to the beginning of the line, 
then you have whether to hit twice `^` or hit `^` followed by `space`. That's 
one key more than necessary so totally unacceptable. Let's see how we can work 
around this.

XWindow has a keyboard variant to turn off dead keys which can be easily 
activated with this command:

``` bash
$ setxkbmap -layout fr,fr -variant nodeadkeys, -option caps:escape,grp:ctrls_toggle
```

It sets two layouts for the current keyboard, one without and one with dead 
keys. One can switch between the two layouts by hitting the two control keys 
simultaneously. So having this setup by default is only a matter of having this 
command called when XWindow starts. It can be put in the `.xsession` or 
`.xinitrc` file for example.

So far so good, that works pretty well as long as you don't need to plug a 
keyboard. Indeed as soon as you to it XWindow affects the default layout to 
this new keyboard and the magic command needs to be called again. Unfortunately 
this situation happens quite often when you use a laptop and carry it around to 
work with others or attend meetings for example.

The usually recommended way to solve this issue is to change the default 
keyboard options in the XWindow configuration file such as `xorg.conf`. But 
then that option will be applied to every single user of the computer which 
doesn't make much sense in my opinion. Keyboard preferences is indeed a very 
personal matter and it should be possible to tweak it on a user level.

As easy it is to tweak the keyboard layouts at XWindow startup, there is no 
builtin way to do it when keyboards are plugged or unplugged. This is where the 
software [inputplug][inputplug] comes in handy. That little tool listens to 
XInput events and triggers arbitrary scripts on any change.

For example, calling [inputplug][inputplug] like this:


``` bash
$ inputplug -c ~/bin/on-new-kbd.sh
```

will trigger the script `~/bin/on-new-kbd.sh` on any XInput event. That script 
could be something like:

``` bash
#!/usr/bin/env bash

echo >&2 "$@"
event=$1 id=$2 type=$3

case "$event $type" in
'XIDeviceEnabled XISlaveKeyboard')
    setxkbmap $id -layout fr,fr -variant nodeadkeys, -option caps:escape,grp:ctrls_toggle
esac
```

Here [inputplug][inputplug] gives us three arguments:

- the name of the event
- its identifier
- its type

Using these, we can filter out the event we're interested in, in our case, a 
new keyboard event and react accordingly by setting up that new keyboard as we 
like.

With this little trick, we can carry on viming on our new keyboard, not being 
annoyed by these deadly *dead keys* !

Read more about [inputplug][inputplug] on its [github page][inputplug].

[inputplug]: https://github.com/andrewshadura/inputplug
