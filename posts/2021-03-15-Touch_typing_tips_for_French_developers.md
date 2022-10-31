---

title: Touch typing tips for French developers
toc: true

---

# Introduction

During the second covid lockdown in April 2020 in France, I was really bored.
I was looking for something new to learn and I decided to practice to become a
touch typist for good. As someone who spent most of its time behind a computer,
I wouldn't say I was a bad touch typist at this time but clearly I wasn't a
good one either. I never really took the time to actually learn the proper way
to touch type so I had a few bad habits I wanted to get rid off.

At this time I was also deeply into vim, and the fact that I wanted to become a
better touch typist was kind of related. But that's another story that might be
the subject of another post eventually.

# Choosing the right layout

The first question I needed to answer was: "What kind of layout do I want to
learn ?" This seems like a dumb question but actually it's not that easy to
answer. I live in France and the main layout here is `ISO-AZERTY-FR` and it's
not well suited for programming.

![](/images/KB_France.svg "ISO-AZERTY-FR")

Notice how all the matching brackets but angle brackets are very far away one
from another. And these useful characters: `{` `}` `[` `]` `~` `|` `\` `` ` ``
`@` are only available by the use of the right `Alt` modifier: a key known as
`AltGr`. This is already bad enough. But wait there is more.

It also has dead keys (in red on the image). Their purposes is to output
accentuated characters like `ê` `ë` or `È`. To output `â` for example, you need
to press the key `^` first then the key `a`. And for caret only, you press `^`
followed by `space`.

On Linux it is not too bad so to speak as the dead keys are only `" ` and `^`.
But on Windows it is worse, as `~` and `` ` `` are also dead keys.

It is very unfortunate that these keys are pretty important shorcuts in vim:

* `^`: jump to the first non-blank character of the line
* `~`: switch case
* `` ` ``: jump to a mark

Compare with the `QWERTY-US` layout below:

![](/images/KB_United_States.svg "QWERTY-US")

Matching brackets are side by side and everything is easily reachable with the
`SHIFT` key. This is much better for development work. One of my workmate
already had made the move to `QWERTY-US` and he was very happy with it. If
required there are plenty of ways to output accentuated characters with these
kinds of layout. One of them is to use this
[variant](http://marin.jb.free.fr/qwerty-fr/) for French people.

Another thing to consider, if I make the move I'll have the wrong labels on any
keyboard. Actually, it is not a problem because when you are a good touch
typist you don't need to watch your keyboard anymore. That is the whole point
of it. So it is not a good deal but it might be annoying.

At the very end I decided to stick with `ISO-AZERTY-FR`. The main reason was
that I wanted to become a good touch typist for good. And because I live in
France, all the keyboards around me are in `ISO-AZERTY-FR` and I wanted to be
able to touch type on all of them. That is unfortunate but that is the way it
is. It is true that it is bad for programming. But after all, I have been able
to work so far with bad touch typing habits. I can cope with weird character
positions as long as I can touch type them. At some point it will end up in
muscle memory and I will just forget about it.

The only thing remaining to solve is this dead key problem. Fortunately there
is a layout variant without dead keys available on Linux.

```console
$ setxkbmap fr -variant oss_nodeadkeys
```

On Windows, it is possible to configure [autokeys](https://www.autohotkey.com/)
to
[disable](https://github.com/jecaro/dotfiles/blob/3adf0514d2c234f7d4adcaf43e428cb540115416/autokey/vim_keys.ahk)
them.

# Learn and practice touch typing

The layout chosen all I had to do is to practice touch typing. There are a lot
of good websites for this.

I first used [Typing Club](https://www.typingclub.com/). It is available for
French `ISO-AZERTY-FR`. It made me learn progressively the good positions of
the fingers on each rows of the keyboard. So it was a good starting point to
get to learn the right position for each finger. It also made me type some
French text with accentuated characters. It helps me to get rid of the bad
habits that I had.

Then I typed some code on [typing.io](https://typing.io/). The website proposes
to type some codes taken from open source software in different programming
languages. It even has some Haskell code from xmonad. I found the Perl code
especially good for practicing typing special characters.

And finally, I practice on a regular basis on
[monkeytype](https://monkeytype.com/). I choose the fun box `ASCII`. As the
name suggests, it makes me type random `ASCII` characters. So it's good to
build up muscle memory on the whole keyboard.

# Conclusion

It took me two or three weeks to learn the good positions of the fingers and a
couple of months to get rid of my bad habits. I only practice five minutes a
day so it doesn't involve a lot of time.

I'm glad I now have good typing habits. I can now touch type on all the
keyboard around me. It requires a minimal configuration to work with vim but I
can cope with that.

Now that I'm a touch typist my wrists thank me at the end of the day and I am
confident that I will never see my keyboard again !
