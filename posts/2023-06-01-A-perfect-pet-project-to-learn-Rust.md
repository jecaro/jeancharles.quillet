---

title: A perfect pet project to learn Rust

---

I have been willing to learn Rust for a while. I wanted to understand if all 
the hype around this language was deserved, how much of the supposedly Haskell 
influence is actually present and if it is as difficult as people say. But I 
didn't have any project at hand to try it out.

For me the best way to learn a new language is to actually build something and 
to make the experience even better it is somehow of important that it solves a 
concrete problem that I have.

Recently I found the perfect pet project to learn Rust.

# Context

A bit of context first, I have been happy [LMS] user for a while now. [LMS] 
stands for logitech media server. It is an open source music server first 
developed by Logitech along great sounding hardware called Squeezeboxes. They 
have done different models such as SqueezeBox Radio or SqueezeBox Touch to name 
the one I own. At some point, Logitech lost interest in these audio devices and 
abandoned the project.

But it has been taking over by the open source community. It still works fine 
on many platforms including the Raspberry PI and it has regularly new features 
implemented. The community also developed a bunch of great third party software 
such as an Android and IPhone apps. There is also a software player called 
[squeezelite].

This is the software I use to listen to my music library from my desktop 
computer. It works perfectly fine but it is missing an important feature for 
me: an [MPRIS] interface.

# What is MPRIS ?

[MPRIS] is a DBUS interface specification to interact with media players. For 
my use case, I use [playerctl], an [MPRIS] client, to control the audio players 
running on my system:

- to show the current track being played
- to toggle the playing status between play and pause
- to move to the next or previous track

I put that into a [polybar] module using a super simple script. That way, on 
whatever desktop I am, I can see what is currently playing and change the track 
if I feel like it.

![](/images/polybar.png){.center}

I find it super useful and the cool thing is that it works whatever the media 
player being used: VLC, chrome and so on. So I really need [squeezelite] to 
have it as well.

I first looked at adding that interface to [squeezelite] itself. I quickly had 
a POC working but in order to get the track currently played, I'd need to talk 
to the server over a socket using either [the HTTP or the CLI API][API]. This 
is not particularly difficult, but not particularly exciting either. I know 
this kind of stuff can be tricky to get right in C ([squeezelite] is written in 
C) and I don't have that much time to spend on it. See where it is going: it 
would be cool to implement that project in a higher level and safer language.

So let's do this in Rust.

# The project

Here is the project: instead of adding an [MPRIS] interface to [squeezelite] I 
could add [squeezelite] to an [MPRIS] interface. That's what [mprisqueeze] 
does. It starts [squeezelite] in the background while exposing an [MPRIS] 
interface to control it. The commands are sent to [LMS] using the [HTTP 
API][API].

This project has the perfect requirements for me to learn a new language:

- it is something I need, which brings motivation
- it is not too ambitious I can get something working pretty fast
- but it's not trivial either

Here what I need to implement in [mprisqueeze]:

- parse the command line arguments
- start and monitor the [squeezelite] process in the background
- implement a DBUS interface
- discover the [LMS] server on the network
- pass the commands to the [LMS] server using the [HTTP API][API]

The implementation was a matter of a couple of weeks working in my free time.

# Feedback on Rust

Overall it was a super smooth experience. Rust is very well documented and the 
tooling, cargo and the language server, are great.

I can see some Haskell influence there but not that much actually. Rust is 
still a very imperative language, purity or higher-kinded types are nowhere to 
be found here. I would say this is more a general functional programing 
influence with the usual features: immutability by default, sum types and 
pattern matching.

This was not really that difficult for me to get into it. Having worked for 
years with C++ and all its footguns, I know that copying is costly and I 
understand the need for move semantics and ownership.

The thing that I really enjoyed is definitely error handling. I like the fact 
that the errors are returned as values. It makes it easy to follow the compiler 
and handle errors when appropriate or letting them bubble up in the stack. 
Unlike exceptions in Haskell, there is no hidden control flow. Also to have one 
simple, yet ergonomic way to handle errors, consistent across the whole 
ecosystem is really amazing.

# Conclusion

The project is published on [crates.io] and the sources can be found on 
[github][mprisqueeze]. If you are a [LMS] user, try it out and let me know how 
you like it.

# Related work

[slimpris2] has almost the same goals. It implements an [MPRIS] interface for 
one or all players connected to an [LMS] server. I have found in its code the 
way to discover the [LMS] server on the network.

[API]: https://raw.githack.com/Logitech/slimserver/public/8.0/HTML/EN/html/docs/cli-api.html
[LMS]: https://github.com/Logitech/slimserver
[MPRIS]: https://specifications.freedesktop.org/mpris-spec/latest/
[crates.io]: https://crates.io/crates/mprisqueeze
[mprisqueeze]: https://github.com/jecaro/mprisqueeze
[playerctl]: https://github.com/altdesktop/playerctl
[polybar]: https://github.com/polybar/polybar
[slimpris2]: https://github.com/mavit/slimpris2
[squeezelite]: https://github.com/ralph-irving/squeezelite

