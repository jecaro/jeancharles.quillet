---

title: Projects
toc: include

---

# This website

This website is built with [Hakyll](https://jaspervdj.be/hakyll/), a static
site generator library in Haskell. I describe its architecture in this [blog
post](/posts/2021-02-22-This_website_is_generated_by_Hakyll.html) and its
source code is fully available on
[github](https://github.com/jecaro/jeancharles.quillet).

# [Mprisqueeze](https://github.com/jecaro/mprisqueeze)

[mprisqueeze] is a wrapper to the software Squeezebox [squeezelite]. It starts 
[squeezelite] in the background and exposes an [MPRIS] interface to control it 
from any [MPRIS] client such as [playerctl].

It is a project I have realized to learn Rust. I have written a [blog 
post][mpris-post] about this experience.

# [Bigball](https://github.com/jecaro/bigball)

[bigball](https://github.com/jecaro/bigball) was my second real life Haskell
project. When I was working for [Scalian](https://www.scalian.com), we had this
huge C++ project. Think about something with more than 300 sub-projects
(libraries or executables). It was tough dealing with such a huge code base and
its complexity. There were a lot of dependencies between the projects and we
had no way to actually list them and view the dependency graph.

We needed a tool to help us to handle all these dependencies.

We were working with Microsoft Visual Studio. Its main file, also called the
solution file, contains this graph. So this was just a matter of parsing this
file, creating the dependency graph and for each project output a file with it.

The software wasn't very difficult to write. All the required libraries were
available on [hackage](https://hackage.haskell.org/). My workmates were pretty
happy with it and it was quickly integrated in our CI.

You can check it out on
[github](https://github.com/jecaro/bigball).

# [Hscalendar](https://github.com/jecaro/hscalendar)

I started working on [hscalendar](https://github.com/jecaro/hscalendar) because
I was beginning to feel confident with Haskell. I already had implemented a few
[terminal games](https://github.com/jecaro/haskell-games), and finished a whole
bunch of [coding-game challenges](https://github.com/jecaro/codinggame-haskell).

And now I needed something more ambitious, something that feels like a real
life project.

I had this need for a specific tool in my previous work. At the end of each
month we needed to report on a half-day basis on which project we have worked.
I've been using a Google spreadsheet for this so far.

It is actually a very good fit. Well firstly because it answers an actual need
that I have, so it's very good for the motivation. And also it contains
everything a programmer needs to know to be fluent in a programming language.

To summarize it's got:

* command line tools
* web backend server
* web frontend
* database access
* command line parsing
* text parsing
* external process launching
* unit tests
* deployment process

For the frontend, I've implemented it in Elm using the library
[haskell-to-elm](https://github.com/folq/haskell-to-elm).

I also spent some time to automate the deployment. When the compilation succeed
and the tests pass, the CI builds a docker image and upload it on dockerhub. So
deployment is now just a matter of pulling the latest image and restart the
service.

The project has been a success as I've used it for more than six months without
any trouble. This project has allowed me to get to use
[servant](https://docs.servant.dev/en/stable/),
[lenses](https://hackage.haskell.org/package/lens),
[RIO](https://hackage.haskell.org/package/rio),
[QuickCheck](https://hackage.haskell.org/package/QuickCheck). It was a great
fun to learn all this.

# Open source contributions

When I find the time, I like to contribute to open source projects. I made some
modest contributions to these ones:

* [LDAP](https://github.com/ezyang/ldap-haskell): This package provides LDAP 
  interface code for Haskell programs, binding to the C LDAP API.
* [ldap-client](https://github.com/alasconnect/ldap-client): Pure Haskell LDAP
  client library implementing (the parts of) RFC 4511.
* [smos](https://github.com/NorfairKing/smos): a comprehensive self-management
  system
* [hatrace](https://github.com/nh2/hatrace): scripted strace
* [path](https://github.com/commercialhaskell/path): support for well-typed
  paths in Haskell
* [termonad](https://github.com/cdepillabout/termonad): a terminal emulator
  configurable in Haskell
* [hledger-flow](https://github.com/apauley/hledger-flow): a command-line
  program that gives you a guided Hledger workflow
* [lib3ds](https://github.com/hoopoe/lib3ds): a free toolkit for handling the
  "3DS" format for 3D model files

[LMS]: https://github.com/Logitech/slimserver
[MPRIS]: https://specifications.freedesktop.org/mpris-spec/latest/
[mpris-post]:/posts/2023-06-01-A-perfect-pet-project-to-learn-Rust.html
[mprisqueeze]: https://github.com/jecaro/mprisqueeze
[playerctl]: https://github.com/altdesktop/playerctl
[squeezelite]: https://github.com/ralph-irving/squeezelite

