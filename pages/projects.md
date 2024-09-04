---

title: Projects
toc: include
lastmod: 2024-04-17

---

# This website

This website is built with [Hakyll](https://jaspervdj.be/hakyll/), a static
site generator library in Haskell. I describe its architecture in this [blog
post](/posts/2021-02-22-This_website_is_generated_by_Hakyll.html) and its
source code is fully available on
[github](https://github.com/jecaro/jeancharles.quillet).

# [Diverk][diverk]

[diverk] is a frontend app fully written in Haskell. It is available 
[online](https://diverk.quillet.org) as well as on the [Play 
Store](https://play.google.com/store/apps/details?id=org.jecaro.diverk). It 
allows you to browse a GitHub repository from your Android phone and is able to 
render markdown files.

Some context about the development of this project appears in this 
[post][diverk-post].

# [Mprisqueeze][mprisqueeze]

[mprisqueeze] is a wrapper to the software Squeezebox [squeezelite]. It starts 
[squeezelite] in the background and exposes an [MPRIS] interface to control it 
from any [MPRIS] client such as [playerctl].

It is a project I have realized to learn Rust. I have written a [blog 
post][mpris-post] about this experience.

# [Systranything][systranything]

[systranything] lets you put anything in your system tray. It uses a YAML file 
which describes the icon to put in the system tray, a context menu and 
callbacks as shell commands.

I use it to turn on and off my VPN, change my monitor and audio setup.

# [Bigball][bigball]

[bigball] was my second real life Haskell project. When I was working for 
[Scalian](https://www.scalian.com), we had this huge C++ project. Think about 
something with more than 300 subprojects (libraries or executables). It was 
tough dealing with such a huge code base and its complexity. There were a lot 
of dependencies between the projects, and we had no way to actually list them 
and view the dependency graph.

We needed a tool to help us to handle all these dependencies.

We were working with Microsoft Visual Studio. Its main file, also called the
solution file, contains this graph. So this was just a matter of parsing this
file, creating the dependency graph and for each project output a file with it.

The software wasn't very difficult to write. All the required libraries were
available on [hackage](https://hackage.haskell.org/). My workmates were pretty
happy with it, and it was quickly integrated in our CI.

You can check it out on
[github](https://github.com/jecaro/bigball).

# [Hscalendar][hscalendar]

I started working on [hscalendar] because I was beginning to feel confident 
with Haskell. I already had implemented a few [terminal 
games](https://github.com/jecaro/haskell-games), and finished a whole bunch of 
[coding-game challenges](https://github.com/jecaro/codinggame-haskell).

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
and the tests pass, the CI builds a docker image and upload it on 
[dockerhub](https://hub.docker.com/r/jecaro/hscalendar-server/tags). So 
deployment is now just a matter of pulling the latest image and restart the 
service.

The project has been a success as I've used it for more than six months without
any trouble. This project has allowed me to get to use
[servant](https://docs.servant.dev/en/stable/),
[lenses](https://hackage.haskell.org/package/lens),
[RIO](https://hackage.haskell.org/package/rio),
[QuickCheck](https://hackage.haskell.org/package/QuickCheck). It was a great
fun to learn all this.

# Toys

## [Minihasklisp][minihasklisp]

[minihasklisp] is a small Lisp interpreter with minimal dependencies. It even 
uses its own [applicative 
parser](https://github.com/jecaro/minihasklisp/blob/master/src/parser/Parser.hs) 
implementation. It is quite impressive to see how powerful such a small 
language can be.

## [Wolfram][wolfram]

[wolfram] is a simple implementation of [elementary cellular 
automaton](https://en.wikipedia.org/wiki/Elementary_cellular_automaton) in 
Haskell. Have fun trying all the 255 rules!

[LMS]: https://github.com/Logitech/slimserver
[MPRIS]: https://specifications.freedesktop.org/mpris-spec/latest/
[bigball]: https://github.com/jecaro/bigball
[diverk-post]: /posts/2023-11-07-Writing-an-Android-app-in-Haskell.html
[diverk]: https://github.com/jecaro/diverk
[hscalendar]: https://github.com/jecaro/hscalendar
[minihasklisp]: https://github.com/jecaro/minihasklisp
[mpris-post]: /posts/2023-06-01-A-perfect-pet-project-to-learn-Rust.html
[mprisqueeze]: https://github.com/jecaro/mprisqueeze
[playerctl]: https://github.com/altdesktop/playerctl
[squeezelite]: https://github.com/ralph-irving/squeezelite
[systranything]: https://github.com/jecaro/systranything
[wolfram]: https://github.com/jecaro/wolfram

