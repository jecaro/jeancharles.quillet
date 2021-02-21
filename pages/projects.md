---

title: Projects
toc: include

---

There are the two main projects I built to learn Haskell.

# [bigball](https://github.com/jecaro/bigball)

[bigball](https://github.com/jecaro/bigball) is my second real life Haskell
project. When I was working for [Scalian](https://www.scalian.com), we had this
huge C++ project. Think about something with more than 300 sub-projects
(libraries or executables). It was tough dealing with such a huge code base and
its complexity. There were a lot of dependencies between the projects and we
had no way to actually list them and view the dependency graph.

We needed a tool to help us to handle all theses dependencies.

We were working with Microsoft Visual Studio. Its main file, also called the
solution file, contains this graph. So this was just a matter of parsing this
file, creating the dependency graph and for each project output a file with it.

The software wasn't very difficult to write. All the required libraries were
available on [hackage](https://hackage.haskell.org/). My workmates were pretty
happy with it and it was quickly integrated in our CI.

You can check it out on
[github](https://github.com/jecaro/bigball).

# [hscalendar](https://github.com/jecaro/hscalendar)

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
any trouble. This project has allowed me to get to use [servant](), [lenses](),
[RIO](), [QuickCheck](). It was a great fun to learn all this.

