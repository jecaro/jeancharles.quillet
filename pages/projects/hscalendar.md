---

title: hscalendar
lastmod: 2024-11-26

---

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

[hscalendar]: https://github.com/jecaro/hscalendar

