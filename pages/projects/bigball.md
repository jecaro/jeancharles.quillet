---

title: bigball
lastmod: 2024-11-26

---

[bigball] was my second real life Haskell project. When I was working for 
[Scalian](https://www.scalian.com), we had this huge C++ project. Think about 
something with more than 300 subprojects (libraries or executables). It was 
tough dealing with such a huge code base and its complexity. There were a lot 
of inter dependencies between the projects, and we had no way to actually list 
them and view the dependency graph.

We needed a tool to help us to handle all these dependencies.

We were working with Microsoft Visual Studio. Its main file, also called the
solution file, contains this graph. So this was just a matter of parsing this
file, creating the dependency graph and for each project output a file with it.

The software wasn't very difficult to write. All the required libraries were
available on [hackage](https://hackage.haskell.org/). My workmates were pretty
happy with it, and it was quickly integrated in our CI.

You can check it out on
[github](https://github.com/jecaro/bigball).

[bigball]: https://github.com/jecaro/bigball

