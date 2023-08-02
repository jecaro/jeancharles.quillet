---

title: Error handling resources for Haskell
lastmod: 2021-10-22
toc: true

---

# Introduction

I'm very interesting in error handling. It is such an important topic in
programming. It's complicated, difficult, but the only way to go for writing
correct software.

This subject is still not solved in the Haskell world. There are a lot of
different ways to handle errors and it is also very controversial.

This post is a collection of the different approaches I stubbled upon.

# Pure code

On the most basic level, let's say in pure code to be simple (i.e. non monadic
code), the subject is not controversial. People usually agree on not to throw
any `Exception` and use `Maybe` as the return type when the function has only
one way to fail or `Either` when it has more than one.

# Monadic code

This is a complete different story in monadic code. There are basically two
camps here:

- the one in favour of exceptions
- the other in favour of errors appearing in the function signature, sometimes
  called "checked exceptions" because the compiler checks that the exceptions
  are handled by the developer

I won't go into the details of the two camps here, arguments of both sides are
pretty well explained in the following posts.

Let's first have a look at what is an `Exception` in Haskell. The following
post contains the most comprehensive description of the different types of
exceptions that I have read so far. So it is an interesting read:

[The three kinds of Haskell exceptions and how to use
them](https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/)

# The authoritative

When I started getting interested in Haskell, the learning material produced by
[FP Complete](https://www.fpcomplete.com) was hugely valuable. Honestly, I'm not
sure I would have been able to make my way into the functional programming
world without it. So I'll be always very grateful to them for producing such
good documentation.

Among the material they produced, there are posts about exceptions and error
handling:

- [Safe exception
  handling](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
- [Exceptions Best Practices in
  Haskell](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/)

When I discovered that what I recognized to be the most authoritative source of
Haskell good practices were advocating for the use of exceptions for error
handling instead of trying to track errors in the type system, I have to say
that this was a huge disappointment for me.

I have some experience about this approach in C++ and Python and I know how
easy it is to forget about handling exceptions. It's not too bad to do it right
when you work alone and on the first code iteration. In this case, it's easy to
keep track of the control flow defined by exceptions in your head. But when the
code needs evolutions or refactoring, there is chance close to 100% that you
forget to handle correctly the errors that needs to be handled.

But anyway, I wanted to learn Haskell so bad and if it was the way to go I would
give it a try. Later I learned that there are alternative ways to handle errors
in Haskell.

I have followed this approach in my main project
[hscalendar](https://github.com/jecaro/hscalendar). My feedback is: yes it is
easy to write and yes it is easy to forget about it. I wasn't very surprised by
the developer experience here. So if you decide to go that way I would
recommend to write down somewhere the different exceptions that you can think
about during the development and for each of them write tests to make sure they
are properly handled.

# Purity with `ExceptT`

Other people do want to track errors on the type level, this is the case for
the author of this post:

[What do you mean ExceptT doesn't
Compose?](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html)

In his example, he makes use of the package
[errors](https://hackage.haskell.org/package/errors). Another package called
[transformers-except](https://hackage.haskell.org/package/transformers-except)
also offers pretty good combinators to handle errors in `ExceptT`.

Going through the related [reddit
discussion](https://www.reddit.com/r/haskell/comments/68kyfx/what_do_you_mean_exceptt_doesnt_compose/)
and you can see how controversial this subject is.

I did use this approach for error handling in a small project of mine:
[bigball](https://github.com/jecaro/bigball/). I can say that it feels good to
be reminded by the compiler to do something with errors. Of course not all the
errors are handled in this code. But at least, all the obvious ones are and
this is good enough for me.

Doing so, I kind of understood the statement "it doesn't compose". It's not
that it doesn't but it is actually true that "it doesn't _easily_ compose". But
in a sense it is not surprising either because it adds another layer of
thinking when you develop. You have to think about what needs to be done in
case of an error and this is usually pretty complex.

Other posts of people following this approach:

- [On EitherT](https://lambdafoo.com/posts/2018-06-22-transformers-either.html)
- [Testing Failure with Either Instead of
  Exception](https://haskell-at-work.com/episodes/2018-03-18-testing-failure-with-either-instead-of-exception.html)

# Making it easy to compose

The fact that this approach does not _easily_ compose is viewed by some people
as an ergonomic problem. Some special syntax needs to be defined to compose
functions that might returns errors. That is what is done in Rust with great
success so it should be possible to do it in Haskell as well.

Matt Parson and Chris Done, two well known members of the Haskell community are
following this way. These guys are trying really hard to find an ergonomic way
to handle errors on the type level.

## Matt Parson

Matt Parson has written numerous posts about this subject:

- [The Trouble with Typed Errors](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html)
- [Plucking Constraints](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html)
- [Plucking In, Plucking Out](https://www.parsonsmatt.org/2020/10/27/plucking_in_plucking_out.html)

The library that came out of this idea:
[plucky](https://hackage.haskell.org/package/plucky)

He also made a pretty complete and well explained video about this subject at
[lambda conf 2018](https://www.youtube.com/watch?v=A5c9kgDYXr8).

I greatly appreciate his honesty in the part about the so called "good
practices" that he presented as "some guy opinion".

Other libraries based on open variants:

- [oops](https://github.com/i-am-tom/oops)
- [Excepts](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-Variant-Excepts.html)

## Chris Done

When [QualifiedDo][QualifiedDo] was introduced in GHC 9.0.1, Chris Done saw it
as an opportunity to address this ergonomic issue.

- [Recoverable errors in Haskell](https://chrisdone.com/posts/recoverable-errors-in-haskell/)
- [Try.do for recoverable errors in Haskell](https://chrisdone.com/posts/try-do/)
- [Is it Try.do that is dangerous?](https://chrisdone.com/posts/is-try-do-dangerous/)
- [How exactly ExceptT differs to EarlyDo](https://chrisdone.com/posts/exceptt-vs-early-do/)

This list of posts is pretty well summarized by Gabriel Gonzales in this tweet:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Here&#39;s my summary
of the recent discussion around <a
href="https://twitter.com/christopherdone?ref_src=twsrc%5Etfw">@christopherdone</a>&#39;s
GHC plugin for early return <a
href="https://t.co/tnfWTag8Dw">pic.twitter.com/tnfWTag8Dw</a></p>&mdash;
Gabriel Gonzalez (@GabrielG439) <a
href="https://twitter.com/GabrielG439/status/1346276478862856192?ref_src=twsrc%5Etfw">January
5, 2021</a></blockquote> <script async
src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

The great people of [Kowainik](https://kowainik.github.io/) are also trying to
follow the [QualifiedDo][QualifiedDo] path with this library:

[IO with Exceptions tracked on the type-level](https://github.com/kowainik/eio)

# Other resources

Here are some other links related to the subject:

- The [failable](https://hackage.haskell.org/package/failable) library
- [Lightweight Checked Exceptions in
  Haskell](https://www.well-typed.com/blog/2015/07/checked-exceptions/)
- [Exceptions tutorial](https://markkarpov.com/tutorial/exceptions.html)
- [Default exception handler in Haskell](https://taylor.fausak.me/2021/04/03/default-exception-handler-in-haskell/)

# Conclusion

That story doesn't end up here and I'm sure there will be in the future a lot
of work done in this area. I'll keep this page up to date as new ideas pops out
and I hope that a consensus will be found in the near future so a lot more
people will start using the same idiom for error handling. As a result it will
bring consistency to the ecosystem and raise approachability for new comers.

As for me, I'm currently sold to the camp of checked exceptions. That doesn't
mean I'll never change my mind. I'd like to use some of these approaches to
confront them to real world problems and see how they scale.

[QualifiedDo]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html

