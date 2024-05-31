---

title: Writing an Android app in Haskell
lastmod: 2024-05-31
toc: true

---

# Introduction

I have been willing to learn [FRP] for a long time. Since I had the chance to 
have a little more time than usual these days, I thought it was the right time 
for me to dive into this paradigm and learn this stuff for good.

Among the different implementations of [FRP] in Haskell, I was especially 
interested in [Reflex]. What I find interesting in this implementation is that 
the very same code can be deployed on the web, as an Android app, or as an 
iPhone app which is spectacular when you think about it.

The idea of being capable of running Haskell code on a phone is so appealing 
that I could not resist. Once I heard about it, I knew that I needed to write 
an app at some point in my Haskell journey. And that promise of being able to 
run the very same code on different platforms. How true can that be? I needed 
to find out.

Unfortunately, examples of such apps in the wild are not that common, if not 
non-existent[^1]. I spent a fair amount of time looking for examples. Only 
tutorials are sometimes deployed on the web but nothing on Google Play (I did 
not dig in the app store as I don't own an iPhone). And as a professional 
developer, I know there is often a big gap between tutorials and real-life 
applications.

I needed to find out by myself. And that is what I did with this project.

# The project

As I like to do it, I needed an idea of something simple yet nontrivial to get 
my hands dirty. I found something that might be a good fit.

I am a big note taker. I take notes all the time about just anything, technical 
or not. I have tried many different tools. But what works best for me is plain 
markdown files in a private git repository.

What I am missing in my workflow is the ability to browse and search my notes 
from my phone. I could use the official GitHub app for that purpose. However, 
that would spoil the fun. I also know that there is a lot of value in owning 
the code of a tool you use. Once the code is written and works, it becomes easy 
to fix bugs and add features as needed.

The project would then be an app able to browse and search in a GitHub 
repository. It should also be able to render markdown files. For simplicity's 
sake, I decided to use the GitHub API to talk to the Git repository.

Here is the summary of the things the app needs to do:

- have some settings
- validate and store the settings
- sends requests to the GitHub API
- react to responses, parse them, and show the results
- render markdown
- handle errors
- wrap everything in an easy-to-use UI

# Experience

I had great fun learning [FRP]. As a developer, this is really the kind of 
experience I am looking at. A new concept that, once grokked, gives a deeper 
understanding of the domain, here, reactive UI development.

People usually say that [FRP] is hard to learn. I would not say that. But it is 
true that one has to change mindset to be able to be productive with it. And 
that is exactly the interesting point. My opinion is that it helps to 
understand the interactions of the widgets and user actions as a whole. It 
makes it clear that if something is difficult to implement in [FRP] that 
usually means the interactions are themselves complicated.

I put the app [online](https://diverk.quillet.org) and on the [Play 
Store](https://play.google.com/store/apps/details?id=org.jecaro.diverk)
for you to try out (and me to use). As advertised by [Obsidian 
Systems][obsidian] folks, it does work the same on the web and on Android. The 
whole code is available on [GitHub](https://github.com/jecaro/diverk).

# Missing pieces and dark corners

I want to take advantage of this post to shed light on some dark corners I 
encountered during the development of this project.

## Developer tools

For me, Obelisk is a developer tool, such as ghc, cabal, 
haskell-language-server, or even gcc. The documentation states that it must be 
installed.

But I don't usually install the developer tools I use. I pull all the ones I 
need from a pinned version of [nixpkgs] and put them in scope with a 
`nix-shell`.

Unfortunately, the `ob` command is not in [nixpkgs]. So I came up with that 
`shell.nix`:

```{.get
url=https://raw.githubusercontent.com/jecaro/diverk/e777f2d44ed4ecdb08ed8f2b3c832d327bea7611/shell.nix
language=nix}
```

One can see, that for this shell, we pin obelisk version v1.2.0.0. Yet the `ob` 
command, whatever version, doesn't care about this and uses [the rev pinned 
there](https://github.com/jecaro/diverk/blob/main/.obelisk/impl/github.json) 
instead. It fetches it, compiles it, and passes the commands to it. This is 
undocumented and can be super confusing if you don't know about it.

Now I also like to have [haskell-language-server][HLS] available in my editor. 
This is not very hard to add it in the development shell, yet quite difficult 
if you don't know how to do it. These are the relevant lines to be added to 
[default.nix](https://github.com/jecaro/diverk/blob/78ff0683f0b77d40d907ff19f4a9771c5406957a/default.nix#L20).

```{.get
url=https://raw.githubusercontent.com/jecaro/diverk/78ff0683f0b77d40d907ff19f4a9771c5406957a/default.nix
from=20
to=23
language=nix}
```

At the end, working around those weird uses of nix, my workflow to work on this 
project is:

```bash
$ nix-shell
```

brings `ob` into scope. I can now use `ob run` or `ob watch` and develop. Then:

```bash
$ ob shell
```

brings [haskell-language-server][HLS] into scope. From this shell, I start my 
editor to have type hovers, auto-formatting and all the niceties that 
[haskell-language-server][HLS] provides.

## Build and upload the Android app

The Play Store expects a signed AAB file. That file can be built with:

```bash
$ nix-build -A android.frontend --arg androidIsRelease true
```

The AAB file is then available in `./result/android-app-release.aab`.

It must be signed with some java tools. Let's start a shell with them:

```bash
$ nix-shell -p jdk17_headless
```

Now create a key store for storing the keys that will be used to sign the file.

```bash
$ keytool -genkey -v -keystore diverk.keystore -alias diverk -keyalg RSA -keysize 2048 -validity 10000
```

It will ask a few questions, along with a password. That one must be kept in a 
safe place. It will be needed for the next commands.

```bash
$ cp ./result/android-app-release.aab .
$ jarsigner -verbose -sigalg SHA256withRSA -digestalg SHA-256 -storepass the-passord-given-to-keytool -keystore diverk.keystore ./android-app-release.aab diverk
```

And that's it! The AAB file is now signed and ready to be uploaded on the Play 
Store.

## Deploy the app on the web

To deploy the app on the web, one must first build the derivation:

```bash
$ nix-build -A linuxExe
$ ls -L result
backend  frontend.jsexe.assets  static.assets  version
```

It contains the following files:

- `backend` is the executable of the HTTP server itself
- `frontend.jsexe.assets` is the web app compiled by ghcjs
- `static.assets` are the static assets used by the frontend

Now you can copy that derivation on any system having nix, reverse proxy it 
with nginx if you want, starts the backend, and it will serve your app on the 
web.

The backend supports many command line options. Find out which ones with 
`backend --help`.

# Learning resources

Here you can find the best resources I have found on the subject:

- Presentation of the library by its creator Ryan Trinkle: [part 
  1](https://www.youtube.com/watch?v=mYvkcskJbc4), [part 
  2](https://www.youtube.com/watch?v=3qfc9XFVo2c)
- [The official tutorial](https://reflex-frp.org/tutorial) and [my 
  playground][playground]
- [Great tutorials](https://qfpl.io/projects/reflex/) made by the Queensland 
  Functional Programming Lab
- Real World Reflex: a video addressing more advanced concepts
  - [video](https://www.youtube.com/watch?v=dNBUDAU9sv4)
  - [slides](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)

[^1]: Except for [tenjinreader](https://github.com/dfordivam/tenjinreader/) on 
    the [Play 
    Store](https://play.google.com/store/apps/details?id=com.blueimpact.tenjinreader)

[FRP]: https://en.wikipedia.org/wiki/Functional_reactive_programming
[HLS]: https://github.com/haskell/haskell-language-server
[Reflex]: https://reflex-frp.org/
[nixpkgs]: https://github.com/NixOS/nixpkgs
[obsidian]: https://obsidian.systems/
[playground]: https://github.com/jecaro/reflex-tutorial

