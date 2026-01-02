---

title: Htagcli - a new command line audio tagger
lastmod: 2026-01-02
toc: true

---

There we go. I just released a new command line audio tagger called htagcli. 
You can check it out on my [GitHub][htagcli].

# The Problem

As a longtime digital music enthusiast, I've spent decades curating my music 
library. Over the years, I've tried countless tools to keep it organized, 
ranging from Windows GUIs (forgive my younger self) to Linux command line 
utilities, and even web-based server apps.

My current workflow looks like this:

- Normalize audio files using [mp3gain], [aacgain], [vorbisgain], or 
  [metaflac], depending on the format.
- Use [beets] to scrape metadata, tag files, and organize them.
- Check collection consistency with [bliss].
- Edit tags as needed with various CLI tools, including [eyeD3], [id3], and 
  [tagutil].

Yes, it's complicated. I need to simplify this workflow.

# [htagcli]

Here is a short demo of [htagcli] in action:

![](/images/htagcli.png)

While [htagcli] doesn't yet streamline the entire process, it's a solid step 
toward taking ownership of my music management workflow.

Currently, [htagcli] can replace both a command line tagger and [bliss] to 
check the library consistency. I plan to add auto-tagging features similar to 
[beets], and I'm still considering how to handle normalization. We'll see how 
it evolves.

I like these well-scoped personal projects, small but non-trivial, and which 
solve a real problem for me. Looking at the git history, I realized I started 
this three years ago but didn't have the time to push it forward. I'm thrilled 
to finally have it in a very usable state.

# Roadmap

Here are the features I plan to implement soon:

- ~~Handle disc ID tags~~
- ~~Check that cover art size is within a configurable range~~
- ~~Ensure no missing tracks in an album~~
- ~~Verify genre consistency at the artist level~~[^1]

Another major goal is to scrape metadata from third-party sources like 
[MusicBrainz], [Discogs], and [Bandcamp] to automatically tag files correctly.

[^1]: All done in version [v0.1.1.0](https://github.com/jecaro/htagcli/releases/tag/v0.1.1.0)

[Bandcamp]: https://bandcamp.com/
[Discogs]: https://www.discogs.com/
[MusicBrainz]: https://musicbrainz.org/
[aacgain]: https://github.com/dgilman/aacgain
[beets]: https://beets.io/
[bliss]: https://www.blisshq.com/
[eyeD3]: https://eyed3.readthedocs.io/en/latest/
[htagcli]: https://github.com/jecaro/htagcli
[id3]: https://squell.github.io/id3/
[metaflac]: https://xiph.org/flac/documentation_tools_metaflac.html
[mp3gain]: https://mp3gain.sourceforge.net/
[tagutil]: https://github.com/kaworu/tagutil
[vorbisgain]: https://sjeng.org/vorbisgain.html

