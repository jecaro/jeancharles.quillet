---

title: Adventures in podcast archiving
lastmod: 2026-08-11

---

I have maintained an audio library ever since mp3 was invented. This audio 
library is served at home by [Lyrion] running on a Raspberry Pi to many 
SqueezeBox radios around my house and also to my laptop thanks to 
[mprisqueeze].

This library is also periodically copied to my mp3 player. So I can listen to 
my music when I am on the go.

I never bothered putting podcasts on this library as a phone is basically the 
best setup for this activity. It automatically tracks new episodes and their 
listened status. However, my daughter has recently been listening to the radios and 
the mp3 player more and more. And as she was getting into podcasts, I started to 
get interested in adding them to my library.

How complicated could that be? Not much really, right? A podcast is served by an 
RSS feed which contains a bunch of URLs to audio files. All I have to do is 
schedule the download of those files and that'd be it.

First, I need a tool to download those audio files from the RSS feed. As I want 
to automate it, I need a CLI tool. There are not that many of them. I don't 
have hard requirements at this point. I just want something that works and 
doesn't get in the way. I chose [podcast-dl] which seemed to fit the bill.

For now, we will consider the podcast [Les Odyssées] which is a great podcast 
for kids.

Let's download a set of files for a start. The `--archive` flag makes [podcast-dl] 
record the list of episodes it has downloaded in a JSON file to make sure 
it won't download the same episode twice.

```
$ podcast-dl --archive --url "https://radiofrance-podcast.net/podcast09/podcast_c361798b-d6e3-4282-ba0a-ebb051b9e424.xml"
Les Odyssées
France Inter invite les enfants de 7 à 12 ans à se plonger dans les aventures des grandes figures de l'histoire.

Vous aimez ce podcast ? Pour écouter tous les épisodes sans limite, rendez-vous sur <a href="https://www.radiofrance.fr/franceinter/podcasts/les-odyssees?at_campaign=desc_podcast&at_medium=lien_RSS">Radio France</a>

/home/user/Music/Podcasts/Les Odyssées does not exist. Creating...

Starting download of 204 episodes

Camille du Gast, à toute allure vers la liberté !  | Starting download of 17.63 MB...
Camille du Gast, à toute allure vers la liberté !  | Download complete!
Alice au pays des merveilles | Starting download of 23.50 MB...
Alice au pays des merveilles | Download complete!
...
Successfully downloaded 204 episodes
```

Awesome, that works!

But looking at the list of episodes, I can see some of them are missing. I know 
this for sure as I did download some of them a few years ago. It looks like 
[RadioFrance] wants us to use their app where more episodes are available. 
That's what [Aerion](https://github.com/Aerion) found out and filled the 
gap by providing alternative feeds with his project 
[rss-radio-france-pour-tous].

Let's try again with the feed from [rss-radio-france-pour-tous]:

```
$ podcast-dl --archive --url "https://rss-rf.aerion.me/rss/c361798b-d6e3-4282-ba0a-ebb051b9e424"
Les Odyssées
France Inter invite les enfants de 7 à 12 ans à se plonger dans les aventures des grandes figures de l'histoire.

/home/user/Music/Podcasts/Les Odyssées does not exist. Creating...

Starting download of 100 episodes

L'Île au trésor  | Starting download of 23.65 MB...
L'Île au trésor  | Download complete!
Les Quatre Filles du docteur March  | Starting download of 20.97 MB...
Les Quatre Filles du docteur March  | Download complete!
...
Successfully downloaded 100 episodes
```

But wait, we were supposed to download more episodes, not fewer. How come? 

The feed available on this website uses pagination. See the details in [this 
issue](https://github.com/Aerion/rss-radio-france-pour-tous/issues/15). Fair 
enough, let's download page 1 as well.

```
$ podcast-dl --archive --url "https://rss-rf.aerion.me/rss/c361798b-d6e3-4282-ba0a-ebb051b9e424?page=1"
Les Odyssées
France Inter invite les enfants de 7 à 12 ans à se plonger dans les aventures des grandes figures de l'histoire.


Starting download of 95 episodes

Les 1001 nuits 1/4 : Shéhérazade conteuse de génie | Starting download of 22.81 MB...
Les 1001 nuits 1/4 : Shéhérazade conteuse de génie | Download complete!
Yasuké, le premier samouraï noir de l'Histoire | Starting download of 26.56 MB...
Yasuké, le premier samouraï noir de l'Histoire | Download complete!
...
Successfully downloaded 95 episodes
$ ls -l Les\ Odyssées/*.m4a | wc -l
195
```

195 episodes, that's still fewer than what we had with the official feed. It 
looks like the original feed contains a lot of duplicate episodes and those 
duplicate episodes are not in [rss-radio-france-pour-tous].

Ok, why not. Now we have all the episodes of [Les Odyssées]. Great. Let's see 
how it looks on [Lyrion].

![](/images/lyrion-1-bad-albums.png){.center}

Oh no, the tags in this single podcast are not consistent. We have no fewer 
than three cases: 

With the album: "Les odyssées"
![](/images/lyrion-1-les-odyssees.png){.center}

With the album: "[ATLAS] Les odyssées"
![](/images/lyrion-1-atlas-les-odyssees.png){.center}

And finally, without any tag at all!
![](/images/lyrion-1-no-tag.png){.center}

This is really bad. It'd be impossible to find anything with that kind of 
organisation.

[podcast-dl] is able to tag the audio files with the metadata present in the 
feed (using the swith `--embed-metadata`). Let's try that with the hope that it 
will lead to a more consistent tagging.

Downloading page 1, then page 0. Let's see how it looks again. 

![](/images/lyrion-2-duplicates.png){.center}

This time we have another issue: some episodes have the same track number. How 
come? That's because [podcast-dl] sets the track numbers according to the number 
of episodes in the feed. Because we process the two pages, it behaves as if there 
were two feeds and numbers the tracks according to their rank in the feed. 

No problem, let's remove those track numbers with my very own CLI audio tagger 
[htagcli] and because the files are prefixed by their publication date, they 
should appear in the right order.

```
$ htagcli set --notrack "./Les Odyssées"
```

Let's see this. Hopefully this time it should be Ok.

![](/images/lyrion-3-alphabetical.png){.center}

Wrong again, without a track number, Lyrion sorts the files in alphabetical 
order of title (after removing the articles "Les", "Le", "La"). Ok, I'll 
add [a new feature](https://github.com/jecaro/htagcli/pull/36) to `htagcli` 
to number the tracks according to their alphabetical order.

```
$ htagcli number-track "./Les Odyssées"
```

Ok, looking good now? No, we're not done yet. I can see that every now and then 
multiple episodes are published the same day. 

```
$ ls "Les Odyssées"
'20190625-Le grand voyage de Christophe Colomb _ 1492, la découverte de l’Amérique (1er épisode).m4a'
'20190625-Le grand voyage de Christophe Colomb _ 1492, la découverte de l’Amérique (2e épisode).m4a'
"20190625-Les aventuriers du grand froid _ l'expédition d'Ernest Shackleton.m4a"
"20190625-L'histoire des États-Unis pour les kids _ Calamity Jane, une histoire d’aventures dans les grands espaces du Far West.m4a"
"20190625-L'histoire des États-Unis pour les kids _ Martin Luther King, le combat d’un homme pour son rêve.m4a"
"20190625-L'histoire des États-Unis pour les kids _ Mission Apollo 11 _ l’histoire du premier homme à avoir marché sur la Lune.m4a"
'20190625-L’histoire du monstre du Loch Ness.m4a'
'20190625-Pauline Léon, une femme au cœur de la Révolution française.m4a'
'20190625-Puyi _ l’histoire incroyable du dernier empereur de Chine_.m4a'
'20190625-Toutânkhamon, ou la découverte d’un tombeau royal.m4a'
'20191015-Jane Goodall, une vie à observer les chimpanzés 1_2 _ Au cœur de la vie des chimpanzés.m4a'
'20191015-Jane Goodall, une vie à observer les chimpanzés 1_2 _ Une vie dédiée aux animaux.m4a'
"20191015-La découverte de l'épave du Titanic.m4a"
"20191015-La folle énigme de l'homme au masque de fer.m4a"
'20191015-La mystérieuse affaire Roswell .m4a'
'20191214-Deux pirates des Caraïbes _ Anne Bonny et Marie Read.m4a'
'20191214-La découverte de la grotte de Lascaux.m4a'
'20191214-La légende du roi Arthur épisode 1 _ La naissance d’un roi.m4a'
'20191214-La légende du roi Arthur épisode 2 _ le destin d’un roi.m4a'
'20191214-Une aventurière sur les routes de l’Himalaya _ Alexandra David-Néel.m4a'
...
```

How can we know for sure which episodes come before the others? We need to add 
the publication time in the filename, but [podcast-dl] doesn't support that. No 
problem, let's send [a new 
PR](https://github.com/lightpohl/podcast-dl/pull/161) to add support for this 
feature.

And now I can see that in that feed, there are also episodes published the same 
day at the very same hour. 

```
$ ls "Les Odyssées"
'20190625-153000-Le grand voyage de Christophe Colomb _ 1492, la découverte de l’Amérique (1er épisode).m4a'
'20190625-153000-Le grand voyage de Christophe Colomb _ 1492, la découverte de l’Amérique (2e épisode).m4a'
"20190625-153000-Les aventuriers du grand froid _ l'expédition d'Ernest Shackleton.m4a"
"20190625-153000-L'histoire des États-Unis pour les kids _ Calamity Jane, une histoire d’aventures dans les grands espaces du Far West.m4a"
"20190625-153000-L'histoire des États-Unis pour les kids _ Martin Luther King, le combat d’un homme pour son rêve.m4a"
"20190625-153000-L'histoire des États-Unis pour les kids _ Mission Apollo 11 _ l’histoire du premier homme à avoir marché sur la Lune.m4a"
'20190625-153000-L’histoire du monstre du Loch Ness.m4a'
'20190625-153000-Pauline Léon, une femme au cœur de la Révolution française.m4a'
'20190625-153000-Puyi _ l’histoire incroyable du dernier empereur de Chine_.m4a'
'20190625-153000-Toutânkhamon, ou la découverte d’un tombeau royal.m4a'
"20191015-092753-La folle énigme de l'homme au masque de fer.m4a"
"20191015-092756-La découverte de l'épave du Titanic.m4a"
'20191015-092758-Jane Goodall, une vie à observer les chimpanzés 1_2 _ Au cœur de la vie des chimpanzés.m4a'
'20191015-092801-Jane Goodall, une vie à observer les chimpanzés 1_2 _ Une vie dédiée aux animaux.m4a'
'20191015-092804-La mystérieuse affaire Roswell.m4a'
'20191214-111923-Deux pirates des Caraïbes _ Anne Bonny et Marie Read.m4a'
...
```

Never mind, at this point, there's nothing else I can do. Let's move on.

Looking more closely at the episodes, I can see there are a few series there. 
But wait, they are ordered in reverse order. What? How could this be that 
bad? This time I manually reordered the files before setting the track number...

But there is more. When trying to copy the files to my mp3 player, I was greeted 
with this error:

```
$ rsync --modify-window=1 --update --recursive --times --delete --exclude=.rockbox --exclude=bookmark.* --info=progress2 Music/ /var/run/media/user/9797-5FC2/
  1,091,635,844   1%    9.76MB/s    0:01:46 (xfr#178, ir-chk=1066/12597)rsync: 
  [generator] recv_generator: mkdir "/var/run/media/user/9797-5FC2/Podcasts/Une histoire et___ Oli " failed: Invalid argument (22)
*** Skipping any contents from this failed directory ***
```

That's because the filesystem format supported by mp3 players is usually vfat and yes, 
vfat doesn't support files ending with a space.

Ok, this should have been the easy part. I wasn't expecting to run into that 
many problems. Now all I have to do is schedule the download of the new 
episodes every night. Thankfully, that's super easy to do with NixOS.

But wait, [podcast-dl] is not on [nixpkgs]. No problem, let's just [add 
it](https://github.com/NixOS/nixpkgs/pull/550725).

And finally, here is the systemd unit with the scheduled task that downloads the 
new podcasts every night:

```nix
{ pkgs, lib, ... }:
let
  gatus = import ../../common/system/gatus.nix pkgs;
  # List of the podcasts to download
  podcasts = [
    {
      name = "Les Odyssées";
      albumartist = "Radio France";
      url = "https://rss-rf.aerion.me/rss/c361798b-d6e3-4282-ba0a-ebb051b9e424";
    }
    {
      name = "Bestioles";
      albumartist = "Radio France";
      url = "https://rss-rf.aerion.me/rss/a80ecbd5-df3d-4c9d-bee7-4e3d9efc1974";
    }
    {
      # The default name is "Une histoire et___ Oli ". But the vfat filesystem
      # on the mp3 player doesn't like the trailing spaces.
      name = "Une histoire et... Oli";
      albumartist = "Radio France";
      url = "https://rss-rf.aerion.me/rss/d555ed4e-dbe5-4908-912e-b3169f9ceede";
    }
  ];

  execStart = pkgs.writeShellScript "podcast-dl-all"
    # Handy for debugging when something goes wrong
    (''
      set -x
    '' +
    (lib.concatMapStringsSep "\n"
      (podcast: ''
        echo 'Download new episodes of "${podcast.name}"'
        ${lib.getExe pkgs.podcast-dl} \
          --embed-metadata \
          --archive "./${podcast.name}/archive.json" \
          --episode-template "{{release_date}}-{{release_time}}-{{title|trim}}" \
          --out-dir "./${podcast.name}/" \
          --url "${podcast.url}"

        # Set metadata for downloaded episodes
        ${lib.getExe pkgs.htagcli} set \
          --genre Podcasts \
          --albumartist "${podcast.albumartist}" \
          "./${podcast.name}/"

        # Update the track numbers
        ${lib.getExe pkgs.htagcli} number-tracks "./${podcast.name}/"
      '')
      podcasts));

  execStopPost = pkgs.writeShellScript "podcast-dl-notify-gatus"
    (gatus.cleanupCommand "podcast-dl");
in
{
  systemd.services.podcast-dl = {
    description = "Download podcasts";
    # podcast-dl relies on ffmpeg to embed metadata in the downloaded files
    path = [ pkgs.ffmpeg ];
    serviceConfig = {
      Type = "oneshot";
      User = "user";
      WorkingDirectory = "/home/user/Music/Podcasts";
      ExecStart = "${execStart}";
      ExecStopPost = "${execStopPost}";
    };
  };

  systemd.timers.podcast-dl = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };
}
```

We'll just ignore the [gatus] part for now. This post is too long already. But 
for your information it is the tool I use to monitor the scheduled task on my 
systems.

The interesting part is how Nix allows you to fold a data structure, here the 
list of podcasts, into a script you can easily plug into a systemd timer.

Thanks if you managed to read this far. This was quite a journey. I wasn't 
expecting to send no fewer than three pull requests just to have a simple 
reliable podcast downloader on my system. Now I understand why most people just 
prefer to use an app on their phone. That's way easier, sure. But while being 
annoying, none of this was specifically very difficult and it makes me very 
happy to still own the stack I use to listen to music.

[Les Odyssées]: https://www.radiofrance.fr/franceinter/podcasts/les-odyssees
[RadioFrance]: https://www.radiofrance.fr/
[gatus]: https://gatus.io
[htagcli]: https://github.com/jecaro/htagcli
[lyrion]: https://lyrion.org/
[mprisqueeze]: https://github.com/jecaro/mprisqueeze
[nixpkgs]: https://github.com/NixOS/nixpkgs
[podcast-dl]: https://github.com/lightpohl/podcast-dl
[rss-radio-france-pour-tous]: https://github.com/Aerion/rss-radio-france-pour-tous
