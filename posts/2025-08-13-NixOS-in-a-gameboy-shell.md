---

title: NixOS in a gameboy shell
toc: true
lastmod: 2025-08-13

---

# Introduction

I built a retro gaming handheld based on a raspberry compute module 3 [sometime 
ago](/posts/2021-12-21-The-coolest-DIY-project-around.html). It was a super fun 
project. I often think my work as a developper misses a bit of manual work. 
Something that I love to do. This project was great for that reason as it 
involved a bit of soldering, 3D printing, hot gluing and precise dremmelling.

Anyway, after I built it, I installed [a custom Linux 
distribution](https://github.com/kiteretro/Circuit-Sword) based on 
[`retropie`](https://retropie.org.uk/) made by the author of the board. It's a 
bit rough in the edge. But it's been working fine so far, can't complain.

Unfortunately, after a couple of years, you know, software gets out of date and 
that distribution needed to be maintained. Seeing that the latest image for 
`retropie` itself is from [March 2022](https://retropie.org.uk/download/), what 
can you expect from a super niche distribution built on top of it? Not much, 
only a couple of forks [here](https://github.com/weese/Circuit-Sword) and 
[there](https://github.com/Antho91/Circuit-Sword), but nothing actively 
maintained.

I wanted to tackle this issue and bring the latest and greatest software to 
this handheld I built myself which has been sitting on my desk for too long. 
Additionally, this is one of the last piece of hardware I have not nixified 
yet.

So here am I, installing NixOS on a gameboy handheld. How cool is that?

![](/images/circuix-nixos.jpg){.center}

I named the project [`circuix-sword`](https://github.com/jecaro/circuix-sword) 
based on the name of the board: `Circuit-Sword`. If by chance you are one of 
the lucky few to have one of those, you can now run NixOS on it. It is very 
easy. Get started by downloading the latest image from the [releases 
page](https://github.com/jecaro/circuix-sword/releases/). Then burn it on an SD 
card, and you are ready to go. All the instructions are in the 
[`README.md`](https://github.com/jecaro/circuix-sword) of the repository.

While working on this project, I ran into countless issues, which is part of 
the reasons I wanted to do it in the first place. I will describe only a few of 
them here in chronological order.

# Boot the system

This one wasn't too hard, I just downloaded [the latest NixOS image for 
aarch64](https://hydra.nixos.org/job/nixos/release-24.11/nixos.sd_image.aarch64-linux), 
and it was able to boot on the CM3 out of the box. I just plugged a screen to 
the HDMI out, a keyboard to the USB port, and I was ready to go.

# WiFi

There is not much you can do without internet. I first reached out for a USB 
ethernet adapter I had around. It is good enough for hacking but not great for 
day to day use.

Hopefully, the `Circuit-Sword` has a WiFi chip on board. It is a `Realtek 
8723bs` but requires two things for it to work properly:

- The right firmware. This is solved using the option: 
  `hardware.enableRedistributableFirmware = true;`
  The closure induced by this option is quite big however (not less than 589 
  MiB). But we will address this issue later on.
- The [sdio 
  overlay](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/system/hardware.nix#L56). 
  This requires messing up with the device tree. NixOS doesn't provide a nice 
  solution for that yet. But I found [this 
  one](https://github.com/NixOS/nixpkgs/issues/320557#issuecomment-2176067772) 
  which works good enough for me.

# Screen, first attempt

Next, I needed to get the built-in screen working. The `Circuit-Sword` has a 
`320x240` DPI screen. Making it work was just a matter of copying the right 
lines from the original distribution in the [`config.txt` 
file](https://github.com/jecaro/circuix-sword/blob/f86544da642e517c09cadd091b6ff283de1e55dd/flake.nix#L63-L80).

While this worked fine for the console output, it does not for graphics. The 
drivers used in the original distribution have been deprecated to be replaced 
by the KMS DRM drivers.

# Screen, second attempt

Ok, now let's make those KMS DRM drivers work.

First, I needed to mess again with the device tree and enabled the 
`vc4-kms-v3d` overlay, which is the KMS driver. I also needed the 
`vc4-kms-dpi-generic` overlay to drive the DPI screen. That second overlay is 
especially tricky to get right as it needs [a lot of 
parameters](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/system/hardware.nix#L61-L63) 
to work properly. One can map the original parameters from the `config.txt` 
file to the new format. But the documentation is not very clear, and it took me 
a fair deal of tinkering to get it right.

But wait, this is not enough. Now the `SDL` library needs to support the KMS 
DRM driver too. I needed to [tweak 
slightly](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/overlays/SDL2.nix) 
the compilation of `SDL` to enable it.

At this point, I was able to run `retroarch` and start games, that was already 
a big achievement!

# Sound

The sound card is a `MicroII` chip. For some reason, it is stuck at a very high 
volume. The Linux module needs [to be 
patched](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/system/snd-usb-audio-modules/default.nix) 
for it to work properly.

# The `cs-hud` service

The `cs-hud` service is a custom service written in C especially for the board.

- It handles the safe-shutdown circuit
- When pressing the special mode button, it shows a help screen on top of the 
  framebuffer. This screen shows the keyboard shortcuts along the current 
  state: the levels of the volume, brightness and battery.

I integrated the source code in [the git 
repo](https://github.com/jecaro/circuix-sword/tree/99078fc328865c9a61cb82d9c3b3571151556375/overlays/cs-hud/src), 
created [the 
derivation](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/overlays/cs-hud/default.nix) 
to build it and wrote [the systemd 
service](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/system/configuration.nix#L74-L89) 
to run it. Standard NixOS stuff so far.

Unfortunately, with the new KMS setup, it is not possible to write an image on 
top of the framebuffer anymore. I then hacked the original code and removed 
pretty much everything related to that OSD feature. I kept the safe-shutdown 
handling, the brightness and volume control, and the battery level management.

On the way, I lost the ability to see all the info from that help screen. I 
can cope without it most of the time. But at some point I will definitely need 
to be able to see the battery level.

In the next section, I will explain how I solved that issue.

# `retroarch`

The original distribution runs `emulationstation`. At first, I tried to make it 
work in NixOS. But soon I realized that `emulationstation` is actually some 
kind of frontend on top of `retroarch`. To make things simpler, I decided to 
just use plain `retroarch` directly, at least for a start. That project is 
involved enough already.

I have made two quality of life improvement changes to `retroarch`:

- I fixed the `NetworkManager` WiFi driver. It is now possible to set up the 
  WiFi using the gamepad itself which is great as it wasn't possible in the 
  original distribution ([merged 
  upstream](https://github.com/libretro/RetroArch/pull/17857)).
- I exposed the battery state to a unix socket using `cs-hud` and made it 
  available in `retroarch`. This way, I can see the battery level in the 
  `retroarch` interface. I keep that change [in my 
  fork](https://github.com/jecaro/RetroArch/tree/circuix-sword) as it has no 
  chance to be merged upstream.

# Closure size optimization

At this point, I had a working system, but it was way bigger than it should be: 
1.81 GiB for the image of the version
[v0.0.2](https://github.com/jecaro/circuix-sword/releases/tag/v0.0.2). I 
started to look at the closure to see what I could remove. This is achieved by 
tweaking the compilation options of various packages using nix overlays.

The most obvious thing was everything related to the desktop, namely: `xorg`, 
`wayland`, `pulseaudio`, `pipewire` and so on. `gtk` was especially difficult 
to get rid of. `NetworkManager` pulls it in the `openconnect` plugin but 
removing all its plugins was not enough for some reason. The solution was to 
get rid of 
[`openconnect`](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/flake.nix#L91) 
itself.

The `mesa` library was pretty big as well (197 MiB). I trimmed it down to 20 
MiB.

The redistribuable firmware derivation is about 589 MiB for only one single 
firmware needed! I did [my own 
derivation](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/overlays/rtl8723-firmware.nix) 
for the firmware I need. The resulting derivation is now 41 KiB. That's much 
better!

The `retroarch` assets derivation is still huge (494 MiB) but I'm ok with that. 
I don't want to track down every single asset and check if it is needed or not. 

I wanted to get rid of `perl` as well. But it [didn't work out 
nicely](https://github.com/jecaro/circuix-sword/issues/7).

The final image is now around 800 MiB, under 1 GiB, which is not great but good 
enough.

# Firmware flash

The last piece of the puzzle is the gamepad firmware that runs on the arduino. 
I wanted [the 
source](https://github.com/jecaro/circuix-sword/tree/99078fc328865c9a61cb82d9c3b3571151556375/overlays/cs-firmware/CS_FIRMWARE) 
to be part of the repo as well. I also made it sure it compiles and [make it 
possible to 
flash](https://github.com/jecaro/circuix-sword/blob/99078fc328865c9a61cb82d9c3b3571151556375/flake.nix#L65-L66) 
it out of the box.

# Conclusion

This has been a difficult but fun journey. I love this project. Building the 
handheld was a great experience and porting the software to NixOS was also 
super fun.

There are so many different parts to it, going from very low to high level that 
makes it super interesting. From the Linux drivers that must be patched, to 
fine-tuning the booting process, the software that needs to be patched as well, 
some custom components, the gamepad firmware, not to mention the closure size 
optimization.

Really, there is something for everyone.

I have found out that NixOS is incredibly good at gluing all these pieces 
together. The fact that it is possible to patch every single package in one 
single repository is a huge win. That whole project took me some time. But the 
final amount of code is fairly reasonable. And I am very confident that I will 
be able to maintain it for a very long time.
