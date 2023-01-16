---

title: Basic nix VM for just anything

---

As easy it is to setup and run a light VM using nix, I think there is no 
shortage of simple and yet concrete examples of what you can do with it to get 
started. This is what I propose here today: to build up the most simple VM with 
nix for just any purpose. Note that this works on [NixOS] but also on any other 
host supporting [nixpkgs].

As for me, I mainly use this kind of VM for two reasons:

- to connect to my work VPN without messing up with the network configuration 
  of my computer
- to test software in isolation of my current system

The code is available [here](https://github.com/jecaro/simple-nix-vm). It uses 
flakes, so it should be fairly reproducible.

One can build the VM with:

```bash
$ nix build ./#nixosConfigurations.vm.config.system.build.vm
```

The build is usually pretty fast as the nix store is shared between the VM and 
the host.

It is then possible to run the VM with:
```bash
$ ./result/bin/run-nixos-vm
```

This starts a [qemu](https://www.qemu.org/) window with the [xfce] desktop 
environment. [xfce] is lightweight enough to make it able to run even on 
computers with limited amount of CPU power.

Nix will create a file for the disk of the VM in the current directory. It is 
called `nixos.qcow2`. Thanks to this file, if you put data into the home 
directory of the VM user, it will be persistent between reboots.

Nothing fancy here, just plain simple nix code, but as usual with nix still 
very hard to guess if you don't have any example around.

Let's go through interesting parts of the code. The VM resolution can be 
changed here for something suitable for your screen:

<a name="qemu-options"/>
```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=12
to=28
language=nix}
```

The user configuration now. Being part of the wheel group makes it possible to 
use sudo if needed. You might want to change the password here.

```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=30
to=38
language=nix}
```

The internationalization options:

```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=8
to=10
language=nix}
```

The xserver options:

```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=40
to=51
language=nix}
```

That last line along the [qemu options](#qemu-options) is especially important. 
This allows to copy/paste between the host computer and the VM. Not having this 
makes the experience incredibly frustrating. More details about this is 
available [here](https://www.kraxel.org/blog/2021/05/qemu-cut-paste/).

One can tweak the packages available:

```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=56
to=66
language=nix}
```

Checkout the [huge list](https://search.nixos.org/packages) of available 
packages in nixpkgs for inspiration.

You also might want to ssh to the VM from the host, so just enable the ssh 
server on the VM:

```{.get
url=https://raw.githubusercontent.com/jecaro/simple-nix-vm/master/vm.nix
from=53
to=54
language=nix}
```

To connect to the VM, one must forward the 22 port to the host. That can be 
done with an environment variable before starting the VM:

```bash
$ QEMU_NET_OPTS="hostfwd=tcp::2222-:22" ./result/bin/run-nixos-vm
```

And that's it for today ! Give it a try and let me know if you have any 
question.

[xfce]: https://xfce.org/
[NixOS]: https://nixos.org/
[nixpkgs]: https://github.com/NixOS/nixpkgs
